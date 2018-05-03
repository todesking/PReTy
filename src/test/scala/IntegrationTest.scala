package com.todesking.prety.integration_test

import com.todesking.prety.scalac_plugin.PretyPlugin

import org.scalatest.FunSpec
import scala.reflect.io.AbstractFile

import com.todesking.prety.util.uniqueMap

class IntegrationTest extends FunSpec {
  init()

  case class LocalPos(line: Int, col: Int) {
    def tuple: (Int, Int) = (line, col)
    override def toString = s"$line:$col"
  }
  object LocalPos {
    def apply(p: scala.reflect.internal.util.Position): LocalPos =
      LocalPos(p.focus.line, p.focus.column)
    implicit val ord: Ordering[LocalPos] = new Ordering[LocalPos] {
      override def compare(l: LocalPos, r: LocalPos) =
        implicitly[Ordering[(Int, Int)]].compare(l.tuple, r.tuple)
    }
  }

  private[this] def init(): Unit = {
    val baseDir = new java.io.File(getClass.getResource("/integration-test").toURI).getPath
    println(s"baseDir=$baseDir")
    val base = AbstractFile.getDirectory(baseDir)
    assert(base != null)
    registerTests(base)
  }

  private[this] def registerTests(dir: AbstractFile): Unit = {
    require(dir.isDirectory, s"Not directory: $dir")
    dir.foreach { f =>
      if (f.isDirectory) {
        describe(f.name) {
          registerTests(f)
        }
      } else if (f.name.endsWith(".scala")) {
        it(f.name) {
          assertCompile(f)
        }
      } else {
        throw new RuntimeException(s"Unknown file found: ${f.path}")
      }
    }
  }

  private[this] def assertCompile(f: AbstractFile): Unit = {
    val content = new String(f.toByteArray)
    if (content.startsWith("// pending\n")) {
      pending
      return
    }
    val debug = content.startsWith("// debugPrint")

    val markerPat = """\s*//\s*\^\s*(.*)""".r
    val expectedErrors: Map[LocalPos, String] = uniqueMap(
      content.split("\n").zipWithIndex.collect {
        case (l @ markerPat(msg), lnum) =>
          val col = l.indexOf("^")
          assert(col >= 0)
          LocalPos(lnum + 1 - 1, col + 1) -> msg.replaceAll("\\\\n", "\n")
      })

    val result = Compiler.compile(f.path, debug)
    assert(result.infos == Seq())
    assert(result.warnings == Seq())

    val errors = result.errors.map { e => LocalPos(e.pos) -> e.message }.groupBy(_._1).mapValues(_.map(_._2).mkString(", "))

    val happendErrors = errors.filter {
      case (k, v) =>
        expectedErrors.get(k).filter { pat => pat == "" || v == pat }.nonEmpty
    }
    val unexpectedErrors = errors.filter {
      case (k, v) =>
        expectedErrors.get(k).filter { pat => pat == "" || v == pat }.isEmpty
    }
    val nothappenedErrors = expectedErrors.filter {
      case (k, pat) =>
        errors.get(k).filter { v => pat == "" || v == pat }.isEmpty
    }

    if (debug) {
      happendErrors.foreach {
        case (pos, msg) =>
          println(s"Expected Error: $pos, $msg")
      }
      nothappenedErrors.foreach {
        case (pos, msg) =>
          println(s"Error expected but not happend: $pos, $msg")
      }
      unexpectedErrors.foreach {
        case (pos, msg) =>
          println(s"Unexpected Error: $pos, $msg")
      }
    }

    assert(unexpectedErrors.isEmpty)
    assert(nothappenedErrors.isEmpty)
  }
}

object Compiler {
  // ref: https://stackoverflow.com/questions/4713031/how-to-use-scalatest-to-develop-a-compiler-plugin-in-scala/4937135#4937135
  // ref: https://stackoverflow.com/questions/4713031/how-to-use-scalatest-to-develop-a-compiler-plugin-in-scala/11710337#11710337
  import scala.tools.nsc.{ Settings, Global }
  import scala.tools.nsc.io.VirtualDirectory
  import scala.tools.nsc.util.ClassPath
  import scala.tools.nsc.reporters.StoreReporter

  import scala.reflect.internal.util.BatchSourceFile
  import scala.reflect.internal.util.Position

  case class Message(pos: Position, message: String)
  case class Result(infos: Seq[Message], warnings: Seq[Message], errors: Seq[Message])

  def compile(path: String, debug: Boolean): Result = {
    val file = AbstractFile.getFile(path)
    val sources = List(new BatchSourceFile(file))

    val settings = new Settings
    // save class files to a virtual directory in memory
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
    fixClasspath(settings)

    val reporter = new StoreReporter

    val compiler = new Global(settings, reporter) {
      override protected def loadRoughPluginsList =
        new PretyPlugin(this, debug) :: super.loadRoughPluginsList
    }

    new compiler.Run().compileSources(sources)

    def toMessage(i: reporter.Info) = Message(i.pos, i.msg)
    new Result(
      reporter.infos.filter(_.severity.id == reporter.INFO.id).toSeq.map(toMessage),
      reporter.infos.filter(_.severity.id == reporter.WARNING.id).toSeq.map(toMessage),
      reporter.infos.filter(_.severity.id == reporter.ERROR.id).toSeq.map(toMessage))
  }

  private[this] def fixClasspath(settings: Settings): Unit = {
    val loader = getClass.getClassLoader.asInstanceOf[java.net.URLClassLoader]
    val entries = loader.getURLs map (_.getPath)
    // annoyingly, the Scala library is not in our classpath, so we have to add it manually
    val sclpath = entries find (_.endsWith("scala-compiler.jar")) map (
      _.replaceAll("scala-compiler.jar", "scala-library.jar"))
    settings.classpath.value = ClassPath.join((entries ++ sclpath): _*)
  }

}
