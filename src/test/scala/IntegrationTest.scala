package com.todesking.prety.integration_test

import com.todesking.prety.scalac_plugin.PretyPlugin

import org.scalatest.FunSpec
import scala.reflect.io.AbstractFile

class IntegrationTest extends FunSpec {
  init()

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
    val (_, linesWithOffset) = content.split("\n")
      .foldLeft(0 -> Seq.empty[(Int, String)]) {
        case ((off, ls), line) =>
          (off + line.length + 1) -> (ls :+ (off -> line))
      }
    val markerPat = """\s*//\s*\^\s*(.*)""".r
    val expectedErrors: Seq[(Int, String)] = linesWithOffset.zip(linesWithOffset.drop(1)).collect {
      case ((off, l0), (_, l1 @ markerPat(msg))) =>
        val posInLine = l1.indexOf("^")
        assert(posInLine >= 0)
        (off + posInLine) -> msg.replaceAll("\\\\n", "\n")
    }

    val result = Compiler.compile(f.path)
    assert(result.infos == Seq())
    assert(result.warnings == Seq())

    val eErrors = expectedErrors.toSet
    val errors = result.errors.map { e => (if (e.pos.isDefined) e.pos.point else -1) -> e.message }.toSet

    val nothappens = (eErrors -- errors).toSeq.sortBy(_._1)
    val unexpecteds = (errors -- eErrors).toSeq.sortBy(_._1)

    nothappens.foreach {
      case (pos, msg) =>
        println(s"Error expected but not happens: $pos, $msg")
    }
    unexpecteds.foreach {
      case (pos, msg) =>
        println(s"Unexpected Error: $pos, $msg")
    }

    assert(errors == expectedErrors)

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

  def compile(path: String): Result = {
    val file = AbstractFile.getFile(path)
    val sources = List(new BatchSourceFile(file))

    val settings = new Settings
    // save class files to a virtual directory in memory
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
    fixClasspath(settings)

    val reporter = new StoreReporter

    val compiler = new Global(settings, reporter) {
      override protected def loadRoughPluginsList =
        new PretyPlugin(this) :: super.loadRoughPluginsList
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
