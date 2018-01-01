package com.todesking.prety.integration_test

import org.scalatest.FunSpec
import scala.reflect.io.AbstractFile

class IntegrationTest extends FunSpec {
  init()

  private[this] def init(): Unit = {
    val baseDirKey = "prety.testing.integration.dir"
    val baseDir = sys.props.get(baseDirKey).filter(_.nonEmpty).getOrElse {
      throw new RuntimeException(s"System property $baseDirKey is not set")
    }
    println(s"Integration test: baseDir=${baseDir}")
    val root = AbstractFile.getDirectory(baseDir)
    require(root != null)

    registerTests(root)
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
    assert(result.infos.isEmpty)
    assert(result.warnings.isEmpty)
    assert(result.errors.size == expectedErrors.size)
    result.errors.sortBy(_.pos.point).zip(expectedErrors.sortBy(_._1)).foreach {
      case (amsg, (epos, emsg)) =>
        assert(amsg.pos.point == epos)
        assert(amsg.message == emsg)
    }
  }
}

object Compiler {
  // ref: https://stackoverflow.com/questions/4713031/how-to-use-scalatest-to-develop-a-compiler-plugin-in-scala/4937135#4937135
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
      // override protected def loadRoughPluginsList: List[Plugin] =
      //   new DivByZero(this) :: super.loadRoughPluginsList
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
