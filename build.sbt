scalaVersion := "2.12.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.sosy-lab" % "java-smt" % "1.0.1"

resolvers += "uuverifiers" at "http://logicrunch.it.uu.se:4096/~wv/maven/"
libraryDependencies += "uuverifiers" %% "princess" % "2018-01-27"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfuture",
  "-Xlint"
  /* "-Yno-adapted-args", */
  /* "-Ywarn-dead-code", */
  /* "-Ywarn-unused-import", */
  /* "-Ywarn-value-discard" */
)
scalacOptions in (Compile, console) ~= {_.filterNot(_ == "-Ywarn-unused-import")}
scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value

/* testOptions in Test += Tests.Argument("-oDF") */

