scalaVersion := "2.12.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

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
