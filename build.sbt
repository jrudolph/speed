scalaVersion := "2.10.4"

scalacOptions ++= Seq(
  "-unchecked", "-language:_"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.specs2" %% "specs2" % "2.3.12" % "test"
) ++ (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % "2.0.0")
  else Nil
)

ScalariformSupport.formatSettings

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

// make sure to recompile tests every time
cleanFiles in Test <<= Seq(classDirectory in Test).join

cleanKeepFiles in Test := Nil

clean in Test <<= (cleanFiles in Test, cleanKeepFiles in Test) map Defaults.doClean

compile in Test <<= (compile in Test).dependsOn(clean in Test)

unmanagedBase in Test <<= baseDirectory / "test-lib"
