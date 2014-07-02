import sbt._
import Keys._

object SpeedBuild extends Build {
  def commonSettings = Seq(
    crossScalaVersions := Seq("2.10.4", "2.11.1"),
    scalacOptions ++= Seq(
      "-unchecked", "-language:_"
    ),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
      "org.specs2" %% "specs2" % "2.3.12" % "test"
    ) ++ (
      if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % "2.0.0")
      else Nil
      ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
  ) ++ ScalariformSupport.formatSettings

  lazy val root =
    Project("root", file("."))
      .aggregate(speed, speedTests)

  lazy val speed =
    Project("speed", file("speed"))
      .settings(commonSettings: _*)

  lazy val speedTests =
    Project("speed-tests", file("speed-tests"))
      .dependsOn(speed)
      .settings(commonSettings: _*)
      .settings(
        unmanagedBase in Test <<= baseDirectory / "test-lib",
        // make sure to recompile tests every time
        cleanFiles in Test <<= Seq(classDirectory in Test).join,
        cleanKeepFiles in Test := Nil,
        clean in Test <<= (cleanFiles in Test, cleanKeepFiles in Test) map Defaults.doClean,
        compile in Test <<= (compile in Test).dependsOn(clean in Test)
      )
}