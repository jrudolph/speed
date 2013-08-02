libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.13" % "test"
)

ScalariformSupport.formatSettings

// this doesn't help alone, you need the extra repo in sbt.boot.properties
resolvers += Resolver.sonatypeRepo("snapshots")

// use macro-paradise
scalaOrganization := "org.scala-lang.macro-paradise"

scalaVersion := "2.10.3-SNAPSHOT"

// we disable auto scala-library
autoScalaLibrary := false

// and add it here manually but with provided scope
// (i.e. practically ignored for dependents)
libraryDependencies <++= scalaVersion ( version =>
  Seq("scala-library", "scala-reflect").map (
    "org.scala-lang.macro-paradise" % _ % version % "provided"
  )
)

// we want another scala version in the tests
scalaVersion in Test := "2.10.2"

scalaOrganization in Test := "org.scala-lang"

scalaInstance in Test <<= (appConfiguration, scalaOrganization in Test, scalaVersion in Test, scalaHome in Test) map { (app, org, version, home) =>
  val launcher = app.provider.scalaProvider.launcher
  home match {
    case None => ScalaInstance(org, version, launcher)
    case Some(h) => ScalaInstance(h, launcher)
  }
}

// make sure to recompile tests every time
cleanFiles in Test <<= Seq(classDirectory in Test).join

cleanKeepFiles in Test := Nil

clean in Test <<= (cleanFiles in Test, cleanKeepFiles in Test) map Defaults.doClean

compile in Test <<= (compile in Test).dependsOn(clean in Test)

scalacOptions ++= Seq(
  "-unchecked", "-language:_"
)

unmanagedBase in Test <<= baseDirectory / "test-lib"
