libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.13" % "test"
)

//scalaVersion := "2.10.2"

ScalariformSupport.formatSettings

Revolver.settings

scalaVersion := "2.10.3-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

scalaOrganization := "org.scala-lang.macro-paradise"

libraryDependencies <++= scalaVersion ( version =>
  Seq("scala-library", "scala-reflect").map (
    "org.scala-lang.macro-paradise" % _ % version % "provided"
  )
)

libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-library" % _ % "provided" )

autoScalaLibrary := false

scalaVersion in Test := "2.10.2"

scalaOrganization in Test := "org.scala-lang"

compile in Test <<= (compile in Test).dependsOn(packageBin in Compile)

scalaInstance in Test <<= (appConfiguration, scalaOrganization in Test, scalaVersion in Test, scalaHome in Test) map { (app, org, version, home) =>
  val launcher = app.provider.scalaProvider.launcher
  home match {
    case None => ScalaInstance(org, version, launcher)
    case Some(h) => ScalaInstance(h, launcher)
  }
}
