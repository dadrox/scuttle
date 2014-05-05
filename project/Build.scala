import sbt._
import Keys._

object Settings {
  val testFramework = new TestFramework("com.dadrox.sbt.junit.JunitFramework")

  val commonSettings = {
    Project.defaultSettings ++ Seq(
      scalaVersion in ThisBuild := "2.10.2",
      testFrameworks := Seq(testFramework),
      testListeners <+= target map (t => new com.dadrox.sbt.test.reports.Xml(t getName)),
      libraryDependencies in ThisBuild ++= Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          "com.github.lalyos" % "jfiglet" % "0.0.2",
          "junit" % "junit" % "4.11" % "test->default",
          "org.fictus" %% "fictus" % "0.9.3" % "test",
          "com.dadrox" % "sbt-junit" % "0.3.1" % "test")
    )
  }
}

object ScuttleBuild extends Build {

  object V {
    val Joda = "2.1"
    val Olson = "2013c"
    val TwitterUtil = "6.3.8"
  }

  val DisablePublish = Seq(publish := (), publishLocal := ())

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Settings.commonSettings ++ DisablePublish,
    aggregate = Seq(scuttle, scuttleMacros, scuttleTwitterConvert, scuttleJodaConvert, scuttleTimeZone))

  lazy val scuttle = Project(
    id = "scuttle",
    base = file("scuttle"),
    settings = Settings.commonSettings ++ Seq(
    )) dependsOn(scuttleMacros)

  lazy val scuttleMacros = Project(
    id = "scuttle-macros",
    base = file("scuttle-macros"),
    settings = Settings.commonSettings ++ Seq(
    ))

  lazy val scuttleTwitterConvert = Project(
    id = "scuttle-twitter-convert",
    base = file("scuttle-twitter-convert"),
    settings = Settings.commonSettings ++ Seq(
      libraryDependencies ++= Seq(
        "com.twitter" %% "util-core" % V.TwitterUtil % "provided")
    )) dependsOn(scuttle)

  lazy val scuttleJodaConvert = Project(
    id = "scuttle-joda-convert",
    base = file("scuttle-joda-convert"),
    settings = Settings.commonSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.joda" % "joda-convert" % "1.2" % "provided",
        "joda-time" % "joda-time" % V.Joda % "provided")
    )) dependsOn(scuttle)

  lazy val scuttleTimeZone = Project(
    id = "scuttle-tz",
    base = file("scuttle-tz"),
    settings = Settings.commonSettings ++ DisablePublish) dependsOn(scuttle)
}
