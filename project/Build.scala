import sbt._
import Keys._

object Data {
  val majorVersion = "0.7"
  val snapshot = true
  val org = "com.dadrox"
}

trait Version {
  import Data._
  def currentVersion(versionExtra: Option[String] = None) = majorVersion + (versionExtra match {
    case Some(extra) => "-"+extra
    case None =>        ""
  }) + (if(snapshot) "-SNAPSHOT" else "")
}

object Settings extends Version {
  val testFramework = new TestFramework("com.dadrox.sbt.junit.JunitFramework")

  def commonSettings(versionExtra: Option[String] = None) = {
    val fullVersion = currentVersion(versionExtra)
    Project.defaultSettings ++ Seq(
      organization := Data.org,
      version := fullVersion,
      crossScalaVersions in ThisBuild := Seq("2.9.2", "2.10.2"),
      scalaVersion in ThisBuild := "2.10.2",
      packageOptions := Seq(Package.ManifestAttributes("Implementation-Version" -> fullVersion)),
      testFrameworks := Seq(testFramework),
      testListeners <+= target map (t => new com.dadrox.sbt.test.reports.Xml(t getName)),
      libraryDependencies in ThisBuild ++= Seq(
          "junit" % "junit-dep" % "4.11" % "test->default",
          "org.fictus" %% "fictus" % "0.8" % "test",
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

  val DisablePublish = Seq(publish := false, publishLocal := false)

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Settings.commonSettings() ++ DisablePublish,
    aggregate = Seq(scuttle, scuttleTwitterConvert, scuttleJodaConvert, scuttleTimeZone))

  lazy val scuttle = Project(
    id = "scuttle",
    base = file("scuttle"),
    settings = Settings.commonSettings() ++ Seq(
    ))

  lazy val scuttleTwitterConvert = Project(
    id = "scuttle-twitter-convert",
    base = file("scuttle-twitter-convert"),
    settings = Settings.commonSettings(Some(V.TwitterUtil)) ++ Seq(
      libraryDependencies ++= Seq(
        "com.twitter" %% "util-core" % V.TwitterUtil)
    )) dependsOn(scuttle)

  lazy val scuttleJodaConvert = Project(
    id = "scuttle-joda-convert",
    base = file("scuttle-joda-convert"),
    settings = Settings.commonSettings(Some(V.Joda)) ++ Seq(
      libraryDependencies ++= Seq(
        "org.joda" % "joda-convert" % "1.2" % "provided",
        "joda-time" % "joda-time" % V.Joda % "provided")
    )) dependsOn(scuttle)

  lazy val scuttleTimeZone = Project(
    id = "scuttle-tz",
    base = file("scuttle-tz"),
    settings = Settings.commonSettings(Some(V.Olson)) ++ DisablePublish) dependsOn(scuttle)
}
