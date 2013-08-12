import sbt._
import Keys._

object Data {
  val majorVersion = "0.4"
  val snapshot = false
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
      crossScalaVersions in ThisBuild := Seq("2.9.1", "2.9.2", "2.9.3", "2.10.0", "2.10.1"),
      scalaVersion in ThisBuild := "2.9.2",
      packageOptions := Seq(Package.ManifestAttributes("Implementation-Version" -> fullVersion)),
      testFrameworks := Seq(testFramework),
      testListeners <+= target map (t => new com.dadrox.sbt.test.reports.Xml(t getName)),
      libraryDependencies in ThisBuild ++= Seq(
          "junit" % "junit-dep" % "4.10" % "test->default",
          "org.fictus" %% "fictus" % "0.6" % "test",
          "com.dadrox" % "sbt-junit" % "0.3" % "test")
    )
  }
}

object ScuttleBuild extends Build {

  object V {
    val Joda = "2.1"
    val Olson = "2013c"
    val TwitterUtil = "5.2.0"
  }

    lazy val root = Project(
        id = "root",
        base = file("."),
	settings = Settings.commonSettings() ++ Seq(
          publish := false,
          publishLocal := false),
	aggregate = Seq(scuttle, scuttleTwitterConvert, scuttleJodaConvert, scuttleTimeZone))

    lazy val scuttle = Project(
        id = "scuttle",
        base = file("scuttle"),
	settings = Settings.commonSettings() ++ Seq(
          name := "scuttle"
        ))

    lazy val scuttleTwitterConvert = Project(
        id = "scuttle-twitter-convert",
        base = file("scuttle-twitter-convert"),
        settings = Settings.commonSettings(Some(V.TwitterUtil)) ++ Seq(
          name := "scuttle-twitter-convert",
          libraryDependencies ++= Seq(
            "com.twitter" % "util-core" % V.TwitterUtil)
        )) dependsOn(scuttle)

    lazy val scuttleJodaConvert = Project(
        id = "scuttle-joda-convert",
        base = file("scuttle-joda-convert"),
        settings = Settings.commonSettings(Some(V.Joda)) ++ Seq(
          name := "scuttle-joda-convert",
          libraryDependencies ++= Seq(
            "org.joda" % "joda-convert" % "1.2" % "provided",
            "joda-time" % "joda-time" % V.Joda % "provided")
        )) dependsOn(scuttle)

    lazy val scuttleTimeZone = Project(
        id = "scuttle-tz",
        base = file("scuttle-tz"),
        settings = Settings.commonSettings(Some(V.Olson)) ++ Seq(
          name := "scuttle-tz"
        )) dependsOn(scuttle)
}
