import sbt._
import Keys._

object ScuttleBuild extends Build {
    lazy val root = Project(
        id = "root",
        base = file("."),
	settings = Project.defaultSettings ++ Seq(
		publish := false,
		publishLocal := false),
	aggregate = Seq(scuttle, scuttleTwitterConvert, scuttleJodaConvert))  

    lazy val scuttle = Project(
        id = "scuttle",
        base = file("scuttle"))

    lazy val scuttleTwitterConvert = Project(
        id = "scuttle-twitter-convert",
        base = file("scuttle-twitter-convert")) dependsOn(scuttle)

    lazy val scuttleJodaConvert = Project(
        id = "scuttle-joda-convert",
        base = file("scuttle-joda-convert")) dependsOn(scuttle)
}
