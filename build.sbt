organization in ThisBuild := "com.dadrox"

version in ThisBuild := "0.4-SNAPSHOT"

libraryDependencies in ThisBuild ++= Seq(
	"junit" % "junit-dep" % "4.10" % "test->default",
	"org.fictus" %% "fictus" % "0.6" % "test",
	"com.dadrox" % "sbt-junit" % "0.3" % "test")

testFrameworks in ThisBuild += new TestFramework("com.dadrox.sbt.junit.JunitFramework")

crossScalaVersions in ThisBuild := Seq("2.9.1", "2.9.2", "2.9.3", "2.10.0", "2.10.1")

scalaVersion in ThisBuild := "2.9.2"
