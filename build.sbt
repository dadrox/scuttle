name := "scuttle"

organization := "com.dadrox"

version := "0.2"

libraryDependencies ++= Seq(
	"junit" % "junit-dep" % "4.10" % "test->default",
	"org.fictus" %% "fictus" % "0.6" % "test",
	"com.dadrox" % "sbt-junit" % "0.3" % "test")

testFrameworks += new TestFramework("com.dadrox.sbt.junit.JunitFramework")

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.9.3", "2.10.0", "2.10.1")

scalaVersion := "2.9.2"
