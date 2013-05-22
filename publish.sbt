publishMavenStyle in ThisBuild := true

publishArtifact in ThisBuild in Test := false

pomIncludeRepository in ThisBuild := { _ => false }

publishTo in ThisBuild <<= (version) { version: String =>
    val sonatype = "https://oss.sonatype.org/"
    if (version.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at sonatype + "content/repositories/snapshots")
    else
        Some("releases"  at sonatype + "service/local/staging/deploy/maven2")
}

credentials in ThisBuild += Credentials(Path.userHome / ".credentials" / "sonatype")

pomExtra in ThisBuild := PomExtra(
    url = "https://github.com/dadrox/scuttle",
    scm = Scm("https://github.com/dadrox/scuttle", "scm:git:git@github.com:dadrox/scuttle.git"),
    license = License("BSD 2-Clause License", "http://opensource.org/licenses/BSD-2-Clause"),
    developers = List(
        Developer("dadrox", "Christopher Wood", "https://github.com/dadrox")))
