val snapshot_? = true

val baseVersion = "1.1"

organization in ThisBuild := "com.dadrox"

version in ThisBuild := s"$baseVersion${if(snapshot_?) "-SNAPSHOT" else ""}"
