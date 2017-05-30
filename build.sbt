name := "palindromes"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

mainClass in (Compile, run) := Some("boczula.mateusz.palindromes.Palindromes")