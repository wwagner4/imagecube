lazy val root = (project in file("."))
  .settings(
    name := "imagecube",
    version := "1.0",
    scalaVersion := "2.12.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.24"
  )

// scalacOptions += "-deprecation",
