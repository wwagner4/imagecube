import org.scalatra.sbt._

val ScalatraVersion = "2.5.0"

lazy val commonSettings = Seq(
    organization := "net.entelijan",
    version := "1.0",
    scalaVersion := "2.12.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val root = project.in(file("."))
  .aggregate(core, web)
  .settings(
    commonSettings,
    name := "imagecube"
  )

lazy val core = project.in(file("core"))
  .settings(
    commonSettings,
    name := "imagecube-core"
  )

lazy val web = project.in(file("web"))
  .dependsOn(core)
  .enablePlugins(JettyPlugin)
  .settings(
    commonSettings,
    name := "imagecube-web",
    ScalatraPlugin.scalatraSettings,
    scalateSettings,
    containerPort in Jetty := 8090,
    resolvers += Classpaths.typesafeReleases,
    assemblyJarName in assembly := "imagecube-web.jar",
    mainClass in assembly := Some("web.imagecube.JettyLauncher"),
    libraryDependencies += "org.scalatra" %% "scalatra" % ScalatraVersion,
    libraryDependencies += "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
    libraryDependencies += "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
    libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.4.5.v20170502",
    libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.24",
      libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
  )
    
    
    
    
// scalacOptions += "-deprecation",
