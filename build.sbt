import org.scalatra.sbt._

val ScalatraVersion = "2.6.3"

lazy val commonSettings = Seq(
  organization := "net.entelijan",
  version := "1.0",
  scalaVersion := "2.12.6",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
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
    name := "imagecube-core",
    assemblyJarName in assembly := "imagecube.jar",
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
  )

lazy val web = project.in(file("web"))
  .dependsOn(core)
  .enablePlugins(JettyPlugin)
  .settings(
    commonSettings,
    name := "imagecube-web",
    ScalatraPlugin.scalatraSettings,
    containerPort in Jetty := 8090,
    resolvers += Classpaths.typesafeReleases,
    assemblyJarName in assembly := "imagecube-web.jar",
    mainClass in assembly := Some("web.imagecube.JettyLauncher"),
    libraryDependencies += "org.scalatra" %% "scalatra" % ScalatraVersion,
    libraryDependencies += "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
    libraryDependencies += "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
    libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.4.12.RC2",
    libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25"
  )




// scalacOptions += "-deprecation",
