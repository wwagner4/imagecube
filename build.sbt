import org.scalatra.sbt._
import org.scalatra.sbt.PluginKeys._
import ScalateKeys._

val ScalatraVersion = "2.5.0"

lazy val commonSettings = Seq(
    organization := "net.entelijan",
    version := "1.0",
    scalaVersion := "2.12.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.24"
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
  .enablePlugins(JettyPlugin)
  .settings(
    commonSettings,
    name := "imagecube-web",
    ScalatraPlugin.scalatraSettings,
    scalateSettings,
    resolvers += Classpaths.typesafeReleases,
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.24",
    libraryDependencies += "org.scalatra" %% "scalatra" % ScalatraVersion,
    libraryDependencies += "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
    libraryDependencies += "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.5" % "runtime",
    libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.2.15.v20160210" % "container",
    libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided"
  )
    
    
    
    
// scalacOptions += "-deprecation",
