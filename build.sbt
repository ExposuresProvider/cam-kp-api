enablePlugins(JavaAppPackaging)

organization := "org.renci"

name := "cam-kp-api"

version := "0.1"

licenses := Seq("MIT license" -> url("https://opensource.org/licenses/MIT"))

scalaVersion := "2.12.11"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ypartial-unification")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

val zioVersion = "1.0.0-RC21-2"
val zioConfigVersion = "1.0.0-RC24"
val tapirVersion = "0.16.7"
val http4sVersion = "0.21.6"
val circeVersion = "0.13.0"
val logbackVersion = "1.2.3"

libraryDependencies ++= {
  Seq(
    "dev.zio"                     %% "zio"                      % zioVersion,
    "dev.zio"                     %% "zio-interop-cats"         % "2.1.3.0-RC16",
    "dev.zio"                     %% "zio-config"               % zioConfigVersion,
    "dev.zio"                     %% "zio-config-magnolia"      % zioConfigVersion,
    "dev.zio"                     %% "zio-config-typesafe"      % zioConfigVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-core"               % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-zio"                % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-zio-http4s-server"  % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-json-circe"         % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"       % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-http4s"  % tapirVersion,
    "org.http4s"                  %% "http4s-blaze-server"      % http4sVersion,
    "org.http4s"                  %% "http4s-dsl"               % http4sVersion,
    "org.http4s"                  %% "http4s-blaze-client"      % http4sVersion,
    "org.apache.jena"              % "apache-jena-libs"         % "3.16.0",
    "org.phenoscape"              %% "sparql-utils"             % "1.2",
    "io.circe"                    %% "circe-core"               % circeVersion,
    "io.circe"                    %% "circe-generic"            % circeVersion,
    "io.circe"                    %% "circe-parser"             % circeVersion,
    "dev.zio"                     %% "zio-test"                 % zioVersion % Test,
    "dev.zio"                     %% "zio-test-sbt"             % zioVersion % Test,
    "org.apache.commons"           % "commons-text"             % "1.8",
    "ch.qos.logback"               % "logback-classic"          % logbackVersion,
    "com.typesafe.scala-logging"  %% "scala-logging"            % "3.9.2"
  )
}
