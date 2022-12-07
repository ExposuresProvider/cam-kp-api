import com.typesafe.sbt.packager.docker._

enablePlugins(JavaAppPackaging, DockerPlugin, ScoverageSbtPlugin)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

organization := "org.renci"

name := "cam-kp-api"

version := "0.3-pre1"

licenses := Seq("MIT license" -> url("https://opensource.org/licenses/MIT"))

scalaVersion := "2.13.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

Compile / packageDoc / publishArtifact := false
Test / fork := true

configs(IntegrationTest)
Defaults.itSettings
IntegrationTest / parallelExecution := false

coverageExcludedPackages := "<empty>;org\\.renci\\.cam\\.domain\\..*;org\\.renci\\.cam\\.Server.*;org\\.renci\\.cam\\.Biolink.*;org\\.renci\\.cam\\.HttpClient.*;org\\.renci\\.cam\\.AppConfig.*;org\\.renci\\.cam\\.Util.*;org\\.renci\\.cam\\.util\\.UpdateBiolinkResources.*;org\\.renci\\.cam\\.SPARQLQueryExecutor.*"

val zioVersion = "1.0.16"
val zioConfigVersion = "1.0.0-RC29-1"
val zioCacheVersion = "0.1.2"
val tapirVersion = "0.19.0-M13"
val http4sVersion = "0.23.12"
val circeVersion = "0.14.1"
val logbackVersion = "1.2.11"

reStart / javaOptions += "-Xmx16G"

libraryDependencies ++= {
  Seq(
    "dev.zio"                     %% "zio"                            % zioVersion,
    "dev.zio"                     %% "zio-interop-cats"               % "3.2.9.1",
    "dev.zio"                     %% "zio-config"                     % zioConfigVersion,
    "dev.zio"                     %% "zio-config-magnolia"            % zioConfigVersion,
    "dev.zio"                     %% "zio-config-typesafe"            % zioConfigVersion,
    "dev.zio"                     %% "zio-cache"                      % zioCacheVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-core"                     % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-zio"                      % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-zio-http4s-server"        % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-cats"                     % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-json-circe"               % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"             % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml"       % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui"               % tapirVersion,
    "org.http4s"                  %% "http4s-blaze-server"            % http4sVersion,
    "org.http4s"                  %% "http4s-dsl"                     % http4sVersion,
    "org.http4s"                  %% "http4s-blaze-client"            % http4sVersion,
    "org.http4s"                  %% "http4s-circe"                   % http4sVersion,
    "org.apache.jena"              % "apache-jena-libs"               % "4.5.0",
    "org.phenoscape"              %% "sparql-utils"                   % "1.3.1",
    "org.apache.commons"           % "commons-text"                   % "1.9",
    "org.apache.commons"           % "commons-csv"                    % "1.9.0",
    "io.circe"                    %% "circe-core"                     % circeVersion,
    "io.circe"                    %% "circe-generic"                  % circeVersion,
    "io.circe"                    %% "circe-parser"                   % circeVersion,
    "io.circe"                    %% "circe-yaml"                     % circeVersion,
    "dev.zio"                     %% "zio-test"                       % zioVersion % "it,test",
    "dev.zio"                     %% "zio-test-sbt"                   % zioVersion % "it,test",
    "com.dimafeng"                %% "testcontainers-scala-scalatest" % "0.40.12"  % "it,test",
    "com.google.guava"             % "guava"                          % "31.1-jre",
    "ch.qos.logback"               % "logback-classic"                % logbackVersion,
    "com.typesafe.scala-logging"  %% "scala-logging"                  % "3.9.5"
  )
}

dockerBaseImage := "openjdk:17-alpine"
Docker / daemonUser := "camkpapi"
dockerExposedPorts += 8080

// test
//dockerEnvVars ++= Map("JAVA_OPTS" -> "-Xmx16g -Xms16g",
//                      "SPARQL_ENDPOINT" -> sys.env("SPARQL_ENDPOINT"),
//                      "CAM_KP_LOG_LEVEL" -> sys.env("CAM_KP_LOG_LEVEL"))

// dev
//dockerEnvVars ++= Map(
//  "JAVA_OPTS" -> "-Xmx16g -Xms16g -DTRAPI_VERSION=1.2.0 -DLOCATION=https://cam-kp-api-dev.renci.org -DMATURITY=development",
//  "SPARQL_ENDPOINT" -> sys.env("SPARQL_ENDPOINT"),
//  "CAM_KP_LOG_LEVEL" -> sys.env("CAM_KP_LOG_LEVEL")
//)

// prod
dockerEnvVars ++= Map(
  "JAVA_OPTS" -> "-Xmx16g -Xms16g",
  "TRAPI_VERSION" -> "1.3.0",
  "LOCATION" -> sys.env.getOrElse("LOCATION", "https://cam-kp-api.renci.org"),
  "MATURITY" -> sys.env.getOrElse("MATURITY", "production"),
  "SPARQL_ENDPOINT" -> sys.env.getOrElse("SPARQL_ENDPOINT", "https://cam-kp-sparql.apps.renci.org/sparql"),
  "CAM_KP_LOG_LEVEL" -> sys.env.getOrElse("CAM_KP_LOG_LEVEL", "info")
)

dockerEntrypoint := Seq("/opt/docker/bin/server")
Docker / dockerApiVersion := Some(DockerApiVersion(1, 40))
dockerChmodType := DockerChmodType.UserGroupWriteExecute
Docker / dockerRepository := Some("renciorg")
dockerCommands := dockerCommands.value.flatMap {
  case cmd @ Cmd("EXPOSE", _) => List(Cmd("RUN", "apk update && apk add bash curl"), cmd)
  case other                  => List(other)
}
