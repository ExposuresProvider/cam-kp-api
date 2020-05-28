enablePlugins(JavaAppPackaging)

organization  := "org.renci"

name          := "cam-kp-api"

version       := "0.1"

licenses := Seq("MIT license" -> url("https://opensource.org/licenses/MIT"))

scalaVersion  := "2.12.11"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ypartial-unification")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

val zioVersion = "1.0.0-RC20"
val tapirVersion = "0.15.3"
val http4sVersion = "0.21.4"

libraryDependencies ++= {
  Seq(
    "dev.zio"                     %% "zio"                     % zioVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-core"              % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-zio"               % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-zio-http4s-server" % tapirVersion,
    "org.http4s"                  %% "http4s-blaze-server"     % http4sVersion,
    "org.apache.jena"             %  "apache-jena-libs"        % "3.15.0",
    "org.phenoscape"              %% "sparql-utils"            % "1.2",
    "io.circe"                    %% "circe-core"              % "0.13.0",
    "io.circe"                    %% "circe-generic"           % "0.13.0",
    "io.circe"                    %% "circe-parser"            % "0.13.0",
    "dev.zio"                     %% "zio-test"                % zioVersion % Test,
    "dev.zio"                     %% "zio-test-sbt"            % zioVersion % Test
  )
}
