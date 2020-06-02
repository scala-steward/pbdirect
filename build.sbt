ThisBuild / scalaVersion := "2.13.1"
ThisBuild / crossScalaVersions := Seq("2.12.11", "2.13.1")
ThisBuild / organization := "com.47deg"

addCommandAlias(
  "ci-test",
  "scalafmtCheckAll; scalafmtSbtCheck; mdoc; +coverage; +test; +coverageReport; +coverageAggregate"
)
addCommandAlias("ci-docs", "mdoc; headerCreateAll")

lazy val pbdirect = project
  .settings(Compile / scalacOptions -= "-Xfatal-warnings")
  .settings(libraryDependencies ++= Seq(
    "com.beachape"               %% "enumeratum"                % "1.6.1",
    "com.chuusai"                %% "shapeless"                 % "2.3.3",
    "com.google.protobuf"         % "protobuf-java"             % "3.12.2",
    "org.typelevel"              %% "cats-core"                 % "2.1.1",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5" % Test,
    "com.github.os72"             % "protoc-jar"                % "3.11.4" % Test,
    "org.scalatest"              %% "scalatest"                 % "3.1.2" % Test,
    "org.scalatestplus"          %% "scalacheck-1-14"           % "3.1.2.0" % Test
  ))

lazy val documentation = project
  .dependsOn(pbdirect)
  .settings(skip in publish := true)
  .enablePlugins(MdocPlugin)
  .settings(mdocOut := file("."))
