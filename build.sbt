val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-parse" % "1.0.0",
      "dev.zio" %% "zio" % "2.1.13",
      "dev.zio" %% "zio-streams" % "2.1.13",
    )
  )
