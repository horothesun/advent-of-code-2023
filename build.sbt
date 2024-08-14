val catsVersion = "2.12.0"

val kittensVersion = "3.3.0"

val fs2Version = "3.10.2"

val catsParseVersion = "1.0.0"

val drosteVersion = "0.9.0"

val munitVersion = "1.0.1"

val munitScalacheckVersion = "1.0.0"

val disciplineMunitVersion = "2.0.0"

val munitCatsEffectVersion = "2.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.horothesun",
    name := "advent-of-code-2023",
    scalaVersion := "3.4.3",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "kittens" % kittensVersion,
      "co.fs2" %% "fs2-core" % fs2Version,
      "org.typelevel" %% "cats-parse" % catsParseVersion,
      "io.higherkindness" %% "droste-core" % drosteVersion,
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitScalacheckVersion % Test,
      "org.typelevel" %% "discipline-munit" % disciplineMunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect" % munitCatsEffectVersion % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-language:postfixOps",
      "-source:future",
      "-explain",
      "-Wvalue-discard",
      "-Wunused:all"
    )
  )
