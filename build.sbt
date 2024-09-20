val catsVersion = "2.12.0"

val kittensVersion = "3.4.0"

val catsParseVersion = "1.0.0"

val catsEffectVersion = "3.5.4"

val fs2Version = "3.11.0"

val drosteVersion = "0.9.0"

val munitVersion = "1.0.2"

val munitScalacheckVersion = "1.0.0"

val munitCatsEffectVersion = "2.0.0"

val scalacheckVersion = "1.18.1"

val scalacheckEffectMunitVersion = "1.0.4"

val disciplineMunitVersion = "2.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.horothesun",
    name := "advent-of-code-2023",
    scalaVersion := "3.5.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "kittens" % kittensVersion,
      "org.typelevel" %% "cats-parse" % catsParseVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "co.fs2" %% "fs2-core" % fs2Version,
      "io.higherkindness" %% "droste-core" % drosteVersion,
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.typelevel" %% "munit-cats-effect" % munitCatsEffectVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitScalacheckVersion % Test,
      "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test,
      "org.typelevel" %% "scalacheck-effect-munit" % scalacheckEffectMunitVersion % Test,
      "org.typelevel" %% "cats-effect-testkit" % catsEffectVersion % Test,
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      "org.typelevel" %% "discipline-munit" % disciplineMunitVersion % Test
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
