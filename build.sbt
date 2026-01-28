val catsVersion = "2.13.0"

val kittensVersion = "3.5.0"

val catsParseVersion = "1.1.0"

val catsEffectVersion = "3.6.3"

val fs2Version = "3.12.2"

val drosteVersion = "0.10.0"

val munitVersion = "1.2.2"

val munitScalacheckVersion = "1.2.0"

val munitCatsEffectVersion = "2.1.0"

val scalacheckVersion = "1.19.0"

val scalacheckEffectMunitVersion = "1.0.4"

val disciplineMunitVersion = "2.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.horothesun",
    name := "advent-of-code-2023",
    scalaVersion := "3.8.1",
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
