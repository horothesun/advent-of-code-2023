val catsVersion = "2.10.0"

val kittensVersion = "3.3.0"

val fs2Version = "3.10.0"

val catsParseVersion = "1.0.0"

val drosteVersion = "0.9.0"

val munitVersion = "0.7.29"

val disciplineMunitVersion = "1.0.9"

val munitCatsEffectVersion = "1.0.7"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.horothesun",
    name := "advent-of-code-2023",
    scalaVersion := "3.3.3",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "kittens" % kittensVersion,
      "co.fs2" %% "fs2-core" % fs2Version,
      "org.typelevel" %% "cats-parse" % catsParseVersion,
      "io.higherkindness" %% "droste-core" % drosteVersion,
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitVersion % Test,
      "org.typelevel" %% "discipline-munit" % disciplineMunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion % Test
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
