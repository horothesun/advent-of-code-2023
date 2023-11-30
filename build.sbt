val catsVersion = "2.10.0"

val kittensVersion = "3.1.0"

val catsEffectVersion = "3.5.2"

val fs2Version = "3.9.3"

val munitVersion = "0.7.29"

val disciplineMunitVersion = "1.0.9"

val munitCatsEffectVersion = "1.0.7"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.horothesun",
    name         := "advent-of-code-2023",
    version      := "0.1.0",
    scalaVersion := "3.3.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"           % catsVersion,
      "org.typelevel" %% "kittens"             % kittensVersion,
      "org.typelevel" %% "cats-effect"         % catsEffectVersion,
      "co.fs2"        %% "fs2-core"            % fs2Version,
      "org.typelevel" %% "cats-laws"           % catsVersion            % Test,
      "org.scalameta" %% "munit"               % munitVersion           % Test,
      "org.scalameta" %% "munit-scalacheck"    % munitVersion           % Test,
      "org.typelevel" %% "discipline-munit"    % disciplineMunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion % Test
    )
  )
