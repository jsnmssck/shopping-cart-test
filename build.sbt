ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.16"

val circeVersion = "0.14.13"
lazy val root    = (project in file(".")).settings(
  name := "cats-effect-3-quick-start",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.5.4",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.5.4",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.5.4",
    // newtype without overhead
    "io.estatico"         %% "newtype"              % "0.4.4",
    "com.squareup.okhttp3" % "okhttp"               % "4.12.0",
    "io.circe"            %% "circe-generic-extras" % "0.14.4",
    // better monadic for compiler plugin as suggested by documentation
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
  ),
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion),
  scalacOptions ++= Seq("-Ymacro-annotations")
)
