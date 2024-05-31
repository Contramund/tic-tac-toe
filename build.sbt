ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.10"
ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

val catsVersion = "2.9.0"
val catsEffect3 = "3.4.8"
val scalatestVersion = "3.2.17"
val scalamockVersion = "5.2.0"
val logbackVersion = "1.4.11"

//scalacOptions ++= Seq(
// "-Ypatmat-exhaust-depth", "80",
// "-Ywarn-value-discard", "true",
//)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      // cats
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffect3,

      // logback
      "ch.qos.logback" % "logback-classic" % logbackVersion,

      // test
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "org.scalamock" %% "scalamock" % scalamockVersion % Test,
    ),
    name := "hw6",
    coverageEnabled := true,
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 90,
    coverageMinimumBranchTotal := 90,
  )
  .enablePlugins(JavaAppPackaging)
