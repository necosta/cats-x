val Scala3 = "3.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "cats-x",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.9",
    libraryDependencies += "org.typelevel" %% "cats-effect-testing-specs2" % "1.3.0" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
    scalaVersion := Scala3
  )

ThisBuild / scalacOptions ++= Seq(
  "-Ykind-projector:underscores"
)
