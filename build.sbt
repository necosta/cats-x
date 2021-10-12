val Scala3 = "3.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "collatz-x",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.9",
    libraryDependencies += "org.typelevel" %% "cats-effect-testing-specs2" % "1.3.0" % Test,
    scalaVersion := Scala3
  )


