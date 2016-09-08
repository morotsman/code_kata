
resolvers := Seq("typesafe" at "http://repo.typesafe.com/typesafe/releases/")

lazy val root = (project in file(".")).
  settings(
    name := "kata",
    version := "1.0",
    scalaVersion := "2.11.4",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    libraryDependencies += "com.typesafe.play" %% "play" % "2.5.0"
  )
