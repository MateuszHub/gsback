name := """BE"""
organization := "uj.mnowak"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.8"

libraryDependencies += guice
libraryDependencies += "io.lemonlabs" % "scala-uri_3" % "4.0.2"
libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.7.4"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test
libraryDependencies += "com.typesafe.slick" %% "slick" % "3.3.3"
libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3"
libraryDependencies += "mysql" % "mysql-connector-java" % "6.0.6"
//libraryDependencies += "com.typesafe.slick" %% "slick-codegen" % "3.3.3"
libraryDependencies += "at.favre.lib" % "bcrypt" % "0.9.0"
libraryDependencies += "com.stripe" % "stripe-java" % "21.4.0"

//(Compile / sourceGenerators) += slick.taskValue // Automatic code generation on build

//lazy val slick = taskKey[Seq[File]]("Generate Tables.scala")
//slick := {
//  val dir = (Compile / sourceManaged).value
//  val outputDir = dir / "slick"
//  val url = "jdbc:mysql://127.0.0.1:3306/gamestore?useSSL=false" // connection info
//  val jdbcDriver = "com.mysql.cj.jdbc.Driver"
//  val slickDriver = "slick.jdbc.MySQLProfile"
//  val pkg = "demo"
//
//  val cp = (Compile / dependencyClasspath).value
//  val s = streams.value
//
//  runner.value.run("slick.codegen.SourceCodeGenerator",
//    cp.files,
//    Array(slickDriver, jdbcDriver, url, outputDir.getPath, pkg, "root", "root"),
//    s.log).failed foreach (sys error _.getMessage)
//
//  val file = outputDir / pkg / "Tables.scala"
//
//  Seq(file)
//}
// Adds additional packages into Twirl
//TwirlKeys.templateImports += "uj.mnowak.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "uj.mnowak.binders._"
