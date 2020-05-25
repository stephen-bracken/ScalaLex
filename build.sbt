name := "ScalaLex"

version := "0.6"

scalaVersion := "2.12.10"

mainClass := Some("scalaLex.Generator")

unmanagedSources / excludeFilter := HiddenFileFilter || "*output*" || "Lexer.scala"

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"//used for supersafe compiler plugin

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")//used with scalatest

enablePlugins(ClassDiagramPlugin)

libraryDependencies ++= Seq(
    "org.slf4j"%"slf4j-api"%"1.7.25",
    "ch.qos.logback"%"logback-classic"%"1.2.3",
    "ch.qos.logback"%"logback-core"%"1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    "org.scalactic" %% "scalactic" % "3.1.0",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    "org.scala-lang" % "scala-compiler" % "2.12.10",
)

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
// see https://tpolecat.github.io/2017/04/25/scalac-flags.html for scalacOptions descriptions
scalacOptions ++= Seq(
    "-deprecation",     //emit warning and location for usages of deprecated APIs
    "-unchecked",       //enable additional warnings where generated code depends on assumptions
    "-explaintypes",    //explain type errors in more detail
    "-Ywarn-dead-code", //warn when dead code is identified
    "-Xfatal-warnings",  //fail the compilation if there are any warnings
    "-language:implicitConversions",
    "-language:postfixOps", //enables postfix notation in project
    "-feature"  //feature warnings (e.g. postfixOps)
    //"-Ypatmat-exhaust-depth 40" //pattern matching depth
)


