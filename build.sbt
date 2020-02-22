name := "Year3Project"

version := "0.4.1"

scalaVersion := "2.12.10"

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")

libraryDependencies ++= Seq(
    "org.slf4j"%"slf4j-api"%"1.7.25",
    "ch.qos.logback"%"logback-classic"%"1.2.3",
    "ch.qos.logback"%"logback-core"%"1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    "org.scalactic" %% "scalactic" % "3.1.0",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
    //"com.lihaoyi" %% "fastparse" % "2.2.2",
    "org.scala-lang" % "scala-compiler" % "2.12.10",
    //"com.novocode" % "junit-interface" % "0.11" % Test
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
    "-language:postfixOps",
    "-feature"
    //"-Ypatmat-exhaust-depth 40"
)


