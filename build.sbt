name := "Year3Project"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
    "org.slf4j"%"slf4j-simple"%"2.0.0-alpha1",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
    "com.novocode" % "junit-interface" % "0.11" % Test
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
)


