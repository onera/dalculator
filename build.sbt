name := "dalculator"

version := "2.0"

scalaVersion := "2.13.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % "test"

// fork the VM for program execution
fork := true

// options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature","-Ymacro-annotations")

javacOptions += "-Xlint:deprecation"

// compile order
Compile / compileOrder := CompileOrder.Mixed

assembly / mainClass := Some("dalculator.gui.DalculatorGraphicalInterface")

// scalaz
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.7"

// shapeless
resolvers ++= Seq(
  Resolver.sonatypeRepo("public"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.10"
)

// spire for numeric typeclasses
libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"

// command line parsing library
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.1.0"

// macro support Lenses and circe
libraryDependencies ++= Seq(
  "dev.optics" %% "monocle-core"  % "3.1.0",
  "dev.optics" %% "monocle-macro" % "3.1.0",
)