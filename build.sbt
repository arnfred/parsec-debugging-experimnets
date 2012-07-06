name := "parsec-debugging-experiments"

organization := "EPFL"

resolvers += "snapshots" at 
"https://oss.sonatype.org/content/repositories/snapshots/" 

version := "0.0.1"


// pick local version of scala compiler & library

scalaHome <<= baseDirectory { f =>
  val props = new java.util.Properties()
  IO.load(props, f / "local.properties")
  val x = props.getProperty("scala.home")
  if (x == null)
    sys.error("I need scala library with support for debugging parser combinators. Define scala.home in local.properties")
  else {
    println("Using: " + x)
    Some(file(x))
  }
}

unmanagedJars in Compile <++= (scalaHome, baseDirectory) map { (sHome, base) =>
  val scalaCompiler = (sHome.get / "lib" / "scala-compiler.jar")
  val unmanagedDirs = base +++ (base / "lib")
  val allJars = (unmanagedDirs ** ".jars") +++ scalaCompiler
  allJars.classpath
}

scalacOptions in Compile ++= Seq("-unchecked")//, "-Ymacro-debug-verbose", "-Xlog-implicits", " -Xmax-classfile-name 100")

scalaVersion := "2.10.0-SNAPSHOT"

resolvers ++= Seq(ScalaToolsSnapshots)

