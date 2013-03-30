libraryDependencies ++= List(
  "jfree" % "jfreechart" % "1.0.12",
  "commons-io" % "commons-io" % "2.0"
)

organization := "com.github.scala-incubator"

name := "sperformance"

version := "0.1"

scalaVersion := "2.9.2"


  // ----------------------- Shared Settings ----------------------- //
publishTo <<= version { v: String =>
	val nexus = "https://oss.sonatype.org/"
	if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
	else                             Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
    <scm>
      <url>git@github.com:jesseeichar/sperformance.git</url>
      <connection>scm:git:git@github.com:jesseeichar/sperformance.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jesseeichar</id>
        <name>Jesse Eichar</name>
      </developer>
    </developers>
  )

licenses := Seq("Scala License" -> url("http://www.scala-lang.org/node/146"))

homepage := Some(url("http://jesseeichar.github.com/scala-io-doc/index.html"))
