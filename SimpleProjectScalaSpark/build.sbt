name := "simpleprojectscalaspark"
version := "1.0"
scalaVersion := "2.11.12"
libraryDependencies ++= Seq(
"org.apache.spark" % "spark-core_2.11" % "2.4.0",
"org.apache.spark" % "spark-sql_2.11" % "2.4.0",
"org.apache.spark" % "spark-streaming_2.11" % "2.4.0",
"org.apache.spark" % "spark-mllib_2.11" % "2.4.0",
"org.jmockit" % "jmockit" % "1.34" % "test",
 "org.scalacheck" %% "scalacheck" % "1.14.1",
"junit" % "junit" % "4.13-rc-1",
 "org.scalactic" %% "scalactic" % "3.0.8",
 "org.scalatest" %% "scalatest" % "3.0.8" % "test",
 "org.json4s" %% "json4s-jackson" % "3.2.11"
)