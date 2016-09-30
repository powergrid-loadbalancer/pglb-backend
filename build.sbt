name := "PowerGridLoadBalancer"

version := "1.0"

lazy val `powergridloadbalancer` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq( jdbc , cache , ws   , specs2 % Test )

libraryDependencies += "com.microsoft.azure" % "azure" % "1.0.0-beta2"

libraryDependencies += "com.microsoft.azure" % "azure-core" % "0.9.4"

libraryDependencies += "com.microsoft.azure" % "azure-servicebus" % "0.9.4"

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"  