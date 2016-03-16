unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

retrieveManaged := true
