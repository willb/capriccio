name := "capriccio"

organization := "com.freevariable"

version := "0.0.1"

scalaVersion := "2.10.5"

crossScalaVersions := Seq("2.10.5", "2.11.8")

def commonSettings = Seq(
)

seq(commonSettings:_*)

seq(bintraySettings:_*)

seq(bintrayPublishSettings:_*)

licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0"))

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/root-doc.txt")

site.settings

site.includeScaladoc()

site.jekyllSupport()

ghpages.settings

git.remoteRepo := "git@github.com:willb/capriccio.git"

lazy val capriccio = project in file(".")

initialCommands in (Compile, console) += "import com.freevariable.capriccio._"