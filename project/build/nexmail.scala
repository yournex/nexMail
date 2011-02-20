import sbt._

class NEXMail(info: ProjectInfo) extends DefaultWebProject(info) {
    override def libraryDependencies = Set(
      "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default",
      "junit" % "junit" % "4.5" % "test->default",
      "ch.qos.logback" % "logback-classic" % "0.9.26",
      "org.scala-tools.testing" %% "specs" % "1.6.6" % "test->default",
      "com.h2database" % "h2" % "1.2.138",
      "javax.servlet" % "servlet-api" % "2.5" % "compile->default",
      "javax.mail"    % "mail" % "1.4" % "compile->default"

    ) ++ super.libraryDependencies

}
