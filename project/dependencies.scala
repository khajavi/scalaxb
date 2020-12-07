import sbt._

object Dependencies {
  val scala213 = "2.13.3"
  val scala212 = "2.12.11"
  val scala211 = "2.11.12"
  val scala210 = "2.10.7"

  val scopt = "com.github.scopt" %% "scopt" % "3.7.1"
  val log4j = "log4j" % "log4j" % "1.2.17"
  val defaultDispatchVersion = "1.0.1"
  def dispatch(sv: String) = CrossVersion partialVersion sv match {
    case Some((2, x)) if x >= 13 => "org.dispatchhttp" %% "dispatch-core" % "1.1.0"
    case Some(_)                 => "org.dispatchhttp" %% "dispatch-core" % "1.0.1"
    case x                       => sys error s"Unexpected Scala version [$sv], with partial version $x"
  }
  val defaultGigahorseVersion = "0.5.0"
  val defaultGigahorseBackend = "okhttp"
  val gigahorse = "com.eed3si9n" %% s"gigahorse-$defaultGigahorseBackend" % defaultGigahorseVersion
  val launcherInterface = "org.scala-sbt" % "launcher-interface" % "0.12.0"
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
  val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
  val scalaParserForScala213 = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  val cxfVersion = "3.3.1"
  val cxfFrontendJaxws = "org.apache.cxf" % "cxf-rt-frontend-jaxws" % cxfVersion
  val cxfFrontendJaxrs = "org.apache.cxf" % "cxf-rt-frontend-jaxrs" % cxfVersion
  val cxfTransportsHttp = "org.apache.cxf" % "cxf-rt-transports-http" % cxfVersion
  val cxfTrapsportsHttpJetty = "org.apache.cxf" % "cxf-rt-transports-http-jetty" % cxfVersion
  val monocleCore = "com.github.julien-truffaut" %% "monocle-core"  % "2.0.3"
  val monocleMacro = "com.github.julien-truffaut" %% "monocle-macro" % "2.0.3"
  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.14.1"
  val scalachecktoolbox = "com.47deg" %% "scalacheck-toolbox-datetime" % "0.4.0"
  val antimirovCore = "org.spire-math" %% "antimirov-core" % "0.2.4"
  val antimirovCheck = "org.spire-math" %% "antimirov-check" % "0.2.4"
  val antimirovProps = "org.spire-math" %% "antimirov-props" % "0.2.4"
  
  def scalaCompiler(sv: String) = "org.scala-lang" % "scala-compiler" % sv

  val specs2Version = "2.4.11"
  def specs2(sv: String) = CrossVersion partialVersion sv match {
    case Some((2,  9)) => "org.specs2" %% "specs2" % "1.12.3"
    case Some((2, 10)) => "org.specs2" %% "specs2" % "2.1.1"
    case Some((2, 11)) => "org.specs2" %% "specs2" % "2.3.13"
    case Some((2, 12)) => "org.specs2" %% "specs2" % "2.4.17"
    case Some((2, 13)) => "org.specs2" %% "specs2-matcher-extra" % "4.6.0"
    case x             => sys error s"Unexpected Scala version [$sv], with partial version $x"
  }

  def appDependencies(sv: String) = Seq(
    launcherInterface % "provided",
    scopt,
    log4j
  ) ++ (sv match {
    case x if sv startsWith "2.10." => Nil
    case x if sv startsWith "2.13." => Seq(scalaXml, scalaParserForScala213) //due to https://github.com/scala/scala-parser-combinators/issues/197
    case _ => Seq(scalaXml, scalaParser)
  })
  def integrationDependencies(sv: String) = Seq(
    dispatch(sv) % "test",
    gigahorse % "test",
    scalaCompiler(sv),
    specs2(sv) % "test",
    cxfFrontendJaxws % "test",
    cxfFrontendJaxrs % "test",
    cxfTransportsHttp % "test",
    cxfTrapsportsHttpJetty % "test",
    monocleCore % Test,
    monocleMacro % Test,
    scalacheck % Test,
    scalachecktoolbox % Test,
    antimirovCore % Test,
    antimirovCheck % Test,
    antimirovProps % Test
  )
}
