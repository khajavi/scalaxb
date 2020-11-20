package scalaxb.compiler.xsd

import scalaxb.compiler.Config

import scala.collection.immutable.List

trait GenScalacheckGenerator { self: ContextProcessor =>
  def buildImport: String
  def buildDefScalacheckGenerator(
      className: String,
      param: List[Params#Param]
  ): String
  def buildObjectBlock(defScalacheckGenerator: String): String =
    defScalacheckGenerator
}

class GenScalacheckGeneratorImpl(var config: Config)
    extends GenScalacheckGenerator
    with ContextProcessor {
  override def buildImport: String = //FIXME: move all SimpleTypeGenerator to separate file
    """
      |object SimpleTypeGenerators {
      |  import org.scalacheck.Gen
      |
      |  import org.scalacheck.Arbitrary
      |  val stringGen = Arbitrary(
      |    Gen.listOf[Char](Arbitrary.arbChar.arbitrary)
      |  ).arbitrary.map(_.mkString)
      |
      |// Date and DateTime generators check for ZONE
      |  import org.joda.time._
      |  import com.fortysevendeg.scalacheck.datetime.instances.joda._
      |  import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
      |  import javax.xml.datatype.XMLGregorianCalendar
      |  import javax.xml.datatype.DatatypeFactory
      |  import javax.xml.datatype.DatatypeConstants
      |  def dateTimeGen(from: DateTime, range: Period): Gen[DateTime] =
      |    genDateTimeWithinRange(from, range)
      |  def xmlDateTimeGen(dateTimeGen: Gen[DateTime]): Gen[XMLGregorianCalendar] =
      |    for { date <- dateTimeGen } yield DatatypeFactory
      |      .newInstance()
      |      .newXMLGregorianCalendar(date.toGregorianCalendar)
      |  val pastDateTimeGen: Gen[XMLGregorianCalendar] = xmlGen(
      |    genDateTimeWithinRange(DateTime.now, Period.years(-100))
      |  )
      |  val xmlDateTimeGenPast: Gen[XMLGregorianCalendar] = for {
      |    date <- pastDateTimeGen
      |  } yield DatatypeFactory
      |    .newInstance()
      |    .newXMLGregorianCalendar(date.toGregorianCalendar())
      |
      |  import com.fortysevendeg.scalacheck.datetime.joda.granularity.hours
      |  def dateGen(from: DateTime, range: Period): Gen[DateTime] =
      |    genDateTimeWithinRange(from, range)
      |  def xmlGen(dateGen: Gen[DateTime]): Gen[XMLGregorianCalendar] =
      |    for { date <- dateGen } yield DatatypeFactory
      |      .newInstance()
      |      .newXMLGregorianCalendarDate(
      |        date.getYear(),
      |        date.getMonthOfYear(),
      |        date.getDayOfMonth(),
      |        DatatypeConstants.FIELD_UNDEFINED
      |      )
      |  val xmlDateGenPast: Gen[XMLGregorianCalendar] = xmlGen(
      |    genDateTimeWithinRange(DateTime.now, Period.years(-100))
      |  )
      |  val dateGen: Gen[XMLGregorianCalendar] = pastDateTimeGen
      |
      |}
      |import SimpleTypeGenerators.{dateGen, stringGen}
      |""".stripMargin

  def buildDefScalacheckGenerator(
      className: String,
      param: List[Params#Param]
  ): String = {
    def lowerCaseFirstChar(s: String): String = {
      val chars = s.toCharArray
      if (chars.isEmpty) "" else (chars(0).toLower + s.substring(1))
    }

    def forline(paramName: String, paramType: String): String =
      s"${lowerCaseFirstChar(paramName)} <- ${lowerCaseFirstChar(paramType)}Gen"

    def makeGeneratorName(t: BuiltInSimpleTypeSymbol): String =
      t match {
        case XsDuration => "duration"
        case XsDateTime => "dateTime"
        case XsTime     => "time"
        case XsDate     => "date"
        case XsBoolean  => "boolean"
        case XsFloat    => "float"
        case XsDouble   => "double"
        case XsString   => "string"
        case XsInteger  => "bigInt"
        case XsLong     => "long"
        case XsInt      => "int"
        case XsByte     => "byte"
        case _ =>
          ??? //FIXME: There are lots of other simple types. Right now we support the most frequent ones.
      }

    def forlines(params: List[Params#Param]): String =
      params.map { x =>
        x.typeSymbol match {
          case symbol: BuiltInSimpleTypeSymbol =>
            (lowerCaseFirstChar(x.name), makeGeneratorName(symbol))
          case _ =>
            throw new Exception(
              "Other types not supported yet"
            ) //FIXME: Finally all type should be supported.
        }
      } map {
        case (genName, genType) => forline(genName, genType)
      } mkString "\n"

    implicit class StringOps(val s: String) {
      def blockIndent(i: Int): String =
        s split "\n" map(indent(i) + _) mkString "\n" //FIXME: use newline to support all platforms
    }

    s"""
       |def ${lowerCaseFirstChar(className)}Gen = for {
       |${forlines(param).blockIndent(1)}
       |} yield ${className}(${param.map(_.name).mkString(", ")})
       |""".stripMargin
  }

}
