package scalaxb.compiler.xsd

import scalaxb.compiler.Config

import scala.collection.immutable.List

trait GenScalacheckGenerator { self: ContextProcessor =>
  def buildImport: String
  def buildEnumGen(localName: String): String
  def buildDefScalacheckGenerator(
      localName: String,
      param: List[Params#Param]
  ): String

  def buildObjectBlock(defScalacheckGenerator: String): String =
    defScalacheckGenerator

  def buildDefChoiceGenerator(
      className: String,
      param: List[Params#Param]
  ): String
}

class GenScalacheckGeneratorImpl(var config: Config)
    extends GenScalacheckGenerator
    with ContextProcessor {
  override def buildImport: String =
    """
    |import scalacheck.generators._
    |import org.scalacheck.Gen
    |""".stripMargin

  def buildEnumGen(localName: String): String =
    s"val ${lowerCaseFirstChar(localName)}Gen = Gen.oneOf(values)"

  def buildDefScalacheckGenerator(
      localName: String,
      param: List[Params#Param]
  ): String = {

    def forlines(
        params: List[Params#Param]
    )(implicit Gen: ScalacheckGenerator[XsTypeSymbol]): String =
      params.map(x =>
        (
          lowerCaseFirstChar(x.name),
          Gen(x.typeSymbol, x.cardinality, config.useLists, x.choices, config.cardinalityUpperBound)
        )
      ) map {
        case (genName, genType) => ScalacheckGenerator.forline(genName, genType)
      } mkString "\n"

    implicit class StringOps(val s: String) {
      def blockIndent(i: Int): String =
        s split "\n" map (indent(
          i
        ) + _) mkString "\n" //FIXME: use newline to support all platforms
    }
    import ScalacheckGenerator.XsTypeSymbolGen
    s"""
       |def ${lowerCaseFirstChar(localName)}Gen = for {
       |${forlines(param)(XsTypeSymbolGen).blockIndent(1)}
       |} yield ${localName}(${param.map(_.name).mkString(", ")})
       |""".stripMargin
  }

  private def lowerCaseFirstChar(s: String): String = {
    val chars = s.toCharArray
    if (chars.isEmpty) "" else chars(0).toLower + s.substring(1)
  }

  override def buildDefChoiceGenerator(
      className: String,
      param: List[Params#Param]
  ): String = {
    s"def ${lowerCaseFirstChar(className)} = "
  }
}

trait ScalacheckGenerator[T] {
  def apply(
      xsTypeSymbol: T,
      cardinality: Cardinality,
      useList: Boolean,
      choices: Option[ChoiceDecl],
      cardinalityMaxBound: Int
  ): String
}

object ScalacheckGenerator {
  def forline(paramName: String, paramType: String): String =
    s"${lowerCaseFirstChar(paramName)} <- ${paramType}"

  def lowerCaseFirstChar(s: String): String = {
    val chars = s.toCharArray
    if (chars.isEmpty) "" else chars(0).toLower + s.substring(1)
  }
  def indent(indent: Int) = "  " * indent

  implicit class StringOps(val s: String) {
    def blockIndent(i: Int): String =
      s split "\n" map (indent(
        i
      ) + _) mkString "\n" //FIXME: use newline to support all platforms
  }

  def toCardinality(minOccurs: Int, maxOccurs: Int): Cardinality =
    if (maxOccurs > 1) Multiple(minOccurs, maxOccurs)
    else if (minOccurs == 0) Optional
    else Single

  implicit object XsTypeSymbolGen extends ScalacheckGenerator[XsTypeSymbol] {
    override def apply(
        xsTypeSymbol: XsTypeSymbol,
        cardinality: Cardinality,
        useList: Boolean,
        choice: Option[ChoiceDecl],
        cardinalityMaxBound: Int
    ): String =
      xsTypeSymbol match {
        case symbol: ReferenceTypeSymbol =>
          RefGenMaker(symbol, cardinality, useList, choice, cardinalityMaxBound)
        case symbol: BuiltInSimpleTypeSymbol =>
          SimpleGenMaker(symbol, cardinality, useList, choice, cardinalityMaxBound)
        case symbol: XsDataRecord =>
          XsDataRecordGenerator(symbol, cardinality, useList, choice, cardinalityMaxBound)
        case _ => throw new Exception
      }
  }

  def makeTypeCardinality(
      cardinality: Cardinality,
      innerGen: String,
      useLists: Boolean,
      cardinalityMaxBound: Int
  ): String =
    cardinality match {
      case Optional =>
        s"Gen.option($innerGen)"
      case Single => innerGen
      case Multiple(minOccurs, maxOccurs) =>
        val listGeneration =
          if (maxOccurs == Integer.MAX_VALUE)
            s"Gen.choose($minOccurs, $cardinalityMaxBound).flatMap(Gen.listOfN(_, $innerGen))"
          else
            s"Gen.choose($minOccurs, $maxOccurs).flatMap(Gen.listOfN(_, $innerGen))"

        if (useLists)
          s"$listGeneration"
        else
          s"$listGeneration.map(_.toSeq)"
    }

  implicit object RefGenMaker extends ScalacheckGenerator[ReferenceTypeSymbol] {
    override def apply(
        symbol: ReferenceTypeSymbol,
        cardinality: Cardinality,
        useLists: Boolean,
        choice: Option[ChoiceDecl],
        cardinalityMaxBound: Int,
    ): String = {
      def makeGenRef(
          typeName: String,
          cardinality: Cardinality,
          useLists: Boolean,
          cardinalityMaxBound: Int,
      ): String = {
        val innerGen = s"$typeName.${lowerCaseFirstChar(typeName)}Gen"
        makeTypeCardinality(cardinality, innerGen, useLists, cardinalityMaxBound)
      }
      makeGenRef(symbol.name, cardinality, useLists, cardinalityMaxBound)
    }
  }

  implicit object XsDataRecordGenerator
      extends ScalacheckGenerator[XsDataRecord] {
    override def apply(
        xsTypeSymbol: XsDataRecord,
        cardinality: Cardinality,
        useList: Boolean,
        choices: Option[ChoiceDecl],
        cardinalityMaxBound: Int
    ): String =
      xsTypeSymbol.member match {
        case ReferenceTypeSymbol(_) =>
          val nametypes = choices.get.particles.map {
            case _: HasParticle | _: AnyDecl | _: ElemRef =>
              ??? //TODO: implement other nessessary cases.
            case e: ElemDecl =>
              (
                e.name,
                XsTypeSymbolGen(
                  e.typeSymbol,
                  toCardinality(e.minOccurs, e.maxOccurs),
                  false,
                  None,
                  cardinalityMaxBound
                )
              )
          }

          def datarecordGen(key: String) =
            s"""scalaxb.DataRecord(None, Some("$key"), $key)"""

          s"""for {
             |${nametypes.map(x => forline(x._1, x._2)).mkString("\n")}
             |dataRecord <- Gen.oneOf(
             |${nametypes
            .map(x => datarecordGen(x._1))
            .mkString(",\n")
            .blockIndent(2)}
             |  )
             |} yield (dataRecord)
             |""".stripMargin
      }
  }

  implicit object SimpleGenMaker
      extends ScalacheckGenerator[BuiltInSimpleTypeSymbol] {
    override def apply(
        symbol: BuiltInSimpleTypeSymbol,
        cardinality: Cardinality,
        useList: Boolean,
        choice: Option[ChoiceDecl],
        cardinalityMaxBound: Int,
    ): String = {
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
          case XsShort    => "short"
          case XsDecimal  => "decimal"
          case _ =>
            ??? //FIXME: There are lots of other simple types. Right now we support the most frequent ones.
        }
      val innerGen = makeGeneratorName(symbol) + "Gen"
      makeTypeCardinality(cardinality, innerGen, useList, cardinalityMaxBound)
    }
  }

}
