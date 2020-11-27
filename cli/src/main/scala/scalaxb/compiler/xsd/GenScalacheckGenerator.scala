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
  override def buildImport: String =
    """
    |import scalacheck.generators._
    |import org.scalacheck.Gen
    |""".stripMargin

  def buildDefScalacheckGenerator(
      localName: String,
      param: List[Params#Param]
  ): String = {
    def lowerCaseFirstChar(s: String): String = {
      val chars = s.toCharArray
      if (chars.isEmpty) "" else chars(0).toLower + s.substring(1)
    }

    def forline(paramName: String, paramType: String): String =
      s"${lowerCaseFirstChar(paramName)} <- ${paramType}"

    def forlines(
        params: List[Params#Param]
    )(implicit Gen: ScalacheckGenerator[XsTypeSymbol]): String =
      params.map(x =>
        (
          lowerCaseFirstChar(x.name),
          Gen(x.typeSymbol, x, config.useLists)
        )
      ) map {
        case (genName, genType) => forline(genName, genType)
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

}

trait ScalacheckGenerator[T] {
  def apply(xsTypeSymbol: T, param: Params#Param, useList: Boolean): String
}

object ScalacheckGenerator {
  def lowerCaseFirstChar(s: String): String = {
    val chars = s.toCharArray
    if (chars.isEmpty) "" else chars(0).toLower + s.substring(1)
  }
  implicit object XsTypeSymbolGen extends ScalacheckGenerator[XsTypeSymbol] {
    override def apply(
        xsTypeSymbol: XsTypeSymbol,
        param: Params#Param,
        useList: Boolean
    ): String =
      xsTypeSymbol match {
        case symbol: ReferenceTypeSymbol =>
          RefGenMaker(symbol, param, useList)
        case symbol: BuiltInSimpleTypeSymbol =>
          SimpleGenMaker(symbol, param, useList)
        case _ => throw new Exception
      }
  }

  def makeTypeCardinality(
      cardinality: Cardinality,
      innerGen: String,
      useLists: Boolean
  ): String =
    cardinality match {
      case Optional =>
        s"Gen.option($innerGen)"
      case Single => innerGen
      case Multiple(minOccurs, maxOccurs) =>
        val listGeneration =
          s"Gen.choose($minOccurs, $maxOccurs).flatMap(Gen.listOfN(_, $innerGen))"
        if (useLists)
          s"$listGeneration"
        else
          s"$listGeneration.map(_.toSeq)"
    }

  implicit object RefGenMaker extends ScalacheckGenerator[ReferenceTypeSymbol] {
    override def apply(
        symbol: ReferenceTypeSymbol,
        param: Params#Param,
        useLists: Boolean
    ): String = {
      def makeGenRef(
          typeName: String,
          cardinality: Cardinality,
          useLists: Boolean
      ): String = {
        val innerGen = s"$typeName.${lowerCaseFirstChar(typeName)}Gen"
        makeTypeCardinality(cardinality, innerGen, useLists)
      }
      makeGenRef(symbol.name, param.cardinality, useLists)
    }
  }

  implicit object SimpleGenMaker
      extends ScalacheckGenerator[BuiltInSimpleTypeSymbol] {
    override def apply(
        symbol: BuiltInSimpleTypeSymbol,
        param: Params#Param,
        useList: Boolean
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
     val innerGen =  makeGeneratorName(symbol) + "Gen"
      makeTypeCardinality(param.cardinality, innerGen, useList)
    }
  }

}
