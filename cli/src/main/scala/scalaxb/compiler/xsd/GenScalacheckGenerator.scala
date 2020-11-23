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
  override def buildImport: String = "import scalacheck.generators._"

  def buildDefScalacheckGenerator(
      className: String,
      param: List[Params#Param]
  ): String = {
    def lowerCaseFirstChar(s: String): String = {
      val chars = s.toCharArray
      if (chars.isEmpty) "" else chars(0).toLower + s.substring(1)
    }

    def forline(paramName: String, paramType: String): String =
      s"${lowerCaseFirstChar(paramName)} <- ${paramType}Gen"

    def makeGeneratorName(t: BuiltInSimpleTypeSymbol): String =
      t match {
        case XsDuration => "duration"
        case XsDateTime => "dateTime"
        case XsTime => "time"
        case XsDate => "date"
        case XsBoolean => "boolean"
        case XsFloat => "float"
        case XsDouble => "double"
        case XsString => "string"
        case XsInteger => "bigInt"
        case XsLong => "long"
        case XsInt => "int"
        case XsByte => "byte"
        case XsShort => "short"
        case XsDecimal => "decimal"
        case _ => ??? //FIXME: There are lots of other simple types. Right now we support the most frequent ones.
      }

    def forlines(params: List[Params#Param]): String =
      params.map { x =>
        x.typeSymbol match {
          case symbol: BuiltInSimpleTypeSymbol =>
            (lowerCaseFirstChar(x.name), makeGeneratorName(symbol))
          case ref: ReferenceTypeSymbol =>
            (lowerCaseFirstChar(x.name), ref.name + "." + lowerCaseFirstChar(ref.name))
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
