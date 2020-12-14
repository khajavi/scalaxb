import java.io.File

import scalaxb.compiler.{Config, Log}
import scalaxb.compiler.ConfigEntry.{
  CardinalityMaxBound,
  GenerateScalacheckGenerator,
  NamedAttributes,
  Outdir,
  PackageNames
}

class GeneratorEventTest extends TestBase {
//  Log.configureLogger(true)
  val event    = new File("integration/src/test/resources/event.xsd")
  val pacs_008_001_09 = new File("integration/src/test/resources/pacs.008.001.09.xsd")
  val pacs_009_001_08 = new File("integration/src/test/resources/pacs.009.001.08.xsd")
  val usageFile = new File(tmp, "GeneratorEvent.scala")
  copyFileFromResource("GeneratorEvent.scala", usageFile)

  private val config = Config.default
    .update(PackageNames(Map(None -> Some("event"))))
    .update(Outdir(tmp))
    .update(GenerateScalacheckGenerator)
    .update(NamedAttributes)
    .update(CardinalityMaxBound(10))

  lazy val generated: Seq[File] = module.processFiles(
    List(
      event,
//      pacs_008_001_09
    ),
    config
  )

  "event.scala file must compile so Event can be used" in {
    (
      List(
        """import event._""",
        """import org.scalacheck.Gen""",
        """import org.scalacheck.rng.Seed""",
        """val b1 = Event.eventGen(Gen.Parameters.default, Seed.random()).get.multipleLocation.isInstanceOf[List[_]]""",
        """val b2 = Event.eventGen(Gen.Parameters.default, Seed.random()).get.singleLocation.isInstanceOf[Location]""",
        """val b3 = Event.eventGen(Gen.Parameters.default, Seed.random()).get.optionalLocation.isInstanceOf[Option[_]]""",
        """b1 && b2 && b3"""
      ),
      generated
    ) must evaluateTo(true, outdir = "./tmp", usecurrentcp = true)
  }

}
