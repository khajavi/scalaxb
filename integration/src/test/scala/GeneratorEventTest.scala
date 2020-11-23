import java.io.File

import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry.{GenerateScalacheckGenerator, Outdir, PackageNames}

class GeneratorEventTest extends TestBase {
  val inFile = new File("integration/src/test/resources/event.xsd")
  val usageFile = new File(tmp, "GeneratorEvent.scala")
  copyFileFromResource("GeneratorEvent.scala", usageFile)

  private val config = Config.default
    .update(PackageNames(Map(None -> Some("event"))))
    .update(Outdir(tmp))
    .update(GenerateScalacheckGenerator)

  lazy val generated: Seq[File] = module.process(inFile, config)

  "event.scala file must compile so Event can be used" in {
    (
      List(
        """import event._""",
        """import org.scalacheck.Gen""",
        """import org.scalacheck.rng.Seed""",
        """Event.eventGen(Gen.Parameters.default, Seed.random()).get.location.city.nonEmpty"""
      ),
      generated
    ) must evaluateTo(true, outdir = "./tmp", usecurrentcp = true)
  }

}
