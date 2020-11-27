import java.io.File

import scalaxb.compiler.Module
import scalaxb.compiler.xsd.Driver

object SampleApp extends App {
  val module: Module = new Driver
  val tmpDir = new File("tmp")
  val input = new File("integration/src/test/resources/event.xsd")
  module.process(input, "foo", tmpDir)
}
