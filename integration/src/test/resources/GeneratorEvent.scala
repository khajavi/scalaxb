import scalaxb._
import event._

object PurchaseOrderUsage {
  def main(args: Array[String]): Unit = {
    allTests
  }

  private def allTests: Boolean = {
    testEvent
    true
  }
  def testEvent: Unit = {
    val event = <Event>
      <date>2020-10-12</date>
      <location>
       <Location>
         <country>USA</country>
         <city>New York</city>
       </Location> 
      </location>
    </Event>

    val e = fromXML[Event](event)
    println(e.date)
  }
}
