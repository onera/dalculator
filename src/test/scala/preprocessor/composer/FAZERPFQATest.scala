package preprocessor.composer

import dalculator.utils.FileManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import preprocessor.ast.Sev

class FAZERPFQATest extends AnyFlatSpec with PFQAComposer with should.Matchers {

  private val removeLayer = (x:String) => x.split("\\.").tail.mkString(".")

  private val ALARM_DICTIONARY = (for{
    file <- FileManager.extractResourceAsFile("FAZER/pfqa/alarmDictionary.csv")
  } yield {
    getDictionary(file)
  }).getOrElse(Map.empty)

  private val ACTION_DICTIONARY = (for{
    file <- FileManager.extractResourceAsFile("FAZER/pfqa/actionDictionary.csv")
  } yield {
    getDictionary(file).transform((_,v) => v.head)
  }).getOrElse(Map.empty)

  val toOutcome: PartialFunction[String, String] = {
    case x if Sev.values.exists(sev => x.contains(sev.toString)) =>
      Sev.values.find(sev => x.contains(sev.toString)).get.name
  }
  val toAlarm: PartialFunction[String, Seq[String]] = {
    case x if x.contains("situation_assessment") =>
      ALARM_DICTIONARY.getOrElse(x, Seq(x))
  }
  val toAction: PartialFunction[String, String] = {
    case x if x.contains(".O") && !x.contains("GroundRiskPolicy") =>
      val m = x.split("[\\^.]").init.last
      ACTION_DICTIONARY.getOrElse(m, m)
  }
  "A functional probable failure analysis" should "be able to parse Cecilia XML and save it as CSV" in {
    for {f <- FileManager.extractResourceAsFile("FAZER/pfqa/functionalPFQA.xml")} yield {
      performAndExportPFQA(f,"fazerFunctionalPFQA.csv",removeLayer)
    }
  }

  "A probable hardware fault analysis" should "be able to parse Cecilia XML and save it as csv" in {
    for {f <- FileManager.extractResourceAsFile("FAZER/pfqa/physicalPFQA.xml")} yield {
      performAndExportPFQA(f,"fazerPhysicalPFQA.csv", removeLayer)
    }
  }
}
