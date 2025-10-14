package preprocessor.composer

import dalculator.cli.CLI
import dalculator.utils.FileManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import preprocessor.ast.Sev

class FAZERPFQATest extends AnyFlatSpec with should.Matchers {

  "For FAZER, the dalculator CLI" should "perform functional PFQA" in {
    for {
      commandFile <- FileManager.extractResourceAsFile("FAZER/commandFiles/functional_pfqa.txt")
    } yield {
      CLI.main(Array(commandFile))
    }
  }

  it should "perform physical PFQA" in {
    for {
      commandFile <- FileManager.extractResourceAsFile("FAZER/commandFiles/physical_pfqa.txt")
    } yield {
      CLI.main(Array(commandFile))
    }
  }
}
