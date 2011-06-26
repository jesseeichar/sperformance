package sperformance
package baseline

import historical.JarBaseLine
import java.io.File
import intelligence.ClusterResults

object TestBaseline extends JarBaseLine {
  def jar = getClass.getClassLoader.getResource("test-baseline.jar")
  val testPackages = List(getClass.getPackage.getName)
}

object BaselineTest extends sperformance.dsl.PerformanceDSLTest {
  
 // override val baseLine = TestBaseline
  
  performance of "Strict Range" in {
    measure method "foreach" in {
      withSize upTo 2000 by 100 withSetup {
        size =>
          1 to size by 50
      } run {
        case range =>
          range.foreach { r => r }
      }
    }
    measure method "filter" in {
      withSize upTo 2000 by 100 withSetup {
        size =>
          1 to size by 50
      } run {
        case range =>
          range.filter { _ < 10 }
      }
    }
  }
  
  def main(args:Array[String]) {
    val csvFile: File = new java.io.File("target/sperformance/TestBaseline.csv")
    val cxt = new CSVRunContext(csvFile)
    BaselineTest.runTest(cxt)
    cxt.writeResults()
    val out: File = new java.io.File("target/sperformance/graphs")
    out.mkdirs()

    val dcxt = new DefaultRunContext(out, "test") {
      override val testContext: ClusterResults = CSV.load(csvFile.toURI.toURL)
    }
    dcxt.generateResultsPage()
  }
}
