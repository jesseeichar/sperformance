package sperformance
package baseline

import historical.JarBaseLine
import java.io.File
import intelligence.ClusterResults
import store.{XmlStoreResults,XmlLoadResults}

object TestBaseline extends JarBaseLine {
  def jar = getClass.getClassLoader.getResource("test-baseline.jar")
  val testPackages = List(getClass.getPackage.getName)
}

object BaselineTest extends sperformance.dsl.PerformanceDSLTest {
  
 // override val baseLine = TestBaseline
  
  performance of "Strict Range" in {
    measure method "map" in {
      withSize upTo 2000 by 100 withSetup {
        size =>
          1 to size by 50
      } run {
        case range =>
          range.map { r => r+1 }
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
    val outFile: File = new java.io.File("target/sperformance/TestBaseline.csv")
    val cxt = new HistoricalRunContext(new File("target/sperformance/history"), true, new XmlStoreResults(_))

    BaselineTest.runTest(cxt)
    cxt.writeBaseline("BaselineTest")

    val graphDir: File = new java.io.File("target/sperformance/graphs")
    graphDir.mkdirs()

    val loadedResults = new XmlLoadResults(outFile.toURI.toURL).read()
    

    val dcxt = new DefaultRunContext(graphDir, "test") {
      override val testContext: ClusterResults = loadedResults
    }
    dcxt.generateResultsPage()

    val graphDir2: File = new java.io.File("target/sperformance/graphsExpected")
    graphDir2.mkdirs()

    val dcxt2 = new DefaultRunContext(graphDir2, "test2") {
      override val testContext: ClusterResults = cxt.testContext
    }
    dcxt2.generateResultsPage()
    assert(loadedResults == cxt.testContext)
  }
}
