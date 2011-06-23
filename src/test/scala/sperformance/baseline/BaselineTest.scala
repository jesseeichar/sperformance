package sperformance
package baseline

import historical.JarBaseLine
object TestBaseline extends JarBaseLine {
  def jar = getClass.getClassLoader.getResource("test-baseline.jar")
  val testPackages = List(getClass.getPackage.getName)
}
object BaselineTest extends sperformance.dsl.PerformanceDSLTest {
  
  override val baseLine = TestBaseline
  
  performance of "Strict Range" in {
    measure method "foreach" in {
      withSize upTo 2000 by 50 withSetup {
        size =>
          1 to size
      } run {
        case range =>
          range.foreach { r => r }
      }
    }
  }
  
  def main(args:Array[String]) {
    BaselineTest.runTest(new CSVRunContext(new java.io.File("target/sperformance/TestBaseline.csv")))
  }
}