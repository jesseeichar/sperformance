package sperformance
package historical

import util.FileUtils
import collection.JavaConverters._
import java.net.{URLClassLoader, URL}
import sperformance.{CSVRunContext, RunContext, PerformanceTest}
import java.io.File
import java.lang.Class
import java.util.zip._

trait BaseLine {
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext,
                           testClass:Class[T]):Unit
}

/**
 * Runs all tests in the provided packages and outputs the results as CSV output in the target directory
 * 
 * First param is path of jar
 * Second param is a : separated list of package names to search for tests
 * Third param is the output directory
 */
object RunBaseLine {
  def main(args:Array[String]) {
    require(args.length == 3)
    val outputDirectory = new File(args(2))
    val packageDirs = args(1).split(":").toList map {_.replace(".","/")}
    val file = new ZipFile(args(0))
    for {
      entry <- file.entries.asScala.toList
      if packageDirs.exists (entry.getName startsWith _) && entry.getName.endsWith(".class")
      cl = Class.forName(entry.getName)
      if classOf[PerformanceTest].isAssignableFrom(cl)
    } {
      val test = cl.newInstance.asInstanceOf[PerformanceTest]
      val context = new CSVRunContext(new File(outputDirectory,test.name)) with BaseLineContext
      test.runTest(context)
      context.writeResults()
    }
    
    
  }
}
/**
 * When mixed into a context the context is known to be a base line context and there is no need
 * to generate the baseline results if the baseline is missing
 */
trait BaseLineContext

object IgnoreBaseLine extends BaseLine {
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext,
                           testClass:Class[T]) = ()
}

case class URLBaseLine(jar:URL, testPackages:String*) extends BaseLine {
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext,
                           testClass:Class[T]):Unit = {
    val baselineDir = new File("target/sperformance/baseline")
    if(!baselineDir.exists) {
      val cache = File.createTempFile("baseline","jar")
      
      FileUtils.copy(jar,cache)
      val cmd = List(
        "java", "-cp", cache.getPath, 
        RunBaseLine.getClass.getName, 
        testPackages.mkString(":"),
        baselineDir.getPath)
      val process = new ProcessBuilder(cmd.asJava).start();
        
      process.waitFor()
      assert((baselineDir.listFiles().filter(_.getName.endsWith(".csv"))).nonEmpty, 
        "Baseline creation failed.  Check that the baseline jar can correctly execute given the command: "+(cmd mkString " "))
      
    }
  }
}