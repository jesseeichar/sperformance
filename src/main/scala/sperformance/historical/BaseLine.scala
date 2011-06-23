package sperformance
package historical

import scala.util.control.Exception._
import util.FileUtils
import collection.JavaConverters._
import java.net.{URLClassLoader, URL}
import sperformance.{CSVRunContext, RunContext, PerformanceTest}
import java.io.File
import java.lang.Class
import java.util.zip._

trait BaseLine {
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext, test:T):Unit
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
    require(args.length == 3, "not Enough parameters.  Expected 3 parameters but only have: "+args.mkString)
    
    val outputDirectory = new File(args(2))
    val packageDirs = args(1).split(":").toList map {_.replace(".","/")}
    val file = new ZipFile(args(0))

    val catcher = catching(classOf[Throwable])
    
    println("Searching for tests in the following directories: "+packageDirs.mkString(", "))
    for {
      entry <- file.entries.asScala.toList
      if packageDirs.exists (entry.getName startsWith _) && entry.getName.endsWith(".class")
      clPath = entry.getName.dropRight(".class".size)
      className = clPath.replace("/",".")
      cl = Class.forName(className)
      if classOf[PerformanceTest].isAssignableFrom(cl)
    } {
      println("Found test Class: "+cl.getName)
      val test = 
        try {
          Some(cl.newInstance.asInstanceOf[PerformanceTest])
        } catch {
          case _:IllegalAccessException if(className.endsWith("$")) =>
            catcher.opt {
              val cons = cl.getDeclaredConstructors(); 
              cons(0).setAccessible(true);
              cons(0).newInstance().asInstanceOf[PerformanceTest]
             }
        }
        
      test.foreach {test => 
        println("Running Test: "+test.name)
        val context = new CSVRunContext(new File(outputDirectory,test.name+".csv")) with BaseLineContext
        test.runTest(context)
        context.writeResults()
        println(test.name+" is complete")
      }
    }
    file.close()
  }
}

/**
 * When mixed into a context the context is known to be a base line context and there is no need
 * to generate the baseline results if the baseline is missing
 */
trait BaseLineContext

object IgnoreBaseLine extends BaseLine {
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext, test:T) = ()
}
object JarBaseLine {
  def apply(jarURL:URL, allTestPackages:String*) = new JarBaseLine{
    val jar = jarURL
    val testPackages = allTestPackages
  }
}
trait JarBaseLine extends BaseLine {
  def jar:URL
  def testPackages:Traversable[String]
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext, test:T):Unit = {
    val baselineDir = new File("target/sperformance/baseline")
    val requiredFile = new File(baselineDir, test.name+".csv")
    if(!(runContext.isInstanceOf[BaseLineContext]) && !baselineDir.exists) {
      println("Baseline does not exist and will be created using the "+JarBaseLine+" jar")
      val cache = File.createTempFile("baseline","jar")
      
      FileUtils.copy(jar,cache)
      val cmd = List(
        "java", "-cp", cache.getPath, 
        RunBaseLine.getClass.getName.filterNot(_ == '$'), 
        cache.getPath,
        testPackages.mkString(":"),
        baselineDir.getPath)

      val process = new ProcessBuilder(cmd.asJava).start();
        
      process.waitFor()
      println("Baseline has been created")

      assert(requiredFile.exists, 
        "Baseline creation failed.  Check that the baseline jar can correctly execute given the command: "+(cmd mkString " "))
      
    }
  }
}