package sperformance
package historical

import scala.util.control.Exception._
import util.FileUtils
import collection.JavaConverters._
import java.net.{URLClassLoader, URL}
import sperformance.{RunContext, PerformanceTest}
import java.io.File
import java.lang.Class
import java.util.zip._
import store.XmlStoreResults
import store.XmlLoadResults
import sperformance.intelligence.ClusterResults

/**
 * The trait for BaseLine implementations.  The concept is that when a test is ran a baseline will be generated for the current
 * system and that will be compared to version 1.  If they differ then a ratio is calculated which is used to relativize the values 
 * of the test. This allows future test to be done on other computers still have the values provide meaning to the other tests done
 * on previous versions of the system.
 * 
 * The Typical implementation will be to download a Jar which contains the test and all dependencies as they were to generate version 1
 * and execute the test using that jar.  The results can then be compared to version one.
 */
abstract class BaseLine {
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext, test:T):Unit
  def relativize[T <: PerformanceTest](runContext:RunContext, test:T):PerformanceTestRunContext
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
    require(args.length == 3, "not Enough parameters.  Expected 3 parameters(jarfile,packages,outputDir) but only have: "+args.mkString)
    
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
        val outFile: File = new File(outputDirectory, test.name + ".xml")
        val context = new HistoricalRunContext(outFile, false, new XmlStoreResults(_)) with BaseLineContext
        test.runTest(context)
        context.writeBaseline(test.name)
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
  def relativize[T <: PerformanceTest](runContext:RunContext, test:T) = 
    runContext.testContext
}
object JarBaseLine {
  def apply(jarURL:URL, allTestPackages:String*) = new JarBaseLine{
    val jar = jarURL
    val testPackages = allTestPackages
  }
}
trait JarBaseLine extends BaseLine {
  def jar:URL
    val historyDir = new File("target/sperformance/history")
    val baselineDir = new File(historyDir,"baseline")

  def testPackages:Traversable[String]
  def relativize[T <: PerformanceTest](runContext:RunContext, test:T):PerformanceTestRunContext = {
    val baselineFile = new File(baselineDir, test.name+".xml")
    val loadedResults = new XmlLoadResults(baselineFile.toURI.toURL).read()

    var relativizedResults = new ClusterResults()
    
    val currentResults = runContext.testContext.asInstanceOf[ClusterResults]
    for {
      (currentMd, currentResult) <- currentResults.clusters
      loadedResult = loadedResults.clusters.get(currentMd)
    } {
      if(loadedResult.isEmpty) {
        println("Warning no result for "+currentMd+" in baseline")
        currentResult.results.foreach{relativizedResults.reportResult}
      } else {
        
      }
    }
    relativizedResults
  }
  def ensureBaseLineExists[T <: PerformanceTest](runContext:RunContext, test:T):Unit = {
    // dirs
    val requiredFile = new File(baselineDir, test.name+".xml")
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
      val version1Data = new File(historyDir,"version1")
      if(!version1Data.exists) {
         org.apache.commons.io.FileUtils.copyFileToDirectory(version1Data, historyDir)
      }
    }
  }
}