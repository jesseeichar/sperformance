package sperformance

import collection.JavaConverters._
import charting.Charting
import intelligence.Cluster
import org.jfree.chart.{ChartUtilities, JFreeChart}
import collection.mutable.ListBuffer
import java.io.{FileOutputStream, BufferedOutputStream, PrintStream, File}
import store.StoreResultStrategy
import util.FileUtils
import java.net.URL
import org.apache.commons.io.{FileUtils => AFileUtils}

/**
 * Abstract interface designed to allow customize where reports go and how they are generated. (i.e. could be sent to a Swing UI).
 *
 * This interface is by no means complete.
 */
trait RunContext {
  /** The context to use when running tests */
  def testContext : PerformanceTestRunContext
  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) : Unit
}


//This just dumps charts into output directories...
class DefaultRunContext(val outputDirectory : File, testName : String) extends RunContext {

  val defaultChartHeight = 600
  val defaultChartWidth = 800

  //Funny how all our intelligence is embedded here for generating clusters...
  override val testContext = new intelligence.ClusterResults 

  case class Chart(clusterName : List[String], chartName : String, chartFile: File)

  private val charts = new ListBuffer[Chart]

  def getResultingFile(clusterName : List[String], chartName : String) : File = {
     def relativeFilename = clusterName.mkString(File.separator) + File.separator + chartName + ".png"
    new File(outputDirectory,relativeFilename)
  }

  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) {
    val file = getResultingFile(clusterName, chartName)
    //Ensure
    if(!file.exists) {
      val parent = file.getParentFile
      if(!parent.exists) {
        parent.mkdirs
      }
    }
    ChartUtilities.saveChartAsPNG(file, chart,defaultChartWidth, defaultChartHeight)

    //TODO - Write information to cache...
    charts += Chart(clusterName, chartName, file)
  }

  import scala.xml._
  def resultsPage : Node = (<html>
    <head><title>{testName} Results</title></head>
    <style type="text/css">{
""".cluster h1 {
      text-align: center;
    }
    .clusterResults {
     width: 90%;
    }
    .chart {
      float: left;
      clear: none;
      width: 50%;
      text-align: center;
    }"""
         }</style>
    <body>
      {
         for {
           (cluster, charts) <- charts.groupBy( c => c.clusterName)
         } yield <div class="cluster">
           <h1>Graphed By {cluster}</h1>
           <div class="attributes">
             {
                //TODO - Output Cluster attributes that lead to this chart?
             }
           </div>
           <div class="clusterResults">
             {
               for(chart <- charts) yield <div class="chart">
                   <img src={util.FileUtils.relativePath(outputDirectory, chart.chartFile)} alt={"Image - " + chart.chartName}/>
               </div>
             }
           </div>
        </div>
      }
    </body>
  </html>)


  def generateResultsPage() {
    //TODO - more cleverness about how we make charts?
    Charting.createReports(testContext.clusters, this)

    // order is important resultsPage depends on charts the fact that charts
    // have already been created
    val content = resultsPage
    val index = new File(outputDirectory, "index.html")
    FileUtils.ensureDirectoryExists(index)
    val output = new PrintStream(new BufferedOutputStream(new FileOutputStream(index)))
    try {
      output.println(content)
    }  finally {
      output.close()
    }
  }
}

/**
 * Generates no charts, instead this writes the results to a file so that tests can be tracked
 * and charted over time.  The format of the file is determined by the [[sperformance.store.StoreResultStrategy]] object
 * passed to writeResults
 * 
 * @param historyDir The directory containing the history of performance tests
 * @param newVersion if true then a new version will be create if false then 
 * 				 	 the latest version will be overridden
 * @param factory    The factory function for creating a StoreResultStrategy 
 */
class HistoricalRunContext(historyDir:File, newVersion:Boolean, factory:File => StoreResultStrategy) extends RunContext {

  val baselineDir = new File(historyDir, "baseline")
  val versionsDir = new File(historyDir, "versions")
  val testContext = new intelligence.ClusterResults
  val currentVersionDir ={
    val version = if(versionsDir.list == null) {
      1
    } else {
      val lastestVersion = versionsDir.list.toList.maxBy(_.toInt)
	  lastestVersion.toInt + (if(newVersion) 1 else 0)
    }
    new File(versionsDir, version toString)
  }

  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) : Unit = {
    // this doesn't write charts
  }

  /**
   * Write out the data to the baseline directory
   */
  def writeBaseline(testName:String) = {
    val strategy = factory(new File(baselineDir,testName))
    strategy.write(testContext)
  }

  /**
   * Write out a version
   */
  def writeVersion(testName:String) = {
    val strategy = factory(new File(currentVersionDir,testName))
    strategy.write(testContext)
  }

}
