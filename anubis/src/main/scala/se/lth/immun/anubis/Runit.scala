package se.lth.immun.anubis

import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.InputStreamReader
import java.io.IOException
import java.util.zip.GZIPInputStream
import java.util.Properties
import java.util.Date
import java.text.DateFormat

import se.lth.immun.files.Glob
import se.lth.immun.app.CLIApplication
import se.lth.immun.app.CommandlineArgumentException
import se.lth.immun.app.LogFile
import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.mzml.MzML
import se.lth.immun.mzml.ghost.XMzML
import se.lth.immun.proteomics.PeptideIon
import se.lth.immun.proteomics.Mz

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import scala.actors.Actor
import scala.actors.OutputChannel
import scala.actors.Actor._

import org.apache.commons.math3.stat.StatUtils


object Runit {
	
	case class RunAnubis(
			props:Properties,
    		refPath:File,
    		refFile:ReferenceFile,
    		outputPath:String,
    		resultFile:ResultFile,
    		mzmlPaths:Seq[File],
    		qCutoff:Double,
    		params:AnubisParams
    	)
    
	case class AlreadyRunning()
	case class Msg(msg:String)
	case class AnubisDone(ra:RunAnubis)
}


class Runit extends Actor {
	    
    var now:Long = System.currentTimeMillis
    
    import Runit._
	import Quantifier._
    
	var client:OutputChannel[Any] = _
	var run:RunAnubis = _
    var quantifier:Option[Quantifier] = None
    val fileQueue = new HashSet[File]
    var dict = PeptideIon.Map[ArrayBuffer[PeakList]]()
    val files = new ArrayBuffer[ResultMzMLFile]
    val bar = new CLIBar(30, true)
    
    def act = 
    	loop {
    		react {
    			case ra:RunAnubis =>
			    	now = System.currentTimeMillis
			    	
			    	if (fileQueue.nonEmpty)
			    		sender ! AlreadyRunning()
    	    		else
    	    			anubis(ra)
    				
    	    	
    	    	case Progress(x, total) =>
    				client ! Msg(bar.update(x, total))
    			
    			case Quantifications(xmzml, path, map) =>
    				files += new ResultMzMLFile(path, xmzml.runTime)
    				val preDict:Map[PeptideIon, PeakList] = map
    				for (kvp <- preDict) {
			            if (!dict.contains(kvp._1)) dict += kvp._1 -> new ArrayBuffer[PeakList]
			            dict(kvp._1) += kvp._2
			        }
    				
    				fileQueue -= path
    				client ! Msg(bar.update(1.0))
			    	client ! Msg(timerMark("analysis time") + "\n\n")
    				
    				if (fileQueue.nonEmpty)
    					startQuantification(fileQueue.head)
    				else 
    					wrapUpAnubis
    		}
    	}
    
    
    
    def anubis(ra:RunAnubis) = {
    	
    	run = ra
    	client = sender
    	
    	setupResultFile(ra)
		client ! Msg(makeInitString(ra))
		
		quantifier = Some(new Quantifier(ra.refFile.precursors, ra.params))
    	quantifier.get.start()
		
    	dict = PeptideIon.Map[ArrayBuffer[PeakList]]()
    	files.clear
		fileQueue ++= ra.mzmlPaths
		
		startQuantification(ra.mzmlPaths.head)
    }
    
    
    
	def startQuantification(mzmlPath:File) = {
    	
		try {
        	val xMzML = parseFile(mzmlPath)
	    	bar.reset
        	client ! Msg("Quantifying file: " + mzmlPath + "\n")
	    	client ! Msg(bar.update(0.0))
        	quantifier.get ! Quantify(xMzML, mzmlPath)
        	
        } catch {
    		case e:IOException => {
    			client ! Msg("Failed to analyze file '"+mzmlPath.getName+"'\n"+e.getMessage)
    		}
        }
	}
    
	
	
	def wrapUpAnubis() = {
        run.resultFile.replicateFiles = files.toArray
        
        client ! Msg(timerMark("analysis time") + "\n")
	    
	    val outputXMLFile = new File(run.outputPath + ".res.xml")
		val outputCSVFile = new File(run.outputPath + ".res.csv")
        val mzqFileName = run.outputPath+".res.mzq"
	    
	    compileResultFile(run, dict)
	    run.resultFile.write(new XmlWriter(new BufferedWriter(new FileWriter(outputXMLFile))))
	    run.resultFile.writeMzq(mzqFileName)	
		client ! Msg("Converting results from xml to csv format...\n")
		client ! Msg("  result xml file: " + outputXMLFile + "\n")
		client ! Msg("  result csv file: " + outputCSVFile + "\n")
		client ! Msg("  result mzq file: " + mzqFileName + "\n")
	    
		val ara = new PostAnubisAnalysis(	run.resultFile,	_.lfdr <= run.qCutoff)
		val csvWriter = new AnubisCSVWriter(ara, new FileWriter(outputCSVFile), run.qCutoff)
		csvWriter.write(outputCSVFile.toString)
		
		client ! Msg("done\n")
	    client ! AnubisDone(run)
	    
	    client = null
	    run = null
	    exit()
    }
	
	    
    
    def parseFile(file:File):XMzML = {
    	val fileName = file.getName
    	if (fileName.toLowerCase.endsWith(".mzml.gz"))
        	try {
        		return XMzML.fromFile(
        				new XmlReader(
        					new BufferedReader(new InputStreamReader(
		    					new GZIPInputStream(new FileInputStream(file))
		    				))
        				))
        	} catch {
        		case e:Exception => 
        			throw new IOException("Failed parsing gzipped file '"+file+
        									"': "+e.getLocalizedMessage)
        	}
        else if (fileName.toLowerCase.endsWith(".mzml"))
        	try {
        		return XMzML.fromFile(new XmlReader(new BufferedReader(new FileReader(file))))
        	} catch {
        		case e:Exception => {
        			e.printStackTrace
        			throw new IOException("Failed parsing file '"+file+
        									"': "+e.getLocalizedMessage)
        		}
        	}
        else
        	throw new IOException("Unsupported file extension. 'mzML' or "+
        							"'.mzML.gz' required")
    }
    
    
    
    def compileResultFile(
    		ra:RunAnubis, 
    		resultHashMap:Map[PeptideIon, ArrayBuffer[PeakList]]
	) {
        var proteins = PeptideIon.Map[String]()
        for (rpc <- ra.refFile.precursors) 
        	proteins += new PeptideIon(new Mz(rpc.mz, ra.params.q1tolerance), rpc.peptideSequence) -> rpc.proteinName

        for ((pepIon, replicateList) <- resultHashMap) {
            val rpc 			= new ResultPrecursor()
            rpc.mz 				= pepIon.mz.d
            rpc.proteinName 	= proteins(pepIon)
            rpc.peptideSequence = pepIon.sequence
            for (i <- 0 until replicateList.length) {
            	val peakList = replicateList(i)
                for (peak <- peakList.peaks) {
	                val rr 			= new ResultReplicate()
	                rr.fileId 		= i
	                rr.totalArea 	= peak.area
	                rr.peakBeforeWindowFlag = peak.peakBeforeWindowFlag
	                rr.peakAfterWindowFlag 	= peak.peakAfterWindowFlag
	                for (t <- 0 until peak.areas.length) {
	                    val frag = peakList.frags(t)
	                    rr.transitions = rr.transitions :+ 
	                    	new ResultTransition(frag.mz, frag.ion, peak.areas(t)) 
	                }
	
	                rr.quality = new ResultQuality(peak.fdr, peak.nOConditions)
	                rr.retentionTime = new ResultRetentionTime(peak.start, peak.peak, peak.end)
	                
	                rpc.replicates = rpc.replicates :+ rr
                }
            }
            ra.resultFile.precursors = ra.resultFile.precursors :+ rpc
        }
    }
    
    
    def setupResultFile(ra:RunAnubis) = {
    	val x = ra.resultFile
    	val p = ra.params
    	
    	x.anubisVersion 		= ra.props.getProperty("pom.version")
    	x.workingDir 			= new File("")
    	x.referenceFile 		= ra.refPath
    	x.peakMinWidth 			= p.minPeakWidth
    	x.nullDistributionSize 	= p.nullDistributionSize
    	x.transitionLimit 		= p.transitionLimit
    	x.singleAnswer 			= p.singleAnswer
    	x.pValueTolerance 		= p.pValueTolerance
    }
    
    
    def makeInitString(ra:RunAnubis) = {
    	val name = ra.props.getProperty("pom.name")
    	val p = ra.params
    	val sb = new StringBuilder
    	sb ++= "\n"
    	sb ++= "Commencing "+name+" - v"+ra.resultFile.anubisVersion + "\n"
    	sb ++= "   max heap size: "+Runtime.getRuntime.maxMemory + "\n"
    	sb ++= "\n"
    	sb ++= "  reference file: "+ra.refPath + "\n"
    	sb ++= "  peak min width: "+p.minPeakWidth + " min\n"
    	sb ++= "  null dist size: "+p.nullDistributionSize + "\n"
    	sb ++= "    #trans limit: "+p.transitionLimit + "\n"
    	sb ++= "   single answer: "+p.singleAnswer + "\n"
    	sb ++= "     p-value tol: "+p.pValueTolerance + "\n"
    	sb ++= "          q1 tol: "+p.q1tolerance + "\n"
    	sb ++= "          q3 tol: "+p.q3tolerance + "\n"
    	sb ++= "     output file: "+ra.outputPath + "\n"
    	sb ++= "      data files:\n"
    	ra.mzmlPaths.foreach(f => sb ++= "    "+f+"\n")
    	sb ++= "\n"
    	sb.result
    }
    
    
    
    def makeFinishString(results:Map[PeptideIon, ArrayBuffer[PeakList]]) = {
    	val peaks 			= results.values.flatten.flatMap(_.peaks)
	    val foundPeaks 		= peaks.filter(p => !p.isMissing)
	    val nPeaksFound 	= foundPeaks.size
	    val medianFDR 		= StatUtils.percentile(foundPeaks.map(_.fdr).toArray, 50)
	    val meanFDR 		= StatUtils.mean(foundPeaks.map(_.fdr).toArray)
    	
	    val sb = new StringBuilder
	    sb ++= "             # found peaks: "+nPeaksFound+" / "+peaks.size + "\n"
    	sb ++= "found peaks median q value: "+medianFDR + "\n"
    	sb ++= "  found peaks mean q value: "+meanFDR + "\n"
    	sb ++= "\n"
    	sb.result
    }
    
    
    
    def padLeft(i:Long, l:Int):String = {
    	val s = i.toString
    	return "".padTo(l-s.length, '0') + s
    }
    
    
    
    def formatTime(timeStamp:Long):String = {
    	val ms = timeStamp % 1000
    	var dt = timeStamp / 1000
    	val s = dt % 60
    	dt = dt / 60
    	val m = dt % 60
    	dt = dt / 60
    	val h = dt % 24
    	val days = dt / 24
    	return if (days > 0) { days + " days, "} else {""} + 
    			padLeft(h, 2) + ":" + padLeft(m, 2) + ":" + padLeft(s, 2) + "." + ms
    }
    
    
    
    def timerMark(title:String) = {
        val str = title + ": " + formatTime(System.currentTimeMillis - now)
        now = System.currentTimeMillis
        str
    }
}
