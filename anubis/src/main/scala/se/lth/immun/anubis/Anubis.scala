package se.lth.immun.anubis

import swing._
import swing.event._ 


import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.util.Properties
import se.lth.immun.files.Glob
import se.lth.immun.xml.XmlReader
import se.lth.immun.app.CommandlineArgumentException
import se.lth.immun.app.CLIApplication

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.Channel
import scala.concurrent.duration.Duration
import scala.concurrent.duration.Duration._

object Anubis extends SimpleSwingApplication with CLIApplication {
	    
    var gui:AnubisGUI = null
    val runit = new Runit
    runit.start
    
	override def main(args:Array[String]):Unit = {
    	
    	var rFile:File 						= null
    	var peakMinWidth 					= 0.1
    	var outputFile:File 				= null
    	var rawFiles:Seq[File] 				= Nil
    	var referenceFile:ReferenceFile 	= null
    	var qCutoff							= 0.01
    	var nullDistributionSize			= 500
    	var transitionLimit					= 6
    	var singleAnswer					= true
    	var q1Tolerance						= 0.7
    	var q3Tolerance						= 0.7
    	var startGUI						= true
    	var noSanityCheck					= false
    	var noEstimatedQuantities			= false
    	var resultFile 						= new ResultFile
    	var properties 						= new Properties
    	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
    	
    	opt("no-GUI", "Don't reject peak candidates where a transition is lower than expected",
    			s => startGUI = false)
    	
    	opt("single-answer", "if true, report multiple peaks per chromatogram (within p-value-tolerance of best), if false report only the best (default: true)",
    			s => singleAnswer = s.toBoolean, "X")
    	
    	opt("q-cutoff", "the q-value below which detections are considered successful (default: 0.01)",
    			s => qCutoff = s.toDouble, "X")
    	
    	opt("q1-tolerance", "allowed difference between reference and mzML precursor m/z (>0, default: 0.7)",
    			s => q1Tolerance = s.toDouble, "X")
    	
    	opt("q3-tolerance", "allowed difference between reference and mzML fragment m/z (>0, default: 0.7)",
    			s => q3Tolerance = s.toDouble, "X")
    	
    	opt("null-dist", "size of null distrubution for each chromatogram (default: 500)",
    			s => nullDistributionSize = s.toInt, "X")
    	
    	opt("trans-limit", "limit on the number of transitions used (default: 6)",
    			s => transitionLimit = s.toInt, "X")
    	
    	opt("no-sanity-check", "Don't reject peak candidates where a transition is lower than expected",
    			s => noSanityCheck = true)
    	
    	opt("no-estimated-quantities", "Report only measured quantities, no estimation.",
    			s => noEstimatedQuantities = true)
    			
    	opt("peak-min-width", "The shorted peak width considered (default: 0.1min)",
    			s => {
    			resultFile.parameters = resultFile.parameters :+ 
    					new ResultParameter("peak-min-width", s)
    			peakMinWidth = s.toDouble
    		}, "X")
    	opt("output-file", "set the output file (default: first input file w extension swapped to .anubis",
    			s => {
    			resultFile.parameters = resultFile.parameters :+ 
    					new ResultParameter("OUTPUT_FILE", s)
    			outputFile = new File(s)
    		}, "X")
    			
    	arg("REFERENCE_FILE", s => {
    			resultFile.parameters = resultFile.parameters :+ 
    					new ResultParameter("REFERENCE_FILE", s)
	    		rFile = new File(s)
	    		referenceFile = ReferenceFile.fromFile(new XmlReader(new FileReader(rFile)))
	    	})
    	arg("MZML_FILE", f => {
    			resultFile.parameters = resultFile.parameters :+ new ResultParameter("MZML_FILE", f)
    			rawFiles = Glob.resolveGlob(new File(f))
    		}, false)
    	rest("MZML_FILES", rest => {
    			resultFile.parameters = resultFile.parameters ++ 
    					rest.map(s => new ResultParameter("MZML_FILE", s))
    			rawFiles = rawFiles ++ rest.toSeq.map(path => Glob.resolveGlob(new File(path))).flatten
    		}, false)
    	try {
    		parseArgs("anubis", args)
    		
    		if (outputFile == null) {
	    		var f = rawFiles.head.toString
	    		if (f.toLowerCase.endsWith(".mzml.gz"))
	    			outputFile = new File(f.dropRight(8) + ".anubis")
	    		else if (f.toLowerCase.endsWith(".mzml"))
	    			outputFile = new File(f.dropRight(5) + ".anubis")
	    		else
	    			outputFile = new File(f + ".anubis")
	    	}
    		
    		import Runit._
    		val channel = new Channel[Any]
    		
    		runit.send(RunAnubis(
    				properties,
    				rFile,
    				referenceFile,
    				outputFile.toString,
    				resultFile,
    				rawFiles,
    				qCutoff,
    				new AnubisParams(
    						peakMinWidth, 
    						nullDistributionSize, 
    						transitionLimit,
    						true,
    						0.01,
    						noSanityCheck,
    						noEstimatedQuantities
    				)
    			), Actor.self)
    		
    		/*
    		var done = false
    		while (!done) 
	    		channel.reactWithin(1 Duration.) {
					case x:String =>
	    				print(x)
	    				
					case Msg(x) =>
	    				print(x)
	    				
	    			case AnubisDone(ra) =>
	    				done = true
	    				//throw new Exception("Anubis done!")
	    				
				}
    			
    		*/
    		try {
    			Actor.loop {
	    			react {
						case x:String =>
		    				print(x)
		    				
						case Msg(x) =>
		    				print(x)
		    				
		    			case AnubisDone(ra) =>
		    				System.exit(0)
		    				//throw new Exception("Anubis done!")
		    				
					}
	    		}
    		} catch {
    			case e:java.lang.InterruptedException => println("this is fine...")
    			case e:Exception => e.printStackTrace
    			case t:Throwable => println(t)
    		}
    		return 
    	} catch {
    		case cae:CommandlineArgumentException => 
    			if (cae.getMessage == "Not enough arguments!" && startGUI)
    				gui = new AnubisGUI(properties)
    			else return
    	}
    	
    	super.main(args)
    }
    
    
    
    def top = gui
}