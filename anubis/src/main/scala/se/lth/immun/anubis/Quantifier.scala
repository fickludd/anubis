package se.lth.immun.anubis

import se.lth.immun.app.CLIApplication
import java.io.File

import se.lth.immun.proteomics.PeptideIon
import se.lth.immun.proteomics.Mz
import se.lth.immun.xml.XmlReader
import se.lth.immun.mzml.ghost.XMzML

import scala.actors.Actor
import scala.actors.OutputChannel
import scala.actors.Actor._

case class PeakList(frags:Seq[AnubisInput.Fragment], peaks:Seq[Peak])


object Quantifier {
	case class Quantify(xmzml:XMzML, path:File)
	case class Quantifications(xmzml:XMzML, path:File, map:Map[PeptideIon, PeakList])
	case class Progress(x:Int, total:Int)
}



class Quantifier(
		val referencePrecursors:Array[ReferencePrecursor], 
		val params:AnubisParams,
		val numWorkers:Int = 1
) extends Actor {
	
    import Quantifier._
    import ChromAnalysis._
    
    var iw = 0
    val workers = for (i <- 0 until numWorkers) yield {
    	val ca = new ChromAnalysis
    	ca.start
    }
    
    var client:Option[OutputChannel[Any]] = None
    var awaiting = PeptideIon.Set()
    var resultDict = PeptideIon.Map[PeakList]()
    var xmzml:XMzML = _
    var path:File = _
    
    def act = {
    	loop {
    		react {
    			case Quantify(xmzml:XMzML, path:File) =>
    				client = Some(sender)
    				this.xmzml = xmzml
    				this.path = path
    				for (rpc <- referencePrecursors) {
    					try {
    						xmzml.grouper.extractGroup(rpc.mz, params.q1tolerance) match {
			                	case Some(cg) => {
			                		val aIn = AnubisInput.of(cg, rpc, params)
			                		val pi = new PeptideIon(new Mz(rpc.mz, params.q1tolerance), rpc.peptideSequence)
			                		awaiting += pi
			                		workers(iw) ! Chrom(pi, aIn)
			                		iw = (iw + 1) % workers.length
			                	}
			                	case None => {
			                		CLIApplication.log.write("{ prot:"+rpc.proteinName+" pepSeq:"+rpc.peptideSequence+" m/z:"+rpc.mz+" } not found in file '"+path+"'")
			                	}
			                }
    					} catch {
			            	case e:AnubisInputException => {
			            		CLIApplication.log.write("{ prot:"+rpc.proteinName+" pepSeq:"+rpc.peptideSequence+" m/z:"+rpc.mz+" }")
			            		CLIApplication.log.write(e.getMessage())
			            	}
			            	case e:Exception => {
			            		CLIApplication.log.write("YARGH! prot:"+rpc.proteinName+" pepSeq:"+rpc.peptideSequence+" m/z:"+rpc.mz)
			            		CLIApplication.log.write(e)
			            	}
			            }
    				}
    				
    			case Peaks(pi, peakList) =>
    				if (peakList.peaks.exists(p => java.lang.Double.isNaN(p.fdr)))
    					CLIApplication.log.write("No peak detected for %s".format(pi))
    				resultDict += pi -> peakList
    				awaiting -= pi
    				processAwaiting
    					
    			case AnalysisError(pi, msg) =>
    				CLIApplication.log.write(msg)
    				awaiting -= pi
    				processAwaiting
    		}
    	}
    }
    
    def processAwaiting = 
    	if (awaiting.isEmpty)
			client.get ! Quantifications(xmzml, path, resultDict)
		else 
			client.get ! Progress(resultDict.size, awaiting.size + resultDict.size)
    
    /*
    def quantify(xmzml:XMzML, path:File):TreeMap[PeptideIon, PeakList] = {
    	var resultDict = PeptideIon.Map[PeakList]()
        var total = referencePrecursors.length
        var curr = 0.0
        implicit val uncertainty = 0.05

        updateBar(0)
        for (rpc <- referencePrecursors) {
            updateBar(curr / total)
            curr += 1.0

            try {
            	//  ============================== COMMENT THIS ======================================
		        // if (rpc.peptideSequence == "YYGYTGAFR") //FQAAAGQLEK, GISEVVFDR
		        	//print("yeah")
		        	//throw new Exception("Debugging, doh! -- comment shit in Quantifier.scala")
		        //  ============================== COMMENT THIS ======================================
                xmzml.grouper.extractGroup(rpc.mz, params.q1tolerance) match {
                	case Some(cg) => {
                		var aIn = AnubisInput.of(cg, rpc, params)
                		resultDict += new PeptideIon(Mz(rpc.mz), rpc.peptideSequence) -> PeakList(aIn.fragments, pepQuant.findBestCandidate(aIn))
                	}
                	case None => {
                		CLIApplication.log.write("{ prot:"+rpc.proteinName+" pepSeq:"+rpc.peptideSequence+" m/z:"+rpc.mz+" } not found in file '"+path+"'")
                	}
                }
            }
            catch {
            	case e:AnubisInputException => {
            		CLIApplication.log.write("{ prot:"+rpc.proteinName+" pepSeq:"+rpc.peptideSequence+" m/z:"+rpc.mz+" }")
            		CLIApplication.log.write(e.getMessage())
            	}
            	case e:Exception => {
            		CLIApplication.log.write("YARGH! prot:"+rpc.proteinName+" pepSeq:"+rpc.peptideSequence+" m/z:"+rpc.mz)
            		CLIApplication.log.write(e)
            	}
            }
        }
        updateBar(1)
        println()
        return resultDict
    }


    val BAR_LENGTH = 30
    var printedLength = 0
    var start = false
    def updateBar(percent:Double) {
    	if (percent == 0 && !start) {
    		printedLength = 0
    		print("[")
    		start = true
    	} else if (percent == 1) {
    		println("]")
    		start = false
        } else {
    		var l = (percent * BAR_LENGTH).toInt
    		if (l > printedLength) {
    			printedLength = l
    			print("=")
    		}
    	}
        /*println("\r"
            + "|" + "".padTo((percent * BAR_LENGTH).toInt, '=').padTo(BAR_LENGTH, ' ')
            + "|" + "".padTo(4 - ("" + ((percent * 100).toInt)).length, ' ') 
            + (percent * 100).toInt + "%")*/
    }
    */
}