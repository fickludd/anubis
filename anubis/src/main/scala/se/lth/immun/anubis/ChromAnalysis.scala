package se.lth.immun.anubis

import scala.actors.Actor
import scala.actors.Actor._


import se.lth.immun.proteomics.PeptideIon

object ChromAnalysis {
	case class Chrom(pi:PeptideIon, aIn:AnubisInput)
	case class Peaks(pi:PeptideIon, peakList:PeakList)
	case class AnalysisError(pi:PeptideIon, msg:String)
}


class ChromAnalysis extends Actor {
	
    val pepQuant:PepQuant = new PepQuant()

	import ChromAnalysis._
	
	def act = {
		loop {
			react {
				case Chrom(pi, aIn) =>
					try {
						sender ! Peaks(pi, PeakList(aIn.fragments, pepQuant.findBestCandidate(aIn)))
					} catch {
						case e:Exception =>
							sender ! AnalysisError(pi, "Exception encountered during analysis of %s\n%s".format(pi, e.getStackTrace.mkString("\n")))
					}
			}
		}
	}
}