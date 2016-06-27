package se.lth.immun.anubis

import java.io.File
import java.io.FileReader
import java.io.BufferedReader
import scala.collection.mutable.ArrayBuffer

import se.lth.immun.xml.XmlReader

import se.lth.immun.math.Ratios
import se.lth.immun.mzml.ghost.XMzML
import se.lth.immun.mzml.ghost.XChromatogramGroup
import se.lth.immun.signal.SummedMaxPeakFinder
import se.lth.immun.signal.Filter

import org.apache.commons.math3.stat.StatUtils

object ReferenceCalculator {
	
	val peakFinder = new SummedMaxPeakFinder(0.2)
	
	def calculate(rf:ReferenceFile, handlePeptideUpdate:ReferencePrecursor => Unit):Unit = {
        for (fi <- 0 until rf.files.length) {
            var fn = rf.files(fi)
            var df:XMzML = XMzML.fromFile(new XmlReader(new BufferedReader(new FileReader(fn))))
            
            for (pc <- rf.precursors.filter(_.fileId == fi)) {
            	try {
            		if (fill(pc, df))
            			pc.fileId = fi
	                
	                handlePeptideUpdate(pc)
            	} catch {
            		case _:Throwable => {}
            	}
            }
        }
	}
	
	
	
	def extractTransitionData(pc:ReferencePrecursor, xmzml:XMzML) = {
		xmzml.grouper.extractGroup(pc.mz, 0.001) match {
			case Some(cg) => {
				var all = pc.allTransitions.toArray
				Some(cg.filter(all.map(_.mz), true, 0.05).resample())
			}
			case None =>
				None
		}
	}
	
	
	
	def fill(pc:ReferencePrecursor, xmzml:XMzML):Boolean = {
		// clear reference precursor and select correct transitions
		var all = pc.allTransitions
		var foundTDs = extractTransitionData(pc, xmzml) match {
			case Some(cg) => cg
			case None => return false
		}
        pc.clearTransitions()
		
		
		// update measured / ignored transitions
        var meas 	= new ArrayBuffer[ReferenceTransition]
        var ign 	= new ArrayBuffer[ReferenceTransition]
        
        for (t <- all)
            foundTDs.chromatograms.find(td => Math.abs(td.q3 - t.mz) < 0.001) match {
            	case Some(td) => {
            		t.dataName = td.toString
            		meas += t
            	}
            	case None => ign += t
            }
        
        pc.measuredTransitions 	= meas
        pc.ignoredTransitions 	= ign

        
        var measuredTDs = foundTDs.chromatograms.filter(_ != null)
        if (measuredTDs.length > 1) {
        	var precursorSignal: Array[Array[Double]] = measuredTDs.map(_.intensities.toArray).toArray
     
            var peak = peakFinder.findPeak(precursorSignal)
            
            var peakSignal = precursorSignal.map(f => slice(f, peak.start, peak.end, 4).toArray)
            fillRatios(pc, peakSignal)

        	var times = measuredTDs(0).times
            pc.retentionTime.start = times(peak.start)
            pc.retentionTime.peak = times(peak.peak)
            pc.retentionTime.end = times(math.min(peak.end, times.length-1))
        }
        return measuredTDs.length > 0
	}
	
	
	
	def fillRatios(pc:ReferencePrecursor, peakSignal:Array[Array[Double]]) = {
		var filtered = peakSignal.map(f => Filter.savitzkyGolay9(f))
        var ps = filtered.map(f => f.slice(4, f.length - 4))

        var peakRatios = new Ratios(ps)

        var ratios = new Array[Ratio](peakRatios.length)
        for (i <- 0 until peakRatios.length) {
            var ri = peakRatios.getRatioIndex(i)
            var ratio = peakRatios.getRatio(i)filter(d => !d.isInfinity && d != Double.NaN)
            ratios(i) = new Ratio(ri.up, ri.down, StatUtils.mean(ratio), math.sqrt(StatUtils.populationVariance(ratio)))
        }
		pc.ratios = ratios
	}
	
	
	
	def slice[T](s:Seq[T], start:Int, end:Int, margin:Int):Seq[T] = {
		var prefix = if (start < margin) margin - start else 0
		var suffix = if (s.length - end < margin) margin - (s.length - end) else 0
		return Nil.padTo(prefix, s(0)) ++ s.slice(start - margin, end + margin).padTo(suffix, s(s.length - 1))
	}
}
