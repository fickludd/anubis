package se.lth.immun.anubis

import se.lth.immun.signal.PartMaxPeakFinder

class Peak(
		pc:PeakCandidate,
		times:Array[Double],
		nOAreas:Int = 0
) {
	def this(nOAreas:Int) = this(null, null, nOAreas)
    
	var start					:Double 		= Double.NaN
	var peak					:Double 		= Double.NaN
	var end						:Double 		= Double.NaN
	var areas					:Seq[Double] 	= Array[Double]().padTo(nOAreas, 0.0)
	var maxIntensity			:Double			= Double.NaN
	var fdr						:Double 		= Double.NaN
	var nOConditions			:Int 			= 0
	var peakBeforeWindowFlag	:Boolean 		= false
	var peakAfterWindowFlag		:Boolean 		= false
		
    if (pc != null && times != null) {
        if (pc.areas.length == nOAreas) areas = pc.areas
        fdr = if (pc.fdr < 0 || pc.fdr > 1) Double.NaN else pc.fdr
        nOConditions = pc.conditions.length
        
        if (pc.start < times.length) {
        	start = times(pc.start)
        	end = times(pc.end)

	        var pf = new PartMaxPeakFinder(0.5)
	        var hmPeak = pf.findPeak(pc.estimates)
	        peak = times(hmPeak.peak + pc.start)
	        maxIntensity = pc.estimates.map(_(hmPeak.peak)).max
	        
	        if (pc.start == 0 && hmPeak.peak < (pc.end - pc.start)/2.0)
	        	peakBeforeWindowFlag = true
	        if (pc.end == times.length && hmPeak.peak > (pc.end - pc.start)/2.0)
	        	peakAfterWindowFlag = true
        }
    }

	def isMissing = java.lang.Double.isNaN(fdr)
	def area = areas.length match { 
    	case 0 => -1
    	case _ => areas.sum
    }

    override def toString:String = "[PEAK: area="+area+" FDR="+fdr+"] " + start + " - " + end;
}