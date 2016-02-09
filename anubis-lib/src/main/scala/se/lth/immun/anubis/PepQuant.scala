package se.lth.immun.anubis

import se.lth.immun.signal.Filter
import se.lth.immun.math.Ratios
import scala.collection.mutable.ArrayBuffer


object PepQuant {
    val NULL_PEAK_CANDIDATE:PeakCandidate = new PeakCandidate
}

class PepQuant() {
	
    //val NO_FAKES = 1000
	import PepQuant._
    
	var ratios			:Ratios					= null
	var filtered		:Array[Array[Double]] 	= Array()
	var uncheckedPcs	:Seq[PeakCandidate] 	= Nil
	var sanePcs									= new ArrayBuffer[PeakCandidate]
	var fakeData		:FakeData 				= null
	var bestPCs			:Seq[PeakCandidate]		= Nil
    var best			:Seq[PeakCandidate] 	= null
    
    var pcf:GraphPeakCandidateFinder 	= null
    var aIn:AnubisInput 				= null
    var refWidth:Int 					= Int.MaxValue
    
    
    var last = 0L
    var t0 = 0L
    var t1 = 0L
    var t2 = 0L
    var t3 = 0L
    var t4 = 0L
    var t5 = 0L
    var t6 = 0L
    var earlyExit = false
    
    def findBestCandidate(ai:AnubisInput):Seq[Peak] = {
		last = System.currentTimeMillis
	    t0 = 0L
	    t1 = 0L
	    t2 = 0L
	    t3 = 0L
	    t4 = 0L
	    t5 = 0L
	    t6 = 0L
		
		best 	= Nil
		aIn 	= ai
		ratios 	= null

        var reduced = Filter.baseLineReduce(
        					aIn.cg.chromatograms.toArray.map(tr => tr.intensities.toArray)
        				)
        filtered 	= reduced.map(ds => Filter.savitzkyGolay9(ds))
        ratios 		= new Ratios(filtered)
        var times 	= aIn.cg.chromatograms(0).times
        var dt 		= (times(times.length - 1) - times(0)) / times.length
        refWidth 	= Math.max(2, Math.floor(aIn.params.minPeakWidth / dt).toInt)
        
        
    	//  ============================== COMMENT THIS ======================================
        //if (aIn.peptideSequence != "GISEVVFDR")
        //	throw new Exception("Debugging, doh! -- comment shit in PepQuant.scala")
        //  ============================== COMMENT THIS ======================================
        
        pcf = new GraphPeakCandidateFinder(ratios, filtered, aIn.refRatios, refWidth / 2)
        
        RelationInterval.setBounds(2.0)
        best = getBiggestAndBest
        
        /*if (largest == null || largest.width <= refWidth || largest.area < 1000) {
        	RelationInterval.setBounds(2.0)
        	var l2 = getBiggestAndBest
        	if (l2 != null)
        		if (largest == null)
        			largest = l2
        		else if (l2.fdr + FDR_TOLERANCE < largest.fdr)
        			largest = l2
        		else if (l2.fdr < largest.fdr + FDR_TOLERANCE && l2.area > largest.area)
        			largest = l2
        				
        }*/
        
        if (best.isEmpty) 
          List(new Peak(NULL_PEAK_CANDIDATE, times.toArray, filtered.length))
        else 
          best.map(pc => new Peak(pc, times.toArray, filtered.length))
    }


    
    private def getBiggestAndBest():Seq[PeakCandidate] = {
    	t0 = System.currentTimeMillis - last
        last = System.currentTimeMillis
        
    	uncheckedPcs = pcf.findPeakCandidates()
        
    	t1 = System.currentTimeMillis - last
        last = System.currentTimeMillis
        
        sanePcs.clear
        bestPCs = Nil
    	var localLargest = NULL_PEAK_CANDIDATE
    	var ret:Seq[PeakCandidate] = Nil
    	
    	if (aIn.params.noSanityCheck)
    		sanePcs ++= uncheckedPcs
    	else
	        for (pc <- uncheckedPcs)
	            if (sanityCheck(pc, filtered))
	                sanePcs += pc

        t2 = System.currentTimeMillis - last
        last = System.currentTimeMillis
        
        if (earlyExit) return Nil
        
        if (sanePcs.length > 0) {
            var bestPC = new PeakCandidate()
	        fakeData = new FakeData(
	        		aIn.cg.chromatograms.map(td => td.intensities.toArray).toArray, 
	        		aIn.params.nullDistributionSize, 
	        		aIn.refRatios, 
	        		refWidth / 2)
            
            t3 = System.currentTimeMillis - last
            last = System.currentTimeMillis
        
            sanePcs.foreach(_.calculateSignalToNoise(filtered))
            
            t4 = System.currentTimeMillis - last
            last = System.currentTimeMillis
            
            var fdrPCs = new ArrayBuffer[PeakCandidate]
	        for (pc <- sanePcs) {
	            var fdr = fakeData.discoveryRate(pc, bestPC.fdr + aIn.params.pValueTolerance)
	            if (fdr >= 0) {
	                pc.fdr = fdr
	                fdrPCs += pc
	                if (fdr < bestPC.fdr) bestPC = pc
	            }
	        }
            
            t5 = System.currentTimeMillis - last
            last = System.currentTimeMillis
        
            bestPCs = fdrPCs.filter(_.fdr < bestPC.fdr + aIn.params.pValueTolerance)
	
	
	        for (pc <- bestPCs) pc.integrate(filtered, aIn.params.noEstimatedQuantities)
	        if (aIn.params.singleAnswer) {
		        for (pc <- bestPCs)
		            if (pc.area > localLargest.area) localLargest = pc
		        ret = Array(localLargest)
	        } else
	        	ret = bestPCs
	        	
            t6 = System.currentTimeMillis - last
            last = System.currentTimeMillis
        }
    	return ret
    }


    private def sanityCheck(pc:PeakCandidate, values:Array[Array[Double]]):Boolean = {
        for (i <- 0 until values.length) {
            var sliceSum = 0.0
            for (t <- pc.start until pc.end) sliceSum += values(i)(t)
            var estSum = pc.estimates(i) reduceRight(_+_)
            if (sliceSum < 0.1 * estSum)
                return false
        }
        return true
    }
}
