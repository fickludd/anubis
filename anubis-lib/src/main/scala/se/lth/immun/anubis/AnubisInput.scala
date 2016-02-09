package se.lth.immun.anubis


import se.lth.immun.mzml.ghost.XChromatogramGroup
import se.lth.immun.mzml.ghost.XChromatogramGrouper
import scala.collection.mutable.ArrayBuffer

import se.lth.immun.math.Ratios
import se.lth.immun.math.Matrix


object AnubisInput {
	
	case class Fragment(mz:Double, ion:String)
	
	def of(cg:XChromatogramGroup, rpc:ReferencePrecursor, params:AnubisParams, times:Seq[Double] = Nil):AnubisInput = {
	    
		var filtered = cg.extract(
	    					rpc.measuredTransitions.map(_.mz).toArray, 
	    					true, 
	    					params.q3tolerance
	    				)
	
	    if (filtered.chromatograms.filter(_ != null).length < 2)
	    	throw new AnubisInputException("Less than 2 of transitions in reference precursor ("+
	    										rpc.measuredTransitions.map(_.mz).mkString(",")+
	    									") in measured data ("+
	    										cg.chromatograms.map(_.q3).mkString(",")+		
	    									")", "rpc:ReferencePrecursor")
	    
	    var temp = Matrix.get2d[Ratio](rpc.measuredTransitions.length)
	    for (r <- rpc.ratios)
	        temp(r.transitionId1)(r.transitionId2) = r
	
	        
	    var temp0 = (temp(0).tail) :+ new Ratio(0,0,1,0)
	    var sorted = temp0.filter(r => filtered.chromatograms(r.transitionId2) != null).sortBy(_.mean)
	    var remove = sorted.drop(params.transitionLimit)
	    var arr = filtered.chromatograms.toArray
	    remove.foreach(r => arr(r.transitionId2) = null)
	    
	    var mapping = new ArrayBuffer[Int]
	    for (i <- 0 until filtered.length)
	        if (filtered.chromatograms(i) != null) mapping += i
	
	    var mappedRatios = new ArrayBuffer[Ratio]
	
	    Ratios.iterate(mapping.length, 
	    			(ri, i, j) => {
	    				var oldRatio = temp(mapping(i))(mapping(j))
	    				mappedRatios += new Ratio(i, j, oldRatio.mean, oldRatio.stdDev) 
	    			}
	    		)
	
	    var subTGD = new XChromatogramGroup(cg.q1, mapping map (filtered.chromatograms(_)))
	    subTGD = subTGD.resample(times)
	    
	    return new AnubisInput(
		        rpc.peptideSequence,
		        rpc.mz,
		        subTGD,
		        mapping.map(i => {
		        	val tr = rpc.measuredTransitions(i)
		        	Fragment(tr.mz, tr.ion)
		        }),
		        mappedRatios.toArray,
		        params
	        )
    }
}



class AnubisInput(
            val peptideSequence:String,
            val precursorMZ:Double,
            val cg:XChromatogramGroup,
            val fragments:Seq[AnubisInput.Fragment],
            val refRatios:Array[Ratio],
            val params:AnubisParams
) {
    if (refRatios.length == 0)
        throw new AnubisInputException("Cannot have zero reference ratios.", "refRatios:Array[Ratio]");

    if (cg.length < 2)
        throw new AnubisInputException("Cannot have "+cg.length+" chromatograms, need at least 2.", "cg:XChromatogramGroup");
}