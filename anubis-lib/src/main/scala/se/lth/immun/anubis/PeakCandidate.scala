package se.lth.immun.anubis

import org.apache.commons.math3.stat.StatUtils

class PeakCandidate(
	var conditions:List[RelationInterval] 	= Nil,
    var start:Int 							= Int.MaxValue,
    var end:Int 							= Int.MinValue,
    var fdr:Double 							= 2.0,
    var signalToNoise:Double 				= 1.0,
    var estimates:Array[Array[Double]] 		= Array(Array[Double]()),
    var areas:Array[Double] 				= Array()
) {
	val MIN_NOISE = 10.0
	
	def area = areaSum(areas.toList)
	def width = end - start
	
	private def areaSum(list:List[Double]):Double  = list match {
		case Nil => return 0
		case h::Nil => return h
		case h::t => return h + areaSum(t)
	}

    def addInterval(ri:RelationInterval) = {
        if (ri.start < start) start = ri.start
        if (ri.end > end) end = ri.end
        conditions = ri :: conditions
    }

    def overlap(ri:RelationInterval, minCommon:Int):Int = {
        var common:Int = Math.min(ri.end, end) - Math.max(ri.start, start)
        return if (common > minCommon) common else -1
    }
    
    def calculateSignalToNoise(values:Array[Array[Double]]) = {
    	var maxSignal = values.map(arr => 0.0)
    	for (c <- conditions) {
    		maxSignal(c.ratio.up) = math.max(maxSignal(c.ratio.up), c.maxUp)
    		maxSignal(c.ratio.down) = math.max(maxSignal(c.ratio.down), c.maxDown)
    	}
    	signalToNoise = maxSignal.zip(
    								values.map(arr => StatUtils.percentile(arr.take(start) ++ arr.drop(end), 50.0))
    						   ).map(t => t._1 / Math.max(t._2, MIN_NOISE)).max
    }

    def integrate(values:Array[Array[Double]], noEstimatedQuantities:Boolean = false) = {
        areas = new Array(values.length)
        for (t <- 0 until estimates.length) {
            areas(t) = 0
            var vals = values(t)
            for (pct <- 0 until estimates(t).length)
                areas(t) += {	if (noEstimatedQuantities || vals(pct + start) <= estimates(t)(pct))
				    				vals(pct + start)
				    			else 
				    				estimates(t)(pct)
            				}
        }
    }


    override def toString:String = {
    	var fdrString = if (fdr != 2) {", " + fdr} else {"NO_FDR";}
    	return "PeakCandidate[" + conditions.length + fdrString + "] " + start + " - " + end;
    }
}