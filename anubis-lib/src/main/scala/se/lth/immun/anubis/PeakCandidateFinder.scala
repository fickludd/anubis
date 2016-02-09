package se.lth.immun.anubis

import org.apache.commons.math3.stat.StatUtils
import se.lth.immun.math.Ratios
import se.lth.immun.math.RatioIndex

import scala.collection.mutable.ArrayBuffer

class GraphPeakCandidateFinder(
            var ratios:Ratios,
            var values:Array[Array[Double]],
            var targets:Array[Ratio],
            var minLengths:Array[Int]
) {
	def this(ratios:Ratios, values:Array[Array[Double]], targets:Array[Ratio], minLength:Int) = 
		this(ratios, values, targets, targets.map(t => minLength))
	
	
	var minOverlap = 1.0
	var findEdges:(Seq[RelationInterval]) => (Array[List[Int]]) = findEdgesRestrictive
	
	
	
	def findPeakCandidates(calculateEstimates:Boolean = true):List[PeakCandidate] = {
		var list = new ArrayBuffer[RelationInterval]

        Ratios.iterate(values.length, (ri, i, j) => 
            {
                if (!java.lang.Double.isNaN(targets(ri).mean)) {
                	var ris = RelationInterval.getPeakIntervals(
	                        new RatioIndex(i, j, ri),
	                        minLengths(ri),
	                        targets(ri).mean,
	                        ratios.getRatio(ri),
	                        values(i),
	                        values(j));
	                for (relInt <- ris) list += relInt 
                }
            })

        list = list.sortWith((riA, riB) => riA.start < riB.start)

        var edges = findEdges(list)

        var pcRefList = new Array[PeakCandidate](list.length)
        var pcList = List[PeakCandidate]()

        for (i <- 0 until list.length) {
            for (e <- edges(i))
                if (pcRefList(e) != null) pcRefList(i) = pcRefList(e)
            
            if (pcRefList(i) == null) {
                var pc = new PeakCandidate()
                pcList = pcList :+ pc
                pcRefList(i) = pc
            }
            for (e <- edges(i))
                if (pcRefList(e) == null) pcRefList(e) = pcRefList(i)
        }

        for (i <- 0 until list.length)
            pcRefList(i).addInterval(list(i))

        if (calculateEstimates)
        	for (pc <- pcList) pc.estimates = getEstimates(pc)
        
        return pcList
    }
	
	
	
	
	def findEdgesLoose(list:Seq[RelationInterval]):Array[List[Int]] = {
		var edges = new Array[List[Int]](list.length)
        for (i <- 0 until list.length) {
            edges(i) = Nil
            var k = i + 1
            var li = list(i)
            while (k < list.length && list(k).start <= li.end - minOverlap) {
                if (    	li.ratio.ratio != list(k).ratio.ratio
                		&&	li.ratio.inCommonTrans(list(k).ratio)) 
                    edges(i) = edges(i) :+ k
                k += 1
            }
        }
		return edges
	}
	
	
	
	
	def findEdgesRestrictive(list:Seq[RelationInterval]):Array[List[Int]] = {
		var edges = new Array[List[Int]](list.length)
        for (i <- 0 until list.length) {
            edges(i) = Nil
            var k = i + 1
            var li = list(i)
            while (
            			k < list.length 
            		&& 	li.end > list(k).start) {
                if (    	li.ratio.ratio != list(k).ratio.ratio
                		&&	li.ratio.inCommonTrans(list(k).ratio)
                		&&	2*(li.end - list(k).start) > math.min(li.length, list(k).length)) 
                    edges(i) = edges(i) :+ k
                k += 1
            }
        }
		return edges
	}




    private def getEstimates(pc:PeakCandidate):Array[Array[Double]] = {
        var targetRatioTable = ratios.getTargetRatioTable(Ratio.ONE)(targets, Ratio.INVERSE)
        def stable(r:Ratio):Boolean = 0.5 * r.mean > r.stdDev

        var ok = new Array[Boolean](values.length)
        var estimates = new Array[Array[Double]](values.length)
        for (e <- 0 until ok.length) estimates(e) = new Array[Double](pc.end - pc.start)

        for (t <- pc.start until pc.end) {
            for (b <- 0 until ok.length) ok(b) = false

            for (c <- pc.conditions)
                if (c.start <= t && c.end >= t) {
                    ok(c.ratio.up) = true
                    ok(c.ratio.down) = true
                }

            for (b <- 0 until ok.length) {
                try {
                	if (ok(b))
	                    estimates(b)(t - pc.start) = values(b)(t)
	                else {
	                    var localEstimates = List[Double]()
	                    for (e <- 0 until ok.length) {
	                        if (e != b && ok(e) && stable(targetRatioTable(b)(e))) {
	                        	var multiplier = targetRatioTable(b)(e).mean
	                        	localEstimates = localEstimates :+ (values(e)(t) * multiplier)
	                        }
	                    }
	                    estimates(b)(t - pc.start) = 
	                    	if (localEstimates.isEmpty)	 values(b)(t) 
	                    	else 						 StatUtils.mean(localEstimates.toArray)
	                }
                } catch {
                	case e:Exception => e.printStackTrace()
                }
            }
        }

        return estimates
    }
}