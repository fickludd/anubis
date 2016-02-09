package se.lth.immun.anubis

import se.lth.immun.math.RatioIndex

object RelationInterval {
    var MIN_SIGNAL = 1.0
	var UPPER_BOUND = 1.5
    var LOWER_BOUND = 1.0 / UPPER_BOUND
    def resetBounds = setBounds()
    def setBounds(b:Double = 1.5) = {
		UPPER_BOUND = b
		LOWER_BOUND = 1.0 / UPPER_BOUND
	}
    
    def getPeakIntervals(
            ratio:RatioIndex,
            minLength:Int,
            target:Double,
            relations:Array[Double],
            trans1:Array[Double],
            trans2:Array[Double]
            ):List[RelationInterval] = {

        var list:List[RelationInterval] = Nil

        var start:Int = 0
        var end:Int = 0
        var max1:Double = 0
        var max2:Double = 0
        var area:Double = 0

        for (i <- 0 until relations.length) {
            if ((	   relations(i) > target * LOWER_BOUND 
            		&& relations(i) < target * UPPER_BOUND
            		&& trans1(i) > MIN_SIGNAL
            		&& trans2(i) > MIN_SIGNAL				)) {
                max1 = Math.max(max1, trans1(i))
                max2 = Math.max(max2, trans2(i))
                area += trans1(i) + trans2(i)
                end += 1
            } else {
                if (end - start > minLength) {
                    list = list :+ new RelationInterval(start, end, ratio, target, area, max1, max2)
                }

                max1 = 0
                max2 = 0
                area = 0
                start = i
                end = i
            }
        }

        if (end - start > minLength) {
            list = list :+ new RelationInterval(start, end, ratio, target, area, max1, max2)
        }

        return list
    }
}

class RelationInterval(
		var start:Int,
		var end:Int,
		var ratio:RatioIndex,
		var target:Double,
		var area:Double,
		var maxUp:Double,
		var maxDown:Double
) {
    override def toString = ("\n  RelationInterval["+ratio.up+" / "+ratio.down+"] target=%.2f").format(target)
    def length = end - start
}