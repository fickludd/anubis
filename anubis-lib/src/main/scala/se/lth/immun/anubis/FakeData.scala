package se.lth.immun.anubis


import se.lth.immun.math.Ratios
import se.lth.immun.signal.Filter
import se.lth.immun.signal.Decomposers
import se.lth.immun.signal.Synthesizers
import se.lth.immun.signal.WaveletLevel
import org.apache.commons.math3.stat.StatUtils


object FakePC {
	
	def of(wavelets:Array[Array[WaveletLevel]]):FakePC = {
		var l = wavelets.length
		var fake = wavelets.map(w => {
            WaveletLevel.synthesize(
                WaveletLevel.generateRandom(w, 4),
                4,
                Synthesizers.halfBandCustom)
        })
        fake = Filter.baseLineReduce(fake) map (ds => Filter.savitzkyGolay9(ds))
        new FakePC(fake, new Ratios(fake), fake.map(f => StatUtils.percentile(f, 50)))
	}
}



class FakePC(
		var intensities:Array[Array[Double]],
		var ratios:Ratios,
		var noise:Array[Double]
) {}



class FakeData(
		var realData:Array[Array[Double]],
		var nOFakes:Int,
		var targets:Array[Ratio],
		var minLength:Int
) {
	
	val CONDITION_LENGTH_TOLERANCE 	= 1.0
	val S2N_TOLERANCE 				= 0.8
	val MIN_NOISE 					= 10.0
	
	type DataSetGroup 	= Array[Array[Double]]
	
    
    var nOFragments 	= realData.length
    var wavelets 		= new Array[Array[WaveletLevel]](nOFragments)
    for (i <- 0 until nOFragments)
        wavelets(i) = WaveletLevel.decompose(
                realData(i), 
                4,
                Decomposers.halfBandCustom)

	var cachedFakes 	= nOFakes <= 1000
    var fakes 			= 
    	if (cachedFakes) 
    		new Array[FakePC](nOFakes).map(f => FakePC.of(wavelets))
    	else null
	def getFake(i:Int) = 
		if (cachedFakes) fakes(i)
		else FakePC.of(wavelets)
    	
    	

    def discoveryRate(pc:PeakCandidate):Double = {
        var count = 0

        for (fi <- 0 until nOFakes) 
            count += amountInFake(pc, getFake(fi))

        return (count) / nOFakes.toDouble
    }



    def discoveryRate(pc:PeakCandidate, roof:Double):Double = {
        var count = 0

        for (fi <- 0 until nOFakes) {
            count += amountInFake(pc, getFake(fi))
            if (count > roof * nOFakes) return -1
        }
        
        return count / nOFakes.toDouble
    }




    private def amountInFake(pc:PeakCandidate, fake:FakePC):Int = {
        var start = 0
        var end = 0

        var fakeDSG = fake.intensities
        var ratio = fake.ratios
        var noise = fake.noise
        
        var minLengths = pc.conditions.map(_.length).sorted
        
        // Condition length sum matching. Good?
        /*var pcLength = minLengths.sum
    	var pcRatios = targets.map(_ => 0)
    	pc.conditions.foreach(ri => pcRatios(ri.ratio.ratio) = 1)
    	var pcRatioSum = pcRatios.sum
        	
        	
        var risList:List[List[RelationInterval]] = Nil
        for (ri <- ratio.indexes)
            risList = risList :+ RelationInterval.getPeakIntervals(
											                ri,
											                minLengths(0),
											                targets(ri.ratio),
											                ratio.getRatio(ri.ratio),
											                fakeDSG(ri.up),
											                fakeDSG(ri.down)
												                  )
        
        var riList = risList.flatten.sortWith((riA, riB) => riA.start < riB.start)

        var edges = new Array[List[Int]](riList.length)
        for (i <- 0 until riList.length) {
            if (edges(i) == null) edges(i) = Nil
            var k = i + 1
            while (k < riList.length && riList(k).start < riList(i).end) {
                if (    	riList(i).ratio.ratio != riList(k).ratio.ratio
                		&&	riList(i).ratio.inCommonTrans(riList(k).ratio)) {
                    edges(i) = edges(i) :+ k
                    if (edges(k) == null) edges(k) = Nil
                    edges(k) = edges(k) :+ i
                }
                k += 1
            }
        }
        
        
        def extract(i:Int):List[RelationInterval] = {
        	var e = edges(i)
        	if (e == null) Nil
        	else {
        		edges(i) = null
        		riList(i) :: (e.map(ei => extract(ei)).flatten)
        	}
        }
        for (ei <- 0 until edges.length) {
        	var ris = extract(ei)
        	var ratios = targets.map(_ => 0)
        	ris.foreach(ri => ratios(ri.ratio.ratio) = 1)
        	if (ris.map(_.length).sum >= pcLength && ratios.sum >= pcRatioSum) return 1
        }
        
        return 0
        */
        
        // Any-pairs conditions matching. Pretty nice
        
        var risList:List[List[RelationInterval]] = Nil
        for (ri <- ratio.indexes)
            risList = risList :+ RelationInterval.getPeakIntervals(
											                ri,
											                minLengths(0),
											                targets(ri.ratio).mean,
											                ratio.getRatio(ri.ratio),
											                fakeDSG(ri.up),
											                fakeDSG(ri.down)
												                  )
        
        while (risList.length > 0) {
        	var ris = risList.head
        	risList = risList.tail
        	for (c <- ris) {
        		start = c.end - minLengths(0)
                end = math.min(start + pc.end - pc.start, fakeDSG(0).length)
                var cs = c :: risList.map(_.filter(
                		ic => 	(	ic.end > start + minLengths(0)
	                			&& 	ic.start < end - minLengths(0))
	                	)).flatten
	            var fragments = (cs.map(_.ratio.up) ++ cs.map(_.ratio.down)).toSet
	            
	            var ok = minLengths.forall(
	            		ml => {
	            			var i = cs.indexWhere(_.length > ml)
	            			if (i >= 0) cs = cs.take(i) ++ cs.drop(i+1)
	            			i >= 0
	            		})
	            
	            if (ok) {
                	if (fragments.exists(f => {
                		var s = fakeDSG(f).slice(start, end).max
                		s / Math.max(noise(f), MIN_NOISE) > S2N_TOLERANCE * pc.signalToNoise
                	})) return 1
                }
        	}
        }
        
        return 0
        
        
        // Same-pairs conditions required. Not right?
        /*
        
        var filteredTargets = Array(Double.NaN).padTo(ratio.length, Double.NaN)
        var minLengths = filteredTargets.map(d => -1)
        for (c <- pc.conditions) {
        	filteredTargets(c.ratio.ratio) = targets(c.ratio.ratio)
        	minLengths(c.ratio.ratio) = c.length
        }
        
        var risList:List[List[RelationInterval]] = Nil

        for (c <- pc.conditions) {
            //minLengths(c.ratio.ratio) = c.end - c.start
            risList = risList :+ RelationInterval.getPeakIntervals(
											                c.ratio,
											                minLengths(c.ratio.ratio),
											                c.target,
											                ratio.getRatio(c.ratio.ratio),
											                fakeDSG(c.ratio.up),
											                fakeDSG(c.ratio.down)
												                  )
        }

        for (ris <- risList) {
            for (c <- ris) {
                start = c.end - minLengths(c.ratio.ratio)
                end = math.min(start + pc.end - pc.start, fakeDSG(0).length)
                
                var isOK:Boolean = risList.forall(
                					iRis => 
                							iRis.length > 0 &&
                							(iRis(0).ratio.ratio == c.ratio.ratio ||
                							 iRis.exists(
	                							ic => 	(	ic.end > start + minLengths(ic.ratio.ratio)
	                									&& 	ic.start < end - minLengths(ic.ratio.ratio))
                							 )
                							)
                					)
                
                if (isOK) {
                	
                	if (pc.conditions.exists(c => {
                		var up = 0.0
               			Lambda.iterate(start, end, 
               				(i:Int) => up = math.max(up, fakeDSG(c.ratio.up)(i) ))
               			var down = 0.0
                		Lambda.iterate(start, end, (i:Int) => down = math.max(down, fakeDSG(c.ratio.up)(i)))
                		math.max(up / noise(c.ratio.up), down / c.ratio.down) > S2N_TOLERANCE * pc.signalToNoise
                	})) return 1
                }
            }
        }
        return 0
        */
        
        
        // Using GraphPeakCandidateFinder... timeconsuming!
        /*
        var pcf = new GraphPeakCandidateFinder(ratio, fakeDSG, filteredTargets, minLengths)
        var pcs = pcf.findPeakCandidates(false)
               
        
        if (pcs.exists(newPc => {
        	newPc.calculateSignalToNoise(fakeDSG)
        	(newPc.signalToNoise > S2N_TOLERANCE * pc.signalToNoise &&
        	 pc.conditions.forall(
        			 c => { newPc.conditions.exists(
        				newC => {	c.ratio.ratio == newC.ratio.ratio &&
				        			newC.length > CONDITION_LENGTH_TOLERANCE * c.length }
        								)
        	 })
        	)
        })) return 1
        return 0
        */
    }
}