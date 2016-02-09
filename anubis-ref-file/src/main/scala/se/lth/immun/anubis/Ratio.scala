package se.lth.immun.anubis

import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.xml.XmlUtil



object Ratio {
	val ONE = new Ratio(0, 0, 1, 0)
	def INVERSE(r:Ratio) = new Ratio(
								r.transitionId2, 
								r.transitionId1, 
								1.0 / r.mean, 
								(r.stdDev / r.mean) * (1 / r.mean)
									)
	
	def fromFile(r:XmlReader):Ratio = {
		import ReferenceFile._
		
		return new Ratio(
				r.readAttributeInt(ID1), 
				r.readAttributeInt(ID2), 
				r.readAttributeDouble(MEAN), 
				r.readAttributeDouble(STD_DEV)
			)
	}
}



class Ratio(
    var transitionId1:Int,
    var transitionId2:Int,
    var mean:Double,
    var stdDev:Double
) {
	import ReferenceFile._
	
	override def toString() = "Ratio["+transitionId1+" / "+transitionId2+"]: " + mean
    
    def write(w:XmlWriter) {
        w.startElement(RATIO)
        w.writeAttribute(ID1, transitionId1)
        w.writeAttribute(ID2, transitionId2)
        w.writeAttribute(MEAN, mean)
        w.writeAttribute(STD_DEV, stdDev)
        w.endElement()
    }
}