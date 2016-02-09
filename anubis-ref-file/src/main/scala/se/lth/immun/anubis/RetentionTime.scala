package se.lth.immun.anubis

import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.xml.XmlUtil


object RetentionTime {
	
	def fromFile(r:XmlReader):RetentionTime = {
		import ReferenceFile._
		
		return new RetentionTime(
					r.readAttributeDouble(START),
			        r.readAttributeDouble(PEAK),
			        r.readAttributeDouble(END)
				)
	}
}

class RetentionTime(
		var start:Double = 0, 
		var peak:Double = 0, 
		var end:Double = 0
) {
	def width = end - start
	import ReferenceFile._

    def write(w:XmlWriter) {
        w.startElement(RT)
        w.writeAttribute(START, start)
        w.writeAttribute(PEAK, peak)
        w.writeAttribute(END, end)
        w.endElement()
    }
}