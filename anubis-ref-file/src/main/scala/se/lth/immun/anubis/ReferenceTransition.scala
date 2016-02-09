package se.lth.immun.anubis

import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.xml.XmlUtil

object ReferenceTransition {
    	
    def fromFile(r:XmlReader):ReferenceTransition = {
		
		import ReferenceFile._
		import XmlUtil._
		var x:ReferenceTransition = new ReferenceTransition
		
		x.mz = r.readAttributeDouble(MZ)
        x.ion = r.readAttributeString(ION)
        x.dataName = r.readAttributeString(DATA_NAME)
        
		return x
    }
}




class ReferenceTransition(
    var mz:Double = 0,
    var ion:String = "",
    var dataName:String = ""
) {
	import ReferenceFile._

    override def toString() = "Transition["+ion+"]: " + mz

    def write(w:XmlWriter, idStr:String, id:Int) {
        w.startElement(ReferenceFile.TRANSITION)
        w.writeAttribute(MZ, mz)
        w.writeAttribute(ION, ion)
        w.writeAttribute(DATA_NAME, dataName)
        w.writeAttribute(idStr, id)
        w.endElement()
    }
}