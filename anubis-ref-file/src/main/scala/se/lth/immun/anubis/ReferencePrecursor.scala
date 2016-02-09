package se.lth.immun.anubis

import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.xml.XmlUtil

import scala.collection.mutable.ArrayBuffer

object ReferencePrecursor {
    	
    def fromFile(r:XmlReader):ReferencePrecursor = {
		
		import ReferenceFile._
		import XmlUtil._
		var x = new ReferencePrecursor(r.readAttributeDouble(MZ))
    	
        if (r.hasAttribute(FILE_ID))
        	x.fileId = r.readAttributeInt(FILE_ID)
        else if (r.hasAttribute(DDB_SCAN_ID))
        	x.ddbScanId = r.readAttributeInt(DDB_SCAN_ID)
        else
        	x.seshatRefId = r.readAttributeLong(SESHAT_REF_ID)
        x.proteinName = r.readAttributeString(PROTEIN)
        x.peptideSequence = r.readAttributeString(PEPTIDE_SEQ)

        r.ensure(RT)
        x.retentionTime = RetentionTime.fromFile(r)

        r.ensure(MEASURED_TRANSITIONS)
        var mt = new Array[ReferenceTransition](r.readAttributeInt(COUNT))
        for (i <- 0 until mt.length) {
            r.ensure(TRANSITION)
            mt(r.readAttributeInt(TRANSITION_ID)) = ReferenceTransition.fromFile(r)
        }
		x.measuredTransitions = mt

        r.ensure(IGNORED_TRANSITIONS)
        var it = new Array[ReferenceTransition](r.readAttributeInt(COUNT))
        for (i <- 0 until it.length) {
            r.ensure(TRANSITION)
            it(r.readAttributeInt(TRANSITION_ID)) = ReferenceTransition.fromFile(r)
        }
		x.ignoredTransitions = it

        r.ensure(RATIOS)
        var ra = new Array[Ratio](r.readAttributeInt(COUNT))
        for (i <- 0 until ra.length) {
            r.ensure(RATIO)
            ra(i) = Ratio.fromFile(r)
        }
		x.ratios = ra
        
        return x
	}
}



class ReferencePrecursor(
		var mz				:Double,
		var charge			:Int		= 0,
		var fileId			:Int 		= -1,
		var ddbScanId		:Int 		= -1,
		var seshatRefId		:Long 		= -1,
    	var peptideSequence	:String 	= "",
    	var proteinName		:String 	= "",
    	var retentionTime		:RetentionTime 				= new RetentionTime(),
    	var measuredTransitions	:Seq[ReferenceTransition] 	= Nil,
    	var ignoredTransitions	:Seq[ReferenceTransition] 	= Nil,
    	var ratios				:Seq[Ratio] 				= Nil
) {

    def allTransitions = measuredTransitions ++ ignoredTransitions

    override def toString() = "RPC: " + peptideSequence + ", " + mz

    def clearTransitions() {
        measuredTransitions 	= Nil
        ignoredTransitions 		= Nil
        ratios 					= Nil
    }



    def write(w:XmlWriter) {
    	import ReferenceFile._
    	
        w.startElement(PRECURSOR)
        w.writeAttribute(MZ, mz)
        if (fileId >= 0) 			w.writeAttribute(XmlUtil.FILE_ID	, fileId		)
        else if (ddbScanId >= 0) 	w.writeAttribute(DDB_SCAN_ID		, ddbScanId		)
        else 						w.writeAttribute(SESHAT_REF_ID		, seshatRefId	)
        w.writeAttribute(PROTEIN, proteinName)
        w.writeAttribute(PEPTIDE_SEQ, peptideSequence)

        retentionTime.write(w)

        w.startListElement(MEASURED_TRANSITIONS, measuredTransitions)
        for (t <- 0 until measuredTransitions.length)
            measuredTransitions(t).write(w, TRANSITION_ID, t)
        w.endElement()

        w.startListElement(IGNORED_TRANSITIONS, ignoredTransitions)
        for (t <- 0 until ignoredTransitions.length)
            ignoredTransitions(t).write(w, TRANSITION_ID, t)
        w.endElement()

        w.startListElement(RATIOS, ratios)
        for (r <- ratios)
            r.write(w)
        w.endElement()

        w.endElement()
    }


    override def equals(obj:Any):Boolean = obj match {
    	case pc:ReferencePrecursor => 
    		pc.peptideSequence == peptideSequence && pc.mz == mz
    	case _ => 
    		throw new IllegalArgumentException(
    				"Could not cast other object to a ReferencePrecursor.")
    }


    override def hashCode():Int = {
        var sum:Int = 0
        for (c <- peptideSequence)
            sum += c

        return ((1000 * mz) + sum).toInt
    }
}