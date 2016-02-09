package se.lth.immun.anubis

import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.xml.XmlUtil

import se.lth.immun.traml.ghost._
import se.lth.immun.unimod.UniMod

import java.io.IOException
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import se.lth.immun.math.Ratios

object ReferenceFile {
	val MAIN_NODE	= "reference"
	val MAIN_NODES 	= Array(MAIN_NODE, "reference_definition", "reference_defition")
    val FILES 		= "reference_files"
    val FILE 		= "reference_file"
    val PRECURSORS 	= "precursors"
    	
    val MEASURED_TRANSITIONS 	= "measured_transitions"
    val IGNORED_TRANSITIONS 	= "ignored_transitions"
    val TRANSITION_ID 			= "transition_id"
    val RATIOS 					= "transition_ratios"
    
    val TRANSITION 	= "transition"
    val ION 		= "ion"
    val DATA_NAME 	= "data_name"
    
    val DDB_SCAN_ID 	= "ddb_scan_id"
    val SESHAT_REF_ID 	= "seshat_ref_id"
	val MZ 				= "mz"
    val AREA 			= "area"
    val PRECURSOR 		= "precursor"
    val PEPTIDE_SEQ 	= "peptide_sequence"
    val PROTEIN 		= "protein_name"
    	
    val RT 		= "retention_time"
    val START 	= "start"
    val END 	= "end"
    val PEAK 	= "peak"
    	
    val RATIO 		= "transition_ratio"
    val ID1 		= "id1"
    val ID2 		= "id2"
    val MEAN 		= "mean"
    val STD_DEV 	= "std_dev"
    	
    	
    	
    def fromFile(r:XmlReader):ReferenceFile = {
		var x = new ReferenceFile
		
		r.untilOneOf(MAIN_NODES)

        r.ensure(FILES) 
        x.files = new Array(r.readAttributeInt(XmlUtil.COUNT))

        for (i <- 0 until x.files.length) {
            r.is(FILE)
            x.files(r.readAttributeInt(XmlUtil.FILE_ID)) =
                new File(r.readAttributeString(XmlUtil.ABS_PATH))
        }
        
        r.ensure(PRECURSORS)
        x.precursors = new Array(r.readAttributeInt(XmlUtil.COUNT))

        for (i <- 0 until x.precursors.length) {
        	r.is(PRECURSOR)
            x.precursors(i) = ReferencePrecursor.fromFile(r)
        }
        
        r.close
        return x
	}
	
	
	def fromTraML(r:XmlReader):ReferenceFile = {
		val gt = GhostTraML.fromFile(r)
		val x = new ReferenceFile
		
		import se.lth.immun.chem._
		
		def getCharge(mz:Double, gp:GhostPeptide) = 
			gp.charge.getOrElse({
				val pep = UniMod.parseUniModSequence(gp.sequence)
				val m = pep.monoisotopicMass()
				math.round(m / mz).toInt
			})
		
		
		var referencePrecursors = new ArrayBuffer[ReferencePrecursor]
		for (tg <- gt.transitionGroups) {
			var key = tg._1
			var ts = tg._2
			if (ts(0).peptideRef != null) {
				var pep = gt.peptides(ts(0).peptideRef)
				var prot = if (!pep.proteins.isEmpty) gt.proteins(pep.proteins.head) else null
				
				var ratios = new ArrayBuffer[Ratio]
				Ratios.iterate(ts.length, (ri,  ui, di) => {
					ratios += new Ratio(ui, di, ts(ui).intensity / ts(di).intensity, -1.0)
				})
				
				var rt = 
					if (ts(0).rtStart > 0 && ts(0).rtEnd > 0)
						new RetentionTime(ts(0).rtStart, (ts(0).rtStart+ts(0).rtEnd)/2, ts(0).rtEnd)
					else if (pep.tramlPeptide.retentionTimes.nonEmpty)
						new RetentionTime(
								pep.tramlPeptide.retentionTimes(0).cvParams(0).value.get.toDouble,
								pep.tramlPeptide.retentionTimes(0).cvParams(0).value.get.toDouble,
								pep.tramlPeptide.retentionTimes(0).cvParams(0).value.get.toDouble
							)
					else new RetentionTime
				
				var seq = pep.fullPeptideName.getOrElse(pep.sequence)
					
				referencePrecursors += 
					new ReferencePrecursor(
						key._1,
						getCharge(key._1, pep),
						-1,
						-1,
						-1,
						seq,
						if (prot != null) prot.id else "",
						rt,
						ts.map(t => new ReferenceTransition(t.q3, t.ions.mkString(","))),
						Nil,
						ratios
					)
			}
		}
		
		x.precursors = referencePrecursors.toArray
		
		return x
	}
	
	
	
	def main(args:Array[String]):Unit = {
		var s = args(0)
		var refFile = new File(s)
		var r = new XmlReader(new java.io.BufferedReader(new java.io.FileReader(refFile)))
		if (s.toLowerCase.endsWith(".traml"))
			ReferenceFile.fromTraML(r)
		else 
			ReferenceFile.fromFile(r)
	}
}



class ReferenceFile(
		var files:Array[File] = Array(),
		var precursors:Array[ReferencePrecursor] = Array(),
		r:XmlReader = null
) {
	def write(w:XmlWriter) {
    	import ReferenceFile._
    	
        w.startDocument()
        w.startElement(MAIN_NODE)

        w.startListElement(FILES, files)
        for (i <- 0 until files.length) {
            w.startElement(FILE)
            w.writeAttribute(XmlUtil.FILE_ID, i)

            w.writeAttribute(XmlUtil.ABS_PATH, files(i).getCanonicalPath)
            w.endElement()
        }
        w.endElement()

        w.startListElement(PRECURSORS, precursors)
        for (pc <- precursors)
            pc.write(w)
        w.endElement()
            
        w.endElement()
        w.endDocument()
    }
}