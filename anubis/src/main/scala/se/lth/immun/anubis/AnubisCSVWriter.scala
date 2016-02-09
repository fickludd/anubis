package se.lth.immun.anubis

import java.io.FileWriter
import PostAnubisAnalysis._

class AnubisCSVWriter(
			var ara:PostAnubisAnalysis,
			var out:FileWriter,
			var fdrCutoff:Double
) {
	
	private def writeln(l:String = "") = out.write(l + "\n")
	private def cite(a:String):String = "\"" + a + "\""
	private def nice(q:QualityDataPoint):String = 
						if (java.lang.Double.isNaN(q.lfdr))	"No peak"
						else if (q.lfdr >= 0 && 
								  q.lfdr <= fdrCutoff) 			q.quantity.toString
						else 									">FDR"
	
	def write(
			title:String
	):Unit = {
		// Column headers
		var l1 = new StringBuilder(title+",")
		var l2 = new StringBuilder("Protein,Peptide")
		var scans = ara.repGrouping.collect { case c:ColTree.Scan => c }
		scans.map(scan => {
			l1 ++= ","
			l2 ++= ","+cite(scan.name)
		})
		writeln(l1.toString)
		writeln(l2.toString)
		
		
		// Row headers and data
		var prots = ara.protGrouping.collect { case p:RowTree.Prot => p }
		for (prot <- prots) {
			for (pep <- prot.children.map(_.asInstanceOf[RowTree.Pep]))
				writePep(pep, prot)
		}
		
		// Empty space
		writeln()
		writeln()
		
		out.close
	}
	
	
	
	def writePep(
			pep:RowTree.Pep, 
			prot:RowTree.Prot, 
			useHeavy:Boolean = false
	):Unit = {
		var l = new StringBuilder(cite(prot.name)+","+cite(pep.name))
		
		var pepScans = pep.getScans(ara)
		pepScans.map(scan => {
			l ++= ","+nice(scan)
		})
		writeln(l.toString)
	}
}