package se.lth.immun.anubis

import se.lth.immun.collection.DataMatrix
import se.lth.immun.proteomics.PeptideIon

import java.io.File



object QualityDataPoint {
	val ZERO = new QualityDataPoint(0.0, 0.0, -1, -1, -1)
	val MISSING = new QualityDataPoint(0.0, Double.NaN, Double.NaN, Double.NaN, Double.NaN)
}

class QualityDataPoint(
		var quantity:Double,
		var lfdr:Double,
		var start:Double = -1,
		var apex:Double = -1,
		var end:Double = -1
) {
	
	def noNaN(Q:Double)(q:Double) = if (java.lang.Double.isNaN(q)) Q else q
	def isNaN = java.lang.Double.isNaN _
	def isMissing = isNaN(lfdr)
	
	def +(q:QualityDataPoint) = 
		if (isMissing && q.isMissing) QualityDataPoint.MISSING
		else if (isMissing) q
		else if (q.isMissing) this
		else
			new QualityDataPoint(
				quantity + q.quantity,
				Math.max(lfdr, q.lfdr),
				-1, -1, -1)
	def norm(q:QualityDataPoint, filter:QualityDataPoint => Boolean) = 
		if (filter(this) && filter(q))
			this / q
		else QualityDataPoint.ZERO
	def /(q:QualityDataPoint) =
		if (isMissing || q.isMissing) QualityDataPoint.MISSING
		else
			new QualityDataPoint(
				noNaN(0)(quantity) / noNaN(1.0)(q.quantity),
				Math.max(lfdr, q.lfdr),
				-1, -1, -1)
	def sum(q:QualityDataPoint, filter:QualityDataPoint => Boolean) =
		if (filter(this) && filter(q))
			this + q
		else if (filter(this)) this
		else if (filter(q)) q
		else QualityDataPoint.ZERO
}






class PostAnubisAnalysis(
		var rf:ResultFile,
		var filter:QualityDataPoint => Boolean,
		protGT:Option[GroupTree.Node] = None,
		repGT:Option[GroupTree.Node] = None,
		expandIsotopes:Boolean = false,
		stripPeptides:Boolean = false
) {
	
	import se.lth.immun.proteomics.MzImplicits._
	implicit val uncertainty = 0.01
	
	// grouping
	var protGrouping = convertProtGT(protGT)
	var repGrouping = convertRepGT(repGT)
	
	
	// setup data model
	private var pis:Array[PeptideIon] = 
		rf.precursors.map(pc => new PeptideIon(pc.mz, getSequence(pc)) )
	private var scans:Array[File] = rf.replicateFiles.map(_.file)
	private var data:Array[Array[QualityDataPoint]] = new Array(pis.length)
	for (ipi <- 0 until rf.precursors.length) {
		data(ipi) = new Array(scans.length)
		var pc = rf.precursors(ipi)
		for (fi <- 0 until scans.length) {
			var repls = pc.replicates.filter(_.fileId == fi)
			if (repls.isEmpty) {
				data(ipi)(fi) = QualityDataPoint.MISSING
			} else {
				var largest = repls.max(Ordering[Double].on[ResultReplicate](_.totalArea))
				data(ipi)(fi) = new QualityDataPoint(
						largest.totalArea, 
						largest.quality.fdr,
						largest.retentionTime.start,
						largest.retentionTime.peak,
						largest.retentionTime.end)
			}
		}
			
		data(ipi) = data(ipi) map (q => if (q == null) QualityDataPoint.MISSING else q)
	}
	var dataMatrix = new DataMatrix(
								data, 
								pis, 
								scans, 
								protGrouping,
								repGrouping
					) {
						colComparator = (f1:File) => (f2:File) => f1.getName == f2.getName
					}
	
	
	
	// protein grouping
	def getSequence(pc:ResultPrecursor) = 
		if (stripPeptides) pc.peptideSequence.filter( _.isLetter )
		else pc.peptideSequence
	def convertProtGT(protGTOpt:Option[GroupTree.Node]):RowTree.Group = {
		protGTOpt match {
		  case None =>
			var prots:List[RowTree.Prot] = Nil
			for (pc <- rf.precursors) {
				if (!prots.exists(_.name == pc.proteinName)) prots = prots :+ new RowTree.Prot(pc.proteinName, Nil)
				var prot = prots.find(_.name == pc.proteinName).get
				var newIso = new RowTree.PepIsotope(
								new PeptideIon(pc.mz, getSequence(pc)))
				prot.addPeptide(newIso)
			}
			return new RowTree.Group("root", prots)
		  case Some(protGT) =>
		    return _convertProtGT(protGT).asInstanceOf[RowTree.Group]
		} 
			
	}
	def _convertProtGT(gtn:GroupTree.Node):RowTree.Row = {
		if (gtn.isParent)
			return new RowTree.Group(gtn.name, gtn.children.map(_convertProtGT(_)))
		else {
			var prot = new RowTree.Prot(gtn.name, Nil)
			for (pc <- rf.precursors.filter(_.proteinName == gtn.name))
				prot.addPeptide(
						new RowTree.PepIsotope(
								new PeptideIon(pc.mz, pc.peptideSequence)))
			return prot
		}
	}
	
	
	// replicate grouping
	def convertRepGT(repGTOpt:Option[GroupTree.Node]):ColTree.Group = {
		repGTOpt match {
		  case None =>
		    new ColTree.Group("root", rf.replicateFiles.map(f => new ColTree.Scan(f.file, 1.0)))
		  case Some(repGT) =>
		    _convertRepGT(repGT).asInstanceOf[ColTree.Group]
		}
	}
	def _convertRepGT(gtn:GroupTree.Node):ColTree.Col = {
		if (gtn.isParent)
			return new ColTree.Group(gtn.name, gtn.children.map(_convertRepGT(_)), gtn.metaType != null)
		else
			return new ColTree.Scan(new File(gtn.name), gtn.value)
	}
	
	/*
	// utility functions
	def getScans(pi:PeptideIon):Array[QualityDataPoint] = {
		var ipi = pis.indexOf(pi)
		
		if (ipi == -1)
			throw new IllegalArgumentException("'"+pi+"' is not among the measured peptide ions.")
		
		return data(ipi)
	}
	
	def getScan(scan:String, row:Array[QualityDataPoint]):QualityDataPoint = {
		var iscan = scans.indexOf(scan)
		
		if (iscan == -1)
			throw new IllegalArgumentException("'"+scan+"' is not a replicate scan.")
		
		return row(iscan)
	}
	
	def get(pi:PeptideIon, scan:String):QualityDataPoint = {
		return getScan(scan, getScans(pi))
	}
	*/
}