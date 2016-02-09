package se.lth.immun.anubis

import org.apache.commons.math3.stat.StatUtils

import se.lth.immun.collection.AbstractAnyTree
import se.lth.immun.collection.AnyHolder
import se.lth.immun.proteomics.PeptideIon
import java.io.File


object ColTree extends AbstractAnyTree {
	type N = Col
	
	abstract class Col(
			id:Any
	) extends AbstractAnyNode {self:N =>
		var obj = id
		
		def name = obj.toString
		override def nodeName = "COL "
		override def toString = treeString()
		
		def getScans(
				ara:PostAnubisAnalysis, 
				row:Array[QualityDataPoint]):Seq[QualityDataPoint]
	}
	
	class Group(
			id:String,
			cols:Seq[Col],
			var average:Boolean = false
	) extends Col(id) {
		for (c <- cols) addChild(c)
		
		override def nodeName = "GRP " + name + (if (average) " +" else "")
		def collapse(
				qdps:Seq[QualityDataPoint], 
				filter:QualityDataPoint => Boolean):QualityDataPoint = {
			var q = StatUtils.mean(qdps.filter(filter).map(_.quantity).toArray)
			var fdr = if (qdps.filter(filter).isEmpty) Double.NaN else qdps.filter(filter).map(_.lfdr).max
			return new QualityDataPoint(q, fdr)
		}
		def getScans(
				ara:PostAnubisAnalysis, 
				row:Array[QualityDataPoint]):Seq[QualityDataPoint] = {
			var scans = children.map(_.getScans(ara, row)).flatten
			if (average)
				Array(collapse(scans, ara.filter))
			else 
				scans
		}
		def flatten():Seq[Group] = {
			if (!isParent) return Nil
			if (average) return Nil
			var subGroups = children.collect {case rg:Group => rg }
			if (subGroups.length == 0 || subGroups.exists(_.average)) return List(this)
			return subGroups.map(_.flatten).flatten
		}
	}
	
	class Scan(
			id:File,
			var value:Double
	) extends Col(id) {
		if (java.lang.Double.isNaN(value))
			value = 1.0
		override def name = id.getName
		override def nodeName = "SCN " + name
		def getScans(
				ara:PostAnubisAnalysis, 
				row:Array[QualityDataPoint]):Seq[QualityDataPoint] = {
			val q = ara.dataMatrix.getColumn(id, row)
			if (value != 1.0 && Scan.useScanValue)
				q.quantity /= value
			Array(q)
		}
	}
	
	object Scan {
		var useScanValue:Boolean = false
	}
}
/*
abstract class Col(
		var name:String,
		var children:Seq[Col]
) {
	def isParent = children.length > 0
	val TYPE_STRING = "COL"
	override def toString = strTree(0)
	def strTree(i:Int):String = strRepr(i, TYPE_STRING)
	def strName = name
	def strRepr(i:Int, t:String):String = 
		"".padTo(i, ' ') + 
		t + " " + strName + "\n" + 
		children.map(_.strTree(i+1)).foldLeft("")((s1, s2) => s1+s2)
	def getScans(
			ara:AnubisResultAnalysis, 
			row:Array[QualityDataPoint]):Seq[QualityDataPoint]
}

class RepGroup(
		name:String,
		childs:Seq[Col],
		var average:Boolean = false
) extends Col(name, childs) {
	override val TYPE_STRING = "GRP"
	override def strName = name + (if (average) " +" else "")
	def collapse(
			qdps:Seq[QualityDataPoint], 
			filter:QualityDataPoint => Boolean):QualityDataPoint = {
		var q = JTMath.mean(qdps.filter(filter).map(_.quantity))
		var fdr = if (qdps.filter(filter).isEmpty) Double.NaN else qdps.filter(filter).map(_.lfdr).max
		return new QualityDataPoint(q, fdr)
	}
	def getScans(
			ara:AnubisResultAnalysis, 
			row:Array[QualityDataPoint]):Seq[QualityDataPoint] = {
		var scans = children.map(_.getScans(ara, row)).flatten
		if (average)
			Array(collapse(scans, ara.filter))
		else 
			scans
	}
	def flatten():Seq[RepGroup] = {
		if (!isParent) return Nil
		if (average) return Nil
		var subGroups = children.collect {case rg:RepGroup => rg }
		if (subGroups.length == 0 || subGroups.exists(_.average)) return List(this)
		return subGroups.map(_.flatten).flatten
	}
}

class Scan(
		name:String
) extends Col(name, Nil) {
	override val TYPE_STRING = "SCN"
	def getScans(
			ara:AnubisResultAnalysis, 
			row:Array[QualityDataPoint]):Seq[QualityDataPoint] = 
				Array(ara.getScan(name, row))
}

*/


object RowTree extends AbstractAnyTree {
	type N = Row
	
	abstract class Row(
			id:Any
	) extends AbstractAnyNode {
		var obj = id
		
		def name = obj.toString
		override def nodeName = "ROW " + obj
		override def toString = treeString()
		def getScans(
				ara:PostAnubisAnalysis):Array[QualityDataPoint]
	}
	
	class Group(
			id:Any,
			prots:Seq[Row]
	) extends Row(id) {
		for (p <- prots) addChild(p)
		
		override def nodeName = "GRP " + obj
		def getScans(
				ara:PostAnubisAnalysis):Array[QualityDataPoint] = 
					Array()
	}
	
	class Prot(
			id:Any,
			ptps:Seq[Pep]
	) extends Row(id) {
		for (p <- ptps) addChild(p)
		
		override def nodeName = "PRT " + obj
		def getScans(
				ara:PostAnubisAnalysis):Array[QualityDataPoint] = {
			
			var rs = children.map(_.getScans(ara)).transpose
			rs.map(r => r.foldLeft(QualityDataPoint.ZERO)((a,b) => a.sum(b, ara.filter))).toArray
		}
		def addPeptide(iso:PepIsotope) = {
				children.map(_.asInstanceOf[Pep]).find(
						_.isIsotope(iso)
					) match {
						case Some(pep) => {
							pep.addIsotope(iso)
						}
						case None => addChild(new Pep(iso))
				}
			}
	}
	
	class Pep(
			var light:PepIsotope,
			var heavy:PepIsotope = null
	) extends Row(light.pi.sequence) {
		addChild(light)
		
		override def nodeName = "PEP " + name
		
		implicit def qpda2wrapper(qdpa:Array[QualityDataPoint]) = new QDPArrayWrapper(qdpa)
		implicit def wrapper2qdpa(wrapper:QDPArrayWrapper) = wrapper.peer
		
		def isIsotope(iso:PepIsotope) = light.pi.sequence == iso.pi.sequence
		def addIsotope(iso:PepIsotope) = {
			if (light.pi.mz.d > iso.pi.mz.d) {
				heavy = light
				light = iso
			} else
				heavy = iso
			addChild(iso)
		}
		
		def getScans(
				ara:PostAnubisAnalysis):Array[QualityDataPoint] = {
			var scans = light.getScans(ara)
			if (Pep.useSisLabel) 	scans / heavy.getScans(ara)
			else 					scans
		}
	}
	
	class PepIsotope(
			var pi:PeptideIon
	) extends Row(pi) {
		
		override def nodeName = "ISO " + pi.mz
		
		def getScans(
				ara:PostAnubisAnalysis):Array[QualityDataPoint] = ara.dataMatrix.getRow(pi)
	}
	
	class QDPArrayWrapper(var peer:Array[QualityDataPoint]) {
		def +(other:Array[QualityDataPoint]) = 
			peer.zip(other).map(t => t._1 + t._2)
		def /(other:Array[QualityDataPoint]) = 
			peer.zip(other).map(t => t._1 / t._2)
	}
	
	object Pep {
		def normalize = normalizeScans != null
		var normalizeScans:Array[QualityDataPoint] = null
		var useSisLabel:Boolean = false
	}
}

