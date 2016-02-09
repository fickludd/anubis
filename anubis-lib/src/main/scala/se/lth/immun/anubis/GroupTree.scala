package se.lth.immun.anubis

import se.lth.immun.collection.AbstractTree
import java.io.File
import scala.io.Source

object GroupTree extends AbstractTree {
	type N = Node
	
	class Node(
			var name:String, 
			var metaType:String = null,
			var value:Double = Double.NaN
	) extends AbstractNode {
		
		override def nodeName = name
		override def toString = treeString()
		
	}
	
	def fromFile(f:File) = {
		val root = new Node("root")
		def addTo(p:Node, names:List[String]):Boolean = 
		  names match {
		  case Nil => true
		  case n::Nil => 
		    if (!p.children.exists(_.name == n))
		      p.addChild(new Node(n))
		    true
		  case n::ns =>
		    p.children.find(_.name == n) match {
		      case Some(child) => addTo(child, ns)
		      case None => false
		    }
		}
		
		for (path <- Source.fromFile(f).getLines.map(_.split("\t").toList)) {
		  addTo(root, path)
		}
		root
	}
}

