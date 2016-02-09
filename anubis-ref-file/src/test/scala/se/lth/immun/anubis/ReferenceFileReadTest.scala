package se.lth.immun.anubis

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import se.lth.immun.xml.XmlReader
import java.io.StringReader
import java.io.File
import java.io.FileReader
import java.io.BufferedReader

class ReferenceFileReadTest extends AssertionsForJUnit {
	
	val valid = """<?xml version="1.0"?>
<reference>
  <reference_files count="1">
    <reference_file file_id="0" absolute_path="/path/to/my/dir/hej.raw" />
  </reference_files>
  <precursors count="1">
	<precursor mz="507.303124" file_id="0" protein_name="Hello" peptide_sequence="ABC">
      <retention_time start="33.333" peak="33.444" end="34.555" />
      <measured_transitions count="2">
      	<transition mz="541.298038" ion="b5" data_name="SIC1" transition_id="0" />
        <transition mz="654.382102" ion="b6" data_name="SIC2" transition_id="1" />
      </measured_transitions>
      <ignored_transitions count="0" />
      <transition_ratios count="1">
        <transition_ratio id1="0" id2="1" mean="1.39669380015338" std_dev="1.18884798746988" />
      </transition_ratios>
    </precursor>
  </precursors>
</reference>
	"""
	
	var x:XmlReader = null
	var s:StringReader = null

	@Before
	def setupReader() = {
		s = new StringReader(valid)
		x = new XmlReader(s)
	}
	
/**	@Test
	def readValidFile() = {
		var rf = ReferenceFile.fromFile(x)
		
		assertEquals(1, rf.files.length)
		assertEquals("file:/path/to/my/dir/hej.raw", rf.files(0).toURI.toString)
		
		assertEquals(1, rf.precursors.length)
		var pc = rf.precursors(0)
		assertEquals(507.303124, pc.mz, 0.0001)
		assertEquals(0, pc.fileId)
		assertEquals("Hello", pc.proteinName)
		assertEquals("ABC", pc.peptideSequence)
		
		var rt = pc.retentionTime
		assertEquals(33.333, rt.start, 0.00001)
		assertEquals(33.444, rt.peak, 0.00001)
		assertEquals(34.555, rt.end, 0.00001)
		
		assertEquals(2, pc.measuredTransitions.length)
		assertEquals(654.382102, pc.measuredTransitions(1).mz, 0.000001)
		assertEquals("b6", pc.measuredTransitions(1).ion)
		assertEquals("SIC2", pc.measuredTransitions(1).dataName)
		
		assertEquals(0, pc.ignoredTransitions.length)
		
		assertEquals(1, pc.ratios.length)
		assertEquals(0, pc.ratios(0).transitionId1)
		assertEquals(1, pc.ratios(0).transitionId2)
		assertEquals(1.3966938, pc.ratios(0).mean, 0.000001)
		assertEquals(1.18884798, pc.ratios(0).stdDev, 0.000001)
	}

	@Test
	def readRealFile() = {
		var r = new XmlReader(new BufferedReader(new FileReader(new File("target/test-classes/final_method.ref"))))
		var rf = ReferenceFile.fromFile(r)
		
		assertEquals(19, rf.files.length)
		assertEquals("C:/Users/JohanTeleman/all/Data/Streptocock/ADH/101104_strep_ADH200.raw", rf.files(0).toString)
		
		assertEquals(162, rf.precursors.length)
		var pc = rf.precursors(0)
		assertEquals(507.303124, 	pc.mz, 				0.0001)
		assertEquals(0, 			pc.fileId)
		assertEquals("", 			pc.proteinName)
		assertEquals("ANELLINVK", 	pc.peptideSequence)
		
		var rt = pc.retentionTime
		assertEquals(33.779899597168, 	rt.start, 	0.00001)
		assertEquals(33.8980484008789, 	rt.peak, 	0.00001)
		assertEquals(34.1340484619141, 	rt.end, 	0.00001)
		
		assertEquals(6, 			pc.measuredTransitions.length)
		assertEquals(654.382102, 	pc.measuredTransitions(1).mz, 0.000001)
		assertEquals("b6", 			pc.measuredTransitions(1).ion)
		assertEquals("SRM SIC : 507.303, 654.382", pc.measuredTransitions(1).dataName)
		
		assertEquals(0, pc.ignoredTransitions.length)
		
		assertEquals(15, 	pc.ratios.length)
		assertEquals(0, 	pc.ratios(3).transitionId1)
		assertEquals(4, 	pc.ratios(3).transitionId2)
		assertEquals(0.132128539082834, 	pc.ratios(3).mean, 0.000001)
		assertEquals(0.00233096655425449, 	pc.ratios(3).stdDev, 0.000001)
	}
	*/
}