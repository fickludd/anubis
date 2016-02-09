package se.lth.immun.anubis

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import java.util.Calendar
import java.io.StringReader
import se.lth.immun.xml.XmlReader

class ResultFileReadTest extends AssertionsForJUnit {
	
	val valid = """<?xml version="1.0"?>
<results>
  <parameters count="3">
    <parameter name="refFile" value="..\final_method.ref" />
    <parameter name="outFile" value="multirun\results.xml" />
    <parameter name="trailing" value="multirun\*.raw" />
  </parameters>
  <anubis version="1.1.0">
    <working_dir absolute_path="C:/Users/JohanTeleman/all/Data/Streptocock/data2" />
    <reference_file absolute_path="C:/Users/JohanTeleman/all/Data/Streptocock/data2/multirun.ref" />
    <peak_min_width value="0.1" />
    <null_distribution_size value="1000" />
    <num_transition_limit value="4" />
  </anubis>
  <replicate_files count="2">
    <replicate_file file_id="0" run_start_time="25/10/11 12:31:21" absolute_path="C:/Users/JohanTeleman/all/Data/Streptocock/data2/multirun/101112_JT_pl5_25.raw" />
    <replicate_file file_id="1" run_start_time="12/09/11 01:11:59" absolute_path="C:/Users/JohanTeleman/all/Data/Streptocock/data2/multirun/101112_JT_pl5_26.raw" />
  </replicate_files>
  <precursors count="1">
    <precursor mz="507.303124" protein_name="" peptide_sequence="ANELLINVK">
      <replicates count="2">
        <replicate area="1135953.49" file_id="0">
          <transision_areas count="6">
            <transition mz="541.298038" ion="b5" area="38167.9329045528" />
            <transition mz="654.382102" ion="b6" area="45779.7399216393" />
            <transition mz="768.42503" ion="b7" area="2172.63783105185" />
            <transition mz="828.51893" ion="y7" area="251808.083985606" />
            <transition mz="699.476337" ion="y6" area="289141.452417119" />
            <transition mz="586.392273" ion="y5" area="508883.644133674" />
          </transision_areas>
          <retention_time start="26.7961502075195" peak="24.8549842834473" end="28.1653842926025" />
          <quality fdr="0" nbr_conditions="-1" />
        </replicate>
        <replicate area="1098208.33" file_id="1">
          <transision_areas count="6">
            <transition mz="541.298038" ion="b5" area="37662.853175126" />
            <transition mz="654.382102" ion="b6" area="46847.2494914042" />
            <transition mz="768.42503" ion="b7" area="1912.99456245365" />
            <transition mz="828.51893" ion="y7" area="257788.559603716" />
            <transition mz="699.476337" ion="y6" area="274245.843643073" />
            <transition mz="586.392273" ion="y5" area="479750.831978391" />
          </transision_areas>
          <retention_time start="26.3703498840332" peak="26.9800003051758" end="27.9023666381836" />
          <quality fdr="0" nbr_conditions="-1" />
        </replicate>
      </replicates>
    </precursor>
  </precursors>
</results>"""
	
	var x:XmlReader = null
	var s:StringReader = null

	@Before
	def setupReader() = {
		s = new StringReader(valid)
		x = new XmlReader(s)
	}


	@Test
	def readValidFile() = {
		var rf = new ResultFile(x)
		
		assertEquals(3, rf.parameters.length)
		assertEquals("trailing", rf.parameters(2).name)
		assertEquals("multirun\\*.raw", rf.parameters(2).value)
		
		assertEquals("1.1.0", rf.anubisVersion)
	//	assertEquals("file:/C:/Users/JohanTeleman/all/Data/Streptocock/data2", rf.workingDir.toURI.toString)
	//	assertEquals("file:/C:/Users/JohanTeleman/all/Data/Streptocock/data2/multirun.ref", rf.referenceFile.toURI.toString)
		assertEquals(0.1, rf.peakMinWidth, 0.00001)
		
		assertEquals(2, rf.replicateFiles.length)
	//	assertEquals("file:/C:/Users/JohanTeleman/all/Data/Streptocock/data2/multirun/101112_JT_pl5_25.raw", rf.replicateFiles(0).file.toURI.toString)
		var cal = Calendar.getInstance
		cal.clear
		cal.set(2011, 9, 25, 12, 31, 21)
		assertEquals(cal.getTime, rf.replicateFiles(0).runStartTime)
		
		assertEquals(1, rf.precursors.length)
		var pc = rf.precursors(0)
		assertEquals(507.303124, pc.mz, 0.00001)
		assertEquals("", pc.proteinName)
		assertEquals("ANELLINVK", pc.peptideSequence)
		
		assertEquals(2, pc.replicates.length)
		var r = pc.replicates(1)
		assertEquals(1098208.33, r.totalArea, 0.00001)
		assertEquals(1, r.fileId)
		
		assertEquals(6, r.transitions.length)
		var t = r.transitions(1)
		assertEquals(654.382102, t.mz, 0.00001)
		assertEquals("b6", t.ion)
		assertEquals(46847.2494914042, t.area, 0.00001)
		
		var rt = r.retentionTime
		assertEquals(26.3703498840332, rt.start, 0.00001)
		assertEquals(26.9800003051758, rt.peak, 0.00001)
		assertEquals(27.9023666381836, rt.end, 0.00001)
		
		var q = r.quality
		assertEquals(0, q.fdr, 0.001)
		assertEquals(-1, q.nOConds)
	}

}