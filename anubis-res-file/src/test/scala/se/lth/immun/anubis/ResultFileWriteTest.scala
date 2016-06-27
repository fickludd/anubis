package se.lth.immun.anubis

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import java.util.Calendar
import java.io.StringWriter
import java.io.File
import se.lth.immun.xml.XmlWriter

class ResultFileWriteTest extends AssertionsForJUnit {

	var s:StringWriter = null
	var x:XmlWriter = null
/**	
	@Before
	def setupWriters() = {
		s = new StringWriter()
		x = new XmlWriter(s)
	}
	
	@Test
	def writeEmptyFile() = {
		var rf = new ResultFile()
		
		rf.write(x)
		
		assertEquals("""<?xml version="1.0"?>
<results>
  <parameters count="0"/>
  <anubis>
    <singleAnswer value="true"/>
    <pValueTolerance value="0.01"/>
  </anubis>
  <replicate_files count="0"/>
  <precursors count="0"/>
</results>
""", s.toString)
	}
	
	@Test
	def writeFile() = {
	var rf = new ResultFile()
		
		rf.parameters = Array(new ResultParameter("a", "1"))
		
		rf.anubisVersion = "1.1.0"
		rf.workingDir = new File("/dir/of/my/file")
		rf.referenceFile = new File("/dir/of/my/file", "hmm.ref")
		rf.peakMinWidth = 0.1
		rf.nullDistributionSize = 100
		rf.transitionLimit = 3
		
		
		var cal = Calendar.getInstance
		cal.clear
		cal.set(2011, 9, 25, 12, 31, 21)
		rf.replicateFiles = Array(
				new ResultMzMLFile(
					new File("/dir/of/my/file", "hmm.raw"),
					cal.getTime))
		rf.precursors = Array(new ResultPrecursor(1, "hello", "ABC",
				Array(new ResultReplicate(100, 0, 
						Array(new ResultTransition(200, "y1", 40)),
						new ResultRetentionTime(13.1, 14.1, 15.1),
						new ResultQuality(0.01, 1),
						true
				))))
		
		rf.write(x)
		
		assertEquals("""<?xml version="1.0"?>
<results>
  <parameters count="1">
    <parameter name="a" value="1"/>
  </parameters>
  <anubis version="1.1.0">
    <working_dir absolute_path="/dir/of/my/file"/>
    <reference_file absolute_path="/dir/of/my/file/hmm.ref"/>
    <peak_min_width value="0.1"/>
    <null_distribution_size value="100"/>
    <num_transition_limit value="3"/>
    <singleAnswer value="true"/>
    <pValueTolerance value="0.01"/>
  </anubis>
  <replicate_files count="1">
    <replicate_file file_id="0" absolute_path="/dir/of/my/file/hmm.raw" run_start_time="25/10/11 12:31:21"/>
  </replicate_files>
  <precursors count="1">
    <precursor mz="1.0" protein_name="hello" peptide_sequence="ABC">
      <replicates count="1">
        <replicate area="100.0" file_id="0" peakBeforeWindowFlag="">
          <transision_areas count="1">
            <transition mz="200.0" ion="y1" area="40.0"/>
          </transision_areas>
          <retention_time start="13.1" peak="14.1" end="15.1"/>
          <quality p_value="0.01" nbr_conditions="1"/>
        </replicate>
      </replicates>
    </precursor>
  </precursors>
</results>
""", s.toString)
	}*/
}
