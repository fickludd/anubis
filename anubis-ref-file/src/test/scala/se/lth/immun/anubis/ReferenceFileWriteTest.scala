package se.lth.immun.anubis

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import java.io.StringWriter
import java.io.File
import se.lth.immun.xml.XmlWriter

class ReferenceFileWriteTest extends AssertionsForJUnit {

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
		var rf = new ReferenceFile()
		
		rf.write(x)
		
		assertEquals("""<?xml version="1.0"?>
<reference>
  <reference_files count="0"/>
  <precursors count="0"/>
</reference>
""", s.toString)
	}
	*/
/**	@Test
	def writeSimpleFile() = {
		var rf = new ReferenceFile()
		rf.files = Array(new File("/path/to/my/dir", "hej.raw"))
		
		rf.precursors = Array(new ReferencePrecursor(
				30.0, 2, 0, -1, -1, "ABC", "Hello",
				new RetentionTime(20.0, 21.2, 22.4),
				Nil,
				Array(new ReferenceTransition(40.0, "y3", "SIC3"),
						new ReferenceTransition(50.0, "y4", "SIC4")),
				Array(new Ratio(0, 1, 5.5, 3.5))
				))
		
		rf.write(x)
		
		assertEquals("""<?xml version="1.0"?>
<reference>
  <reference_files count="1">
    <reference_file file_id="0" absolute_path="/path/to/my/dir/hej.raw"/>
  </reference_files>
  <precursors count="1">
    <precursor mz="30.0" file_id="0" protein_name="Hello" peptide_sequence="ABC">
      <retention_time start="20.0" peak="21.2" end="22.4"/>
      <measured_transitions count="0"/>
      <ignored_transitions count="2">
        <transition mz="40.0" ion="y3" data_name="SIC3" transition_id="0"/>
        <transition mz="50.0" ion="y4" data_name="SIC4" transition_id="1"/>
      </ignored_transitions>
      <transition_ratios count="1">
        <transition_ratio id1="0" id2="1" mean="5.5" std_dev="3.5"/>
      </transition_ratios>
    </precursor>
  </precursors>
</reference>
""", s.toString)
	}*/
}
