package se.lth.immun.anubis

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import se.lth.immun.math.RatioIndex
import se.lth.immun.math.Ratios

class PeakCandidateFinderTest {

	var pcf:GraphPeakCandidateFinder = null
	
	var ris1 = Array(
			new RelationInterval(1, 6, new RatioIndex(0, 1, 0), 3, 0, 0, 0),
			new RelationInterval(2, 13, new RatioIndex(0, 2, 1), 1.4, 0, 0, 0),
			new RelationInterval(12, 16, new RatioIndex(0, 1, 0), 3, 0, 0, 0)
			)
			
	var ris2 = Array(
			new RelationInterval(1, 6, new RatioIndex(0, 1, 0), 3, 0, 0, 0),
			new RelationInterval(4, 10, new RatioIndex(0, 2, 1), 1.4, 0, 0, 0),
			new RelationInterval(6, 17, new RatioIndex(0, 3, 2), 3, 0, 0, 0),
			new RelationInterval(11, 18, new RatioIndex(0, 1, 0), 3, 0, 0, 0)
			)
			
	@Before
	def setup = {
		pcf = new GraphPeakCandidateFinder(
				new Ratios(2), 
				Array(Array(1.0)), 
				Array(new Ratio(0, 1, 1.0, 0.0)), 
				1
			)
	}
	
	@Test
	def findEdgesLoose = {
				
		var e = pcf.findEdgesLoose(ris1)
		
		assert(e != null)
		assertEquals(3, e.length)
		assertEquals(1, e(0).length)
		assertEquals(1, e(0).head)
		assertEquals(1, e(1).length)
		assertEquals(2, e(1).head)
		assertEquals(0, e(2).length)
	}
	
	@Test
	def findEdgesLoose2 = {
				
		var e = pcf.findEdgesLoose(ris2)
		
		assert(e != null)
		assertEquals(4, e.length)
		assertEquals(1, e(0).length)
		assertEquals(1, e(0).head)
		assertEquals(1, e(1).length)
		assertEquals(2, e(1).head)
		assertEquals(1, e(2).length)
		assertEquals(3, e(2).head)
		assertEquals(0, e(3).length)
	}
	
	@Test
	def findEdgesRestrictive = {
				
		var e = pcf.findEdgesRestrictive(ris1)
		
		assert(e != null)
		assertEquals(3, e.length)
		assertEquals(1, e(0).length)
		assertEquals(1, e(0).head)
		assertEquals(0, e(1).length)
		assertEquals(0, e(2).length)
	}
	
	@Test
	def findEdgesRestrictive2 = {
				
		var e = pcf.findEdgesRestrictive(ris2)
		
		assert(e != null)
		assertEquals(4, e.length)
		assertEquals(0, e(0).length)
		assertEquals(1, e(1).length)
		assertEquals(2, e(1).head)
		assertEquals(1, e(2).length)
		assertEquals(3, e(2).head)
		assertEquals(0, e(3).length)
	}
}