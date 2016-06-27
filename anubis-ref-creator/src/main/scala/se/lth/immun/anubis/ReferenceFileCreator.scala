package se.lth.immun.anubis

import swing._
import swing.event._ 

import se.lth.immun.proteomics.PeptideIon
import se.lth.immun.proteomics.Mz
import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.files.DelimitedReader
import se.lth.immun.files.SparseDelimitedReader
import se.lth.immun.app.CLIApplication
import se.lth.immun.app.CommandlineArgumentException
import se.lth.immun.app.LogFile

import java.io.FileReader
import java.io.FileWriter
import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.IOException
import java.util.zip.GZIPInputStream
import java.util.Properties

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import se.lth.immun.mzml.ghost.XMzML
import se.lth.immun.mzml.ghost.XChromatogram
import se.lth.immun.mzml.ghost.XChromatogramGroup
import se.lth.immun.traml.ghost.GhostTraML

import se.lth.immun.graphs.LineGraph
import se.lth.immun.graphs.util.Curve2
import se.lth.immun.graphs.util.Annotation
import se.lth.immun.graphs.util.XAnnotation
import se.lth.immun.graphs.util.HeightBoxAnnotation


object ReferenceFileCreator extends SimpleSwingApplication with CLIApplication {
	
	
	// constants
	val NONE_STRING = "---"
	val EMPTY_STATUS = "---"
		
	val iPROTEIN = 0
	val iSEQUENCE = 1
	val iMZ = 2
	val iCOVERAGE = 3
	val iSOURCE = 4
	val numCOLUMNS = 5
	
	// meta
    var tlType = "unsched"
    var keepUnMeasured = false
    var allDDB = false
    var currPC:ReferencePrecursor = null
    val q3tolerance = 0.05
    
   
    // files
    var tl				:File 					= null
    var referenceFile	:ReferenceFile 			= null
    var globalMzmlFile	:File 					= null
    var outFile			:File 					= null
    var out				:XmlWriter 				= null
    var mzmlFiles 								= new HashMap[String, XMzML]() 
    var chromGroup		:XChromatogramGroup 	= null
    
    
    // swing
    var mainFrame		:MainFrame 		= null
	var mzmlFileChooser	:FileChooser 	= null
    var mzmlFileTF 				= new TextField("- Enter mzML file -") 
	var mzmlFileButton 			= new Button("Get MzML for selected peptides")
	var clearButton 			= new Button("Clear selected transitions")
	var transitionListButton 	= new Button("Load transition list")
	var selectZeroCoverageButton		= Button("Select zero coverage") { selectZeroCoverage }
	var schedComboBox 			= new ComboBox(List("unsched", "sched", "sparse"))
	var transitionListFileChooser	:FileChooser = null
	var peptideTable	:Table 			= null
	var tableModel		:JTTableModel 	= null
	var status			:Label 			= null
	var saveButton		:Button 		= null
	var cancelButton	:Button 		= null
	var outFileChooser	:FileChooser 	= null
	var chromGraph		:LineGraph 		= null
	var mouseZoomCB		= new CheckBox("mouse zoom")
	var zoomBestPeakCB	= new CheckBox("zoom best peak")
	var defineReference	:Button 		= null
	
	var properties = new Properties
    properties.load(this.getClass.getResourceAsStream("/pom.properties"))
	var name 		= properties.getProperty("pom.name")
	var version 	= properties.getProperty("pom.version")
	
		
	
    
	override def main(args:Array[String]):Unit = {
		opt(
				"transition-list-type", 
				"Type of csv file, default unsched", 
				s => tlType = s, 
				"unsched|sched|sparse")
		opt(
				"keep-unmeasured", 
				"Store unmeasured transitions in the reference file", 
				s => keepUnMeasured = true)
		opt(
				"in-file", 
				"Mzml file to use for all peptide reference ratios", 
				s => globalMzmlFile = new File(s), 
				"MZML_FILE")
		opt(
				"out-file", 
				"Output reference file to create", 
				s => outFile = new File(s), 
				"REF_FILE")
		opt(
				"transition-list", 
				"transition list file, either as csv of traML", 
				s => loadTLFile(new File(s), tlType), 
				"TRANSITION_LIST_CSV|TRAML")
		
		try {
			parseArgs("ReferenceFileCreator-"+version, args)
		} catch {
			case e:CommandlineArgumentException => 
				return
		}
		
		if (referenceFile == null)
			referenceFile = new ReferenceFile
		
		var currWorkingDir = new File("./")
		mzmlFileChooser = new FileChooser(currWorkingDir) {
			//fileFilter = new javax.swing.filechooser.FileNameExtensionFilter("MzML file", "mzML", "mzML.gz", "mzml", "mzml.gz")
		}
		transitionListFileChooser = new FileChooser(currWorkingDir) {}
		outFileChooser = new FileChooser(currWorkingDir) {
			fileFilter = new javax.swing.filechooser.FileNameExtensionFilter("Reference file", "ref")
		}
		tableModel = new JTTableModel(
				convertReferenceFileData,
				Array("Protein", "Peptide", "m/z", "Coverage", "Raw file")
			)
		
		if (globalMzmlFile != null && globalMzmlFile.exists && outFile != null) {
			referenceFile.files = Array(globalMzmlFile)
			for (pc <- referenceFile.precursors)
				pc.fileId = 0
			ReferenceCalculator.calculate(referenceFile, pc => {})
			saveAndQuit
		}
		
				
		super.main(args)
	}



	def selectZeroCoverage = {
		peptideTable.selection.rows.clear
		peptideTable.selection.rows ++= tableModel._data.zipWithIndex.filter(_._1(iCOVERAGE).toString.startsWith("0")).map(_._2)
	}
	
	
	
	
	def saveAndQuit() = {
		out = new XmlWriter(new FileWriter(outFile),() => 0L, () => "")
		if (!keepUnMeasured)
			referenceFile.precursors = referenceFile.precursors.filter(_.measuredTransitions.length > 1)
		referenceFile.write(out)
		quit()
	}
	
	
	
	
	def top = {
		mainFrame = new MainFrame {
			title = name + " "+version + (if (tl != null) " - "+tl.getCanonicalPath else "")
			contents = new BorderPanel {
				focusable = true
				minimumSize = new Dimension(1024, 800)
				preferredSize = new Dimension(1024, 800)
				
				import BorderPanel.Position._
				layout(new FlowPanel {
						contents += schedComboBox
						contents += transitionListButton
						contents += clearButton
						contents += selectZeroCoverageButton
				}) = North
				
				peptideTable = new Table {
						model = tableModel
					}
				chromGraph 	= new LineGraph {
						minimumSize 	= new Dimension(1, 300)
						preferredSize 	= new Dimension(Int.MaxValue, 300)
						maximumSize 	= new Dimension(Int.MaxValue, 300)
						cacheRender 	= true
						style.backgroundColor = java.awt.Color.WHITE
					}
				layout(new BoxPanel(Orientation.Vertical) {
					contents += new ScrollPane {
						import ScrollPane.BarPolicy._
						horizontalScrollBarPolicy 	= AsNeeded
						verticalScrollBarPolicy 	= AsNeeded
						contents = peptideTable
						listenTo(peptideTable)
						reactions += {
							case r:UIElementResized => {
								repaint
							}
						}
					}
					contents += new FlowPanel {
						contents += mzmlFileTF
						contents += mzmlFileButton
						minimumSize 	= new Dimension(1, 30)
						preferredSize 	= new Dimension(Int.MaxValue, 30)
						maximumSize 	= new Dimension(Int.MaxValue, 30)
					}
					//Swing.RigidBox(new Dimension(0, 5))
					contents += chromGraph
				}) = Center
				
				status = new Label(EMPTY_STATUS)
				mouseZoomCB = new CheckBox("mouse zoom")
				chromGraph.mouseZoom = mouseZoomCB.selected
				mouseZoomCB.action = Action("mouse zoom") {
					chromGraph.mouseZoom = mouseZoomCB.selected
				}
				defineReference = new Button("set reference")
				defineReference.enabled = false
				saveButton = new Button("Save")
				cancelButton = new Button("Cancel")
				layout(new BorderPanel {
					layout(status) = Center
					layout(new FlowPanel {
						contents += zoomBestPeakCB
						contents += mouseZoomCB
						contents += defineReference
						contents += cancelButton
						contents += saveButton
					}) = East
				}) = South
				
			} // contents
			
			
			listenTo(mzmlFileButton.mouse.clicks		)
			listenTo(clearButton.mouse.clicks			)
			listenTo(transitionListButton.mouse.clicks	)
			listenTo(saveButton.mouse.clicks			)
			listenTo(cancelButton.mouse.clicks			)
			listenTo(defineReference.mouse.clicks		)
			listenTo(mzmlFileTF.keys					)
			listenTo(peptideTable.selection				)
			reactions += {
				case KeyPressed(_, key, _, _) => onKeyPress(key)
				case mc:MouseClicked => onMouseClick(mc)
				case trs:TableRowsSelected => onSelectionChanged(trs)
			}
			
			import scala.swing.event.Key._
			def onKeyPress(keyCode: Value) = {
				keyCode match {
				    case Enter => {
				    	var file = new File(mzmlFileTF.text)
				    	if (file.exists) {
					    	for (row <- peptideTable.selection.rows)
								peptideTable.update(row, iSOURCE, file)
							peptideTable.repaint
							status.text = EMPTY_STATUS
				    	} else {
				    		status.text = "The mzML file '"+file.toString+"' does not exist"
				    	}
				    }
				    case _ => {}
				}
			}
			
			
			
			def onMouseClick(mc:MouseClicked) = {
				// pick mzML file
				if (mc.source == mzmlFileButton) {
					mzmlFileChooser.showDialog(contents(0), "Select a mzML file")
			
					if (mzmlFileChooser.selectedFile != null) {
						try {
							mzmlFileTF.text = mzmlFileChooser.selectedFile.toString
							var mzmlFile = parseFile(new File(mzmlFileTF.text))
							mzmlFiles += mzmlFileTF.text -> mzmlFile
							for (row <- peptideTable.selection.rows) {
								peptideTable.update(row, iSOURCE, mzmlFileChooser.selectedFile)
								var arow = tableModel._data(row)
								var pc = getReferencePrecursor(arow).get
								ReferenceCalculator.fill(pc, mzmlFile)
								handlePeptideUpdate(pc)
							}
							peptideTable.repaint
							status.text = EMPTY_STATUS
						} catch {
							case e:IOException => {
								e.printStackTrace
								status.text = e.getMessage
							}
						}
					}
					
				// clear selected
				} else if (mc.source == clearButton) {
					for (row <- peptideTable.selection.rows) {
						var arow = tableModel._data(row)
						var pc = getReferencePrecursor(arow).get
						pc.ignoredTransitions = pc.allTransitions
						pc.measuredTransitions = Nil
						peptideTable.update(row, iSOURCE, NONE_STRING)
						peptideTable.update(row, iCOVERAGE, makeCoverageString(pc))
					}
					peptideTable.repaint
					status.text = EMPTY_STATUS
				
				// get new transition list
				} else if (mc.source == transitionListButton) {
					transitionListFileChooser.showDialog(contents(0), "Select a transition list file (csv or traML)")
					
					if (transitionListFileChooser.selectedFile != null) {
						loadTLFile(transitionListFileChooser.selectedFile, schedComboBox.selection.item)
						title = "ReferenceFileCreator - "+tl.getCanonicalPath
						tableModel.setData(convertReferenceFileData)
					}
				
				// define reference
				} else if (mc.source == defineReference) {
					if (chromGraph.selection.isDefined) {
						var i = chromGraph.selection.get
						var start 	= Math.min(i.x0, i.xn)
						var end 	= Math.max(i.x0, i.xn)
						
						var peakSignal = getPart(start, end).toArray
						var peak = ReferenceCalculator.peakFinder.findPeak(peakSignal)
						
						ReferenceCalculator.fillRatios(currPC, peakSignal)
						var times = getPartTimes(start, end)
			            currPC.retentionTime.start = times.head
			            currPC.retentionTime.peak = times(peak.peak)
			            currPC.retentionTime.end = times.last
			            
			            annotateChromatogram(currPC.retentionTime)
			            chromGraph.clearSelection
			            chromGraph.repaint
					}
					
				// save and quit
				} else if (mc.source == saveButton) {
					if (outFile == null) {
						outFileChooser.showDialog(contents(0), "Save reference file")
						outFile = outFileChooser.selectedFile
					}
					if (outFile != null) {
						if (!outFile.getName.toLowerCase.endsWith(".ref"))
							outFile = new File(outFile.toString + ".ref")
						for (row <- tableModel._data) {
							var pc = getReferencePrecursor(row).get
							if (row(iSOURCE) != NONE_STRING) {
								var file:File = row(iSOURCE).asInstanceOf[File]
								if (!referenceFile.files.contains(file)) referenceFile.files = referenceFile.files :+ file
								pc.fileId = referenceFile.files.indexOf(file)
							}
						}
						//ReferenceCalculator.calculate(referenceFile, handlePeptideUpdate)
						peptideTable.repaint
						
						out = new XmlWriter(new FileWriter(outFile),() => 0L, () => "")
						
						if (!keepUnMeasured) {
							var pcs = referenceFile.precursors 
							referenceFile.precursors = referenceFile.precursors.filter(_.measuredTransitions.length > 1)
							referenceFile.write(out)
							referenceFile.precursors = pcs
						} else 
							referenceFile.write(out)
					}
					status.text = outFile.toString + " saved successfully."
				
				// cancel and quit
				} else if (mc.source == cancelButton)	quit
			}
			
			
			
			def onSelectionChanged(trs:TableRowsSelected) = {
				if (peptideTable.selection.rows.size > 0) {
					var r = peptideTable.selection.rows.head
					var arow = tableModel._data(r)
					currPC = getReferencePrecursor(arow).get
					var source = arow(iSOURCE).toString
					if (mzmlFiles.contains(source)) {
						var mzmlFile = mzmlFiles(source)
						defineReference.enabled = true
						loadChromatogram(currPC, mzmlFile, source)
					} else {
						defineReference.enabled = false
						chromGraph.clear
						chromGraph.repaint
					}
				} else
					currPC = null
			}
		}
		mainFrame
	} // top
	
	
	
	
	def handlePeptideUpdate(pc:ReferencePrecursor):Unit = {
		status.text = "Updated "+pc.peptideSequence+" in "+pc.proteinName
		(tableModel._data.find(row => 
				row(iPROTEIN) == pc.proteinName && 
				row(iSEQUENCE) == pc.peptideSequence && 
				row(iMZ) == pc.mz
								).get)(iCOVERAGE) = makeCoverageString(pc)
	}
	
	
	
	def parseFile(file:File):XMzML = {
    	var fileName = file.getName
    	if (fileName.toLowerCase.endsWith(".mzml.gz"))
        	try {
        		return XMzML.fromFile(
        				new XmlReader(
        					new BufferedReader(new InputStreamReader(
		    					new GZIPInputStream(new FileInputStream(file))
		    				))
        				))
        	} catch {
        		case e:Exception => 
        			throw new IOException("Failed parsing gzipped file '"+file+
        									"': "+e.getLocalizedMessage)
        	}
        else if (fileName.toLowerCase.endsWith(".mzml"))
        	try {
        		return XMzML.fromFile(new XmlReader(new BufferedReader(new FileReader(file))))
        	} catch {
        		case e:Exception => {
        			e.printStackTrace
        			throw new IOException("Failed parsing file '"+file+
        									"': "+e.getLocalizedMessage)
        		}
        	}
        else
        	throw new IOException("Unsupported file extension. 'mzML' or "+
        							"'.mzML.gz' required")
    }
	
	
	
	
	def makeCoverageString(pc:ReferencePrecursor):String = 
		pc.measuredTransitions.length + " / " + pc.allTransitions.length
		
		
		
	
	def getReferencePrecursor(arow:Seq[Any]) = 
		referenceFile.precursors.find(
									pc => pc.proteinName == arow(iPROTEIN) && 
									pc.peptideSequence == arow(iSEQUENCE) && 
									pc.mz == arow(iMZ).asInstanceOf[Double]
								)
	
		
		
		
	def loadTLFile(tl:File, tlType:String) = {
		this.tl = tl
		if (tl.getName.toLowerCase.endsWith(".traml"))
			referenceFile = readTraMLFile(GhostTraML.fromFile(
					new XmlReader(new BufferedReader(new FileReader(tl)))
				))
		else
			referenceFile = tlType match {
				case "unsched" => readUnscheduledList(new DelimitedReader(',', '"', 
														new BufferedReader(new FileReader(tl))))
				case "sched" => readScheduledList(new DelimitedReader(',', '"', 
														new BufferedReader(new FileReader(tl))))
				case "sparse" => readSparseList(new SparseDelimitedReader(',', '"', 
														new BufferedReader(new FileReader(tl))))
				case a => throw new CommandlineArgumentException("Unrecognised transition list formating '"+a+"'")
			}
	}
	
	
	
	
	def convertReferenceFileData = {
		referenceFile.precursors.map(
					pc => {
						var a:Array[AnyRef] = new Array(numCOLUMNS)
						a(iPROTEIN) = pc.proteinName
						a(iSEQUENCE) = pc.peptideSequence
						a(iMZ) = pc.mz.asInstanceOf[AnyRef]
						a(iCOVERAGE) = makeCoverageString(pc)
						a(iSOURCE) = 
							if (globalMzmlFile != null && globalMzmlFile.exists) 	globalMzmlFile 
							else 													NONE_STRING
						a.toList;
					}
			).toList
	}
	
	
	
	
	def readTraMLFile(
				gt:GhostTraML
			):ReferenceFile = {
		
		import se.lth.immun.traml.ghost.GhostTransition
		
		def is(pc:ReferencePrecursor, t:GhostTransition):Boolean = 
			 pc.peptideSequence == gt.peptides(t.peptideRef).sequence && math.abs(pc.mz - t.q1) < q3tolerance
		
		var precursors = new ArrayBuffer[ReferencePrecursor]
		for (t <- gt.transitions) {
			if (!precursors.exists(is(_, t))) 
				precursors += new ReferencePrecursor(
								t.q1, -1, -1, -1, -1, 
								gt.peptides(t.peptideRef).sequence, 
								gt.proteins(gt.peptides(t.peptideRef).proteins(0)).name
							)
			precursors.find(is(_, t)) match {
				case Some(pc) => pc.ignoredTransitions = 
					pc.ignoredTransitions :+ new ReferenceTransition(t.q3, t.ions(0))
				case None => {}
			}
		}
		
		return new ReferenceFile(Array(), precursors.toArray)
	}
			
	
	
			
	def readUnscheduledList(
				tlReader:DelimitedReader 
			):ReferenceFile = {
		
		def is(pc:ReferencePrecursor, pi:PeptideIon):Boolean = 
			 new PeptideIon(new Mz(pc.mz, q3tolerance), pc.peptideSequence) == pi
		
		var precursors:List[ReferencePrecursor] = Nil
		
		var row = tlReader.readRow
		while (row.length > 0) {
			var pi = new PeptideIon(new Mz(row(0).toDouble, q3tolerance), row(4))
			var prot = row(5)
			var q3 = row(1).toDouble
			var ion = row(6)
			if (!precursors.exists(is(_, pi))) 
				precursors = precursors :+ new ReferencePrecursor(pi.mz.d, -1, -1, -1, -1, pi.sequence, prot)
			precursors.find(is(_, pi)) match {
				case Some(pc) => pc.ignoredTransitions = pc.ignoredTransitions :+ new ReferenceTransition(q3, ion)
				case None => {}
			}
			row = tlReader.readRow
		}
		
		return new ReferenceFile(Array(), precursors.toArray)
	}
    
    
    
	
	def readScheduledList(
				tlReader:DelimitedReader 
			):ReferenceFile = {
		
		def is(pc:ReferencePrecursor, pi:PeptideIon):Boolean = 
			 new PeptideIon(new Mz(pc.mz, q3tolerance), pc.peptideSequence) == pi
		
		var precursors:List[ReferencePrecursor] = Nil
		
		var row = tlReader.readRow
		while (row.length > 0) {
			var pi = new PeptideIon(new Mz(row(0).toDouble, q3tolerance), row(6))
			var prot = row(7)
			var q3 = row(1).toDouble
			var ion = row(8)
			if (!precursors.exists(is(_, pi))) 
				precursors = precursors :+ new ReferencePrecursor(pi.mz.d, -1, -1, -1, -1, pi.sequence, prot)
			precursors.find(is(_, pi)) match {
				case Some(pc) => pc.ignoredTransitions = pc.ignoredTransitions :+ new ReferenceTransition(q3, ion)
				case None => {}
			}
			row = tlReader.readRow
		}
		
		return new ReferenceFile(Array(), precursors.toArray)
	}
    
    
    
	
	def readSparseList(
				tlReader:SparseDelimitedReader 
			):ReferenceFile = {
		
		def is(pc:ReferencePrecursor, pi:PeptideIon):Boolean = 
			 new PeptideIon(new Mz(pc.mz, q3tolerance), pc.peptideSequence) == pi
		
		var precursors:List[ReferencePrecursor] = Nil
		
		var row = tlReader.readRow
		while (row.length > 0) {
			var pi = new PeptideIon(new Mz(row(2).toDouble, q3tolerance), row(1))
			var prot = row(0)
			var q3 = row(3).toDouble
			var ion = row(4)
			if (!precursors.exists(is(_, pi))) 
				precursors = precursors :+ new ReferencePrecursor(pi.mz.d, -1, -1, -1, -1, pi.sequence, prot)
			precursors.find(is(_, pi)) match {
				case Some(pc) => pc.ignoredTransitions = pc.ignoredTransitions :+ new ReferenceTransition(q3, ion)
				case None => {}
			}
			row = tlReader.readRow
		}
		
		return new ReferenceFile(Array(), precursors.toArray)
	}
	
	
	
	
	def loadChromatogram(pc:ReferencePrecursor, xmzml:XMzML, source:String):Unit = {
		
		status.text = "Drawing "+pc+" from file '"+xmzml+"'"
		def error(str:String):Unit = {
			status.text = str
			chromGraph.clear
			chromGraph.repaint
		}
		
		chromGroup = ReferenceCalculator.extractTransitionData(pc, xmzml) match {
			case Some(cg) => cg
			case None => return error(pc + " not found in file "+source)
		}
		
		def toCurve(c:XChromatogram):Curve2[Double, Double] = 
			new Curve2(
					c.times,
					c.intensities,
					c.intensities.map(d => java.lang.Double.isNaN(d)),
					"%.2f".format(c.q3)
					)
		
		chromGraph.setCurves(chromGroup.chromatograms.map(c => toCurve(c)))
		chromGraph.title = pc.peptideSequence + " mz=" + pc.mz + " - " + source
		
		annotateChromatogram(pc.retentionTime)
		
		if (zoomBestPeakCB.selected) {
			var times = chromGroup.chromatograms.head.times
			var padding = 2*pc.retentionTime.width
			chromGraph.setZoom(
					math.max(times.head, pc.retentionTime.start - padding),
					math.min(times.last, pc.retentionTime.end + padding)
					)
		}
		

		status.text = pc.peptideSequence + 
					" mz=" + pc.mz
		chromGraph.repaint
	}
	
	
	
	def annotateChromatogram(rt:RetentionTime) = {
		chromGraph.annotations = Nil
		if (rt.start > 0 && rt.end > 0) {
			var qTraces:ArrayBuffer[Array[Double]] = getPart(rt.start, rt.end)
			if (!qTraces(0).isEmpty) {
				var max = qTraces.map(_.max).max
				chromGraph.addAnnotation(new HeightBoxAnnotation(
											rt.start, 	max, 
											rt.end, 		
											Annotation.BACKGROUND) {
										background = true
									})
			}
		}
	}
	
	
	
	def getPart(start:Double, end:Double) = {
		chromGroup.chromatograms.map(tr => 
					tr.intensities.zip(tr.times)
						.dropWhile(_._2 < start)
						.takeWhile(_._2 < end)
						.map(_._1).toArray)
	}
	
	
	def getPartTimes(start:Double, end:Double) = {
		chromGroup.chromatograms(0).times
						.dropWhile(_ < start)
						.takeWhile(_ < end)
	}
}


class JTTableModel(
		data:Seq[Seq[AnyRef]], 
		columnNames:Seq[String]
) extends javax.swing.table.AbstractTableModel {
	
	var _data = data.map(_.toArray).toArray
	
	def setData(data:Seq[Seq[AnyRef]]) = {
		_data = data.map(_.toArray).toArray
		fireTableDataChanged
	}
	
	override def getColumnName(column:Int):String = columnNames(column)
	override def getRowCount():Int = _data.length
	override def getColumnCount():Int = columnNames.length
	override def getValueAt(row:Int, column:Int):java.lang.Object = {
		_data(row)(column).asInstanceOf[java.lang.Object]
	}
	override def setValueAt(value:AnyRef, row:Int, column:Int):Unit = {
		_data(row)(column) = value
	}
	override def isCellEditable(row:Int, column:Int):Boolean = {
			row >= 0 &&	row < _data.length && 
			column >= 0 && column < columnNames.length
	}
}
