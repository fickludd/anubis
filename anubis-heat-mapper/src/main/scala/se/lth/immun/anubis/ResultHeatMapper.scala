package se.lth.immun.anubis

import swing._
import swing.event._ 	

import java.io.File
import java.io.FileReader
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.IOException
import java.util.zip.GZIPInputStream
import java.awt.Color
import java.util.Properties
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap

import se.lth.immun.mzml.MzML
import se.lth.immun.mzml.ghost.XMzML
import se.lth.immun.mzml.ghost.XChromatogram
import se.lth.immun.mzml.ghost.XChromatogramGroup
import se.lth.immun.xml.XmlReader
import se.jt.CLIApp
import se.jt.CLIArgumentException
import se.lth.immun.collection.DataMatrix

import scala.collection.mutable.HashMap

import se.lth.immun.proteomics.PeptideIon
import se.lth.immun.proteomics.Mz

import se.lth.immun.graphs.heatmap.HeatMap
import se.lth.immun.graphs.heatmap.HeatMapParams
import se.lth.immun.graphs.LineGraph
import se.lth.immun.graphs.util.LineAnnotation
import se.lth.immun.graphs.util.Annotation
import se.lth.immun.graphs.util.XAnnotation
import se.lth.immun.graphs.util.Curve2

import org.apache.commons.math3.distribution.NormalDistribution

class Peptide(
		var sequence:String,
		var protName:String,
		var mz:Double
) {
	
}

object ResultHeatMapper extends SimpleSwingApplication with CLIApp {
	
	var resFiles:Seq[File] 			= Nil
	var results:Seq[ResultFile] 	= Nil
	var res:ResultFile 				= null
	var ara:PostAnubisAnalysis 		= null
	
	var grid:HeatMap[PeptideIon, File, QualityDataPoint] = null
	var hmParams:HeatMapParams[QualityDataPoint] = null
	var legend:Legend 					= null
	var status:Label 					= null
	var exportB							= Button("Export") {exportImage()}
	var zoomBestPeakCB					= new CheckBox("zoom best peak")
	var imgFile:File					= null
	var imgFileChooser:FileChooser 		= null
	var mainFrame:MainFrame				= null
	//var peakGraph:LineGraph = null
	var width:Int = 0
	var height:Int = 0
	var fdrCutoff:Double = 0.05
	
	var wholeGraph:LineGraph = null
	var currFile:File = null
	var currData:XMzML = null
	
    // refs
	var refFiles:Seq[File] 			= Nil
	var refs:Seq[ReferenceFile] 	= Nil
	var ref:ReferenceFile 			= null
	var currRefFile:File 			= null
	var currRefData:XMzML 			= null
	var refGraph:LineGraph 			= null
	
	var q1tolerance 	= 0.05
	var q3tolerance 	= 0.05
    
	val NO_PEAK = -1.0
	val DISCARDED_PEAK = -2.0
	val NO_DATA = -3.0
	val PROT_ROW = -4.0
	
	
	
	def printSelection() = println(grid.selectedColumn+" "+grid.selectedRow)
    	
	val params = new ResultHeatMapperParams(name, version)
	var properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
	val name 		= properties.getProperty("pom.artifactId")
	val version 	= properties.getProperty("pom.version")
	
	override def main(args:Array[String]):Unit = {
		
    	failOnError(parseArgs(name, version, args, params, List("resultFiles"), Some("resultFiles")))
		
    	resFiles = params.resultFiles.value.split(" ").map(new File(_))
    	results = resFiles.map(f => new ResultFile(new XmlReader(
								new BufferedReader(new FileReader(f))
							)))
		
		
		
		println("=== " + properties.getProperty("pom.name") +
				" v" + properties.getProperty("pom.version") + " ===")
				
    	println("      data files:")
    	resFiles.foreach(f => println("    "+f))
		
		refFiles 	= results.map(_.referenceFile)
		refs 		= refFiles.map(f => findRef(f))
		ref 		= mergeReferences(refs.filter(_ != null))
		
		res 		= mergeResults(results)
		ara 		= new PostAnubisAnalysis(
							res, 
							q => q.lfdr <= fdrCutoff, 
							params.parseProtGroup, 
							params.parseRepGroup)
		
		hmParams 		= new HeatMapParams[QualityDataPoint](
			q => q.quantity,
			ara.filter,
			d => math.log10(d),
			q => 
				if (java.lang.Double.isNaN(q.lfdr))	new Color(0x333333)
				else if (q.lfdr > fdrCutoff) 		new Color(0x554444)
				else 								Color.BLACK
		) {	tileWidth = 12; tileHeight = 12	}
		
		status = new Label("...")
		legend = new Legend(hmParams, str => status.text = str, properties)
	
		grid = new HeatMap[PeptideIon, File, QualityDataPoint](
			ara.dataMatrix, hmParams,
			{	case pi:PeptideIon => pi.mz.d.toInt.toString
				case f:File => f.getName.take(f.getName.length - 5) },
			legend
		)
		
		imgFileChooser = new FileChooser() {
			fileFilter = new javax.swing.filechooser.FileNameExtensionFilter("Png file", "png")
		}
		
		
		println("done")
		
		print("initiating user interface...")
		super.main(args)
	}
	
	
	
	def top = {
		mainFrame = new MainFrame {
			title = 	properties.getProperty("pom.name") + 
				" v" + properties.getProperty("pom.version") + 
				" - " + (
					if (resFiles.length == 1) 	resFiles.head.getCanonicalPath 
					else 						resFiles.length+" result files" )
			contents = new BorderPanel {
				focusable = true
				minimumSize = new Dimension(grid.pxWidth + 20, grid.pxHeight + 300)
				preferredSize = new Dimension(grid.pxWidth + 20, grid.pxHeight + 300)
				
				wholeGraph = new LineGraph() { 
					xLayoutAlignment 	= java.awt.Component.CENTER_ALIGNMENT
					minimumSize 		= new Dimension(1, 300)
					preferredSize 		= new Dimension(1000, 300)
					maximumSize 		= new Dimension(Int.MaxValue, 300)
					cacheRender 		= true
				}
				
				refGraph = new LineGraph() { 
					xLayoutAlignment 	= java.awt.Component.CENTER_ALIGNMENT
					minimumSize 		= new Dimension(1, 300)
					preferredSize 		= new Dimension(300, 300)
					maximumSize 		= new Dimension(Int.MaxValue, 300)
					mouseZoom			= false
				}
				//peakGraph = new LineGraph()
				
				import BorderPanel.Position._
				layout(
					new BoxPanel(Orientation.Vertical) {
						contents += new ScrollPane {
							import ScrollPane.BarPolicy._
							
							horizontalScrollBarPolicy = AsNeeded
							verticalScrollBarPolicy = AsNeeded
							
							contents = grid
							listenTo(grid)
							reactions += {
								case r:UIElementResized => {
									repaint
								}
							}
						}
						contents += Swing.RigidBox(new Dimension(0, 5))
						contents += new BoxPanel(Orientation.Horizontal) {
							contents += wholeGraph
							contents += refGraph
						}
						/*new SplitPane(
							Orientation.Horizontal,
							wholeGraph,
							peakGraph
						) { 
							xLayoutAlignment = java.awt.Component.CENTER_ALIGNMENT
							dividerLocation = 150
							minimumSize = new Dimension(1, 300)
							preferredSize = new Dimension(Int.MaxValue, 300)
							maximumSize = new Dimension(Int.MaxValue, 300)
						}
						*/
					}
				) = Center
				
				layout(new BorderPanel {
					layout(status) = Center
					layout(new FlowPanel {
						contents += zoomBestPeakCB
						contents += exportB
					}) = East
				}) = South
				
				listenTo(keys)
				listenTo(grid)
				listenTo(legend.repaintPublisher)
				reactions += {
					case KeyPressed(_, key, _, _) => onKeyPress(key)
					case sc:SelectionChanged => redrawGraph
					case vc:ValueChanged => {
						grid.heatMap.repaint
						legend.repaint
					}
				}
			} // end contents
		
			import scala.swing.event.Key._
			def onKeyPress(keyCode: Value) = {
				keyCode match {
				    case Left => 	grid.selectLeft
				    case Right => 	grid.selectRight
				    case Up => 		grid.selectUp
				    case Down => 	grid.selectDown
				    case Enter => 	printSelection
				    case Key.F =>	
				    	findDialog.open
				    	findDialog.centerOnScreen
				    case _ => {}
				}
			}
		}
		
		println("done")
		mainFrame
	} // end top
	
	
	
	val findText 			= new TextField { preferredSize = new Dimension(200, 20)}
	val findButton 			= Button("find!") { findAndSelect(findText.text); findDialog.close }
	val cancelFindButton 	= Button("nevermind") { findDialog.close }
	val findDialog:Dialog 	= new Dialog {
		contents = new FlowPanel {
			contents += findText
			contents += findButton
			contents += cancelFindButton
		}
		listenTo(findText)
		reactions += {
			case ed:EditDone =>
				if (findDialog.showing) {
					println("edit done")
					findAndSelect(findText.text)
					findDialog.close
				}
		}
		
		
	}
	
	def findAndSelect(str:String):Unit = {
		println("finding and selecting: "+str)
		val ri = grid.heatMap.rowLeaves.indexWhere(_.obj match {
			case pi:PeptideIon =>
				pi.sequence.toLowerCase.contains(str.toLowerCase)
			case _ => false
		})
		
		if (ri >= 0) {
			grid.heatMap.highlighted.select(new se.lth.immun.graphs.heatmap.MatrixPosition(0, ri))
			grid.heatMap.selected.select(grid.heatMap.highlighted.pos)
			status.text = grid.heatMap.data.rows(ri)+" found at index "+ri
		} else {
			status.text = "Peptide containing sequence '"+str+"' not found"
		}
	}
	
	
	
	def redrawGraph() = drawGraphs(grid.selectedRow.get, grid.selectedColumn.get)
	def drawGraphs(pi:PeptideIon, file:File):Unit = {
		
		status.text = "Drawing "+pi+" from '"+file.toString+"'"
		def error(str:String):Unit = {
			status.text = str
			wholeGraph.clear
			//peakGraph.clear
			wholeGraph.repaint
			//peakGraph.repaint
		}
		
		def toCurve(td:XChromatogram):Curve2[Double, Double] = 
			new Curve2(
					td.times,
					td.intensities,
					td.intensities.map(d => java.lang.Double.isNaN(d)),
					"%.2f".format(td.q3)
					)
		
		def tryFile(file:File) = {
			if (file.exists) {
				currFile = file
				true
			} else false
		}
		
		if (currFile != file) {
			if (
					tryFile(file)
			) {}
			else if (
					tryFile(new File(resFiles.head.getParent, file.getName))
			) {}
			else if (
					tryFile(new File(file.getName))
			) {}
			else if (
						file.getName.toLowerCase.endsWith(".mzml.gz") 
					&& 	tryFile(new File(file.getName.dropRight(3)))
			) {}
			else if (
						file.getName.toLowerCase.endsWith(".mzml") 
					&& 	tryFile(new File(file.getName + ".gz"))
			) {}
			else
				return error("Could not find file '" + file.getCanonicalPath + "'.")

			try {
				currData = parseFile(currFile)
			} catch {
				case e:Exception => {
					e.printStackTrace
					return error("Error opening the file '" + currFile.getCanonicalPath + "'.")
				}
			}
		}
		
		res.precursors.find( rpc => rpc.peptideSequence == pi.sequence && rpc.mz == pi.mz.d) match {
			case None => {
				return error(pi + " not found in result file.")
			}
			case Some(rpc) => {
				
				var reps = rpc.replicates.filter(rep => 
					res.replicateFiles(rep.fileId).file == file)
				
				if (reps.isEmpty)
					return error(rpc.peptideSequence + " not found in result replicate '" + currFile.getName + "'.")
				
				val cgInFile = 
					currData.grouper.extractGroup(pi.mz.d, q1tolerance) getOrElse {
						return error(rpc.peptideSequence + " not found in replicate '" + currFile.getName + "'.")
					}
				
				val cg = cgInFile.extract(
						reps(0).transitions.map(_.mz).toArray, true, q3tolerance)
				
				wholeGraph.setCurves(cg.chromatograms.map(c => toCurve(c)))
				wholeGraph.title = pi + " - " + currFile.getName
				
				for (rep <- reps) {
					var rt = rep.retentionTime
					
					if (rt.start > 0 && rt.end > 0) {
						wholeGraph.addAnnotation(new XAnnotation(rt.start))
						wholeGraph.addAnnotation(new XAnnotation(rt.end))
						/*
						peakGraph.title = "ZOOMED " + wholeGraph.title
						peakGraph.setCurves(tgd.transitions.map(tr => toCurve(tr)))
						peakGraph.setZoom(rt.start - 1.5*rt.width, rt.end + 1.5*rt.width)
						peakGraph.addAnnotation(new XAnnotation(rt.start, AnnotationColor.Passive))
						peakGraph.addAnnotation(new XAnnotation(rt.end, AnnotationColor.Passive))
						*/
					} else {
						//peakGraph.clear
					}
				}
				
				if (reps.length == 1 && zoomBestPeakCB.selected) {
					var largest = reps.head
					var times = cg.chromatograms.head.times
					var padding = 2*largest.retentionTime.width
					wholeGraph.setZoom(
							math.max(times.head, largest.retentionTime.start - padding),
							math.min(times.last, largest.retentionTime.end + padding)
							)
					
				}
				
				status.text = pi + " #quantifications=" + reps.length
				wholeGraph.repaint
				
				drawRef(pi, cg.chromatograms.map(c => new Mz(c.q3, q3tolerance)))
			}
		}
	}
	
	
	
	def drawRef(pi:PeptideIon, q3s:Seq[Mz]):Unit = {
		
		import se.lth.immun.anubis.RetentionTime
		
		def error(str:String):Unit = {
			status.text = str
			println(str)
			refGraph.clear
			//peakGraph.clear
			refGraph.repaint
			//peakGraph.repaint
			currRefData = null
		}
		
		def toZoomedCurve(td:XChromatogram, istart:Int, iend:Int):Curve2[Double, Double] = {
			new Curve2(
					td.times.slice(istart, iend),
					td.intensities.slice(istart, iend),
					td.intensities.slice(istart, iend).map(d => java.lang.Double.isNaN(d)),
					"%.2f".format(td.q3)
					)
		}
		
		
		def drawIdealized(rpc:se.lth.immun.anubis.ReferencePrecursor, q3s:Seq[Mz]) = {
			val relInts = 
				(
					for (i <- 0 until rpc.measuredTransitions.length) yield {
						val relIntensities = 
							for {
								j <- 0 until rpc.measuredTransitions.length
							} yield (
								if (j < i) rpc.ratios.filter(x => x.transitionId1 == j && x.transitionId2 == i).head.mean
								else if (j == i) 1.0
								else 1 / rpc.ratios.filter(x => x.transitionId1 == i && x.transitionId2 == j).head.mean
							)
						relIntensities.map(_ / relIntensities.max)
					}
				)
			
			val consensusRelIntensities = relInts.transpose.map(x => x.sum / x.length)
			
			
			
			val times = -3.0 until 3.0 by 0.1
			val normDist = new NormalDistribution()
			val gaussian = times.map(t => normDist.density(t))
			val curves = 
				ListMap() ++ (for ((tr, relInt) <- rpc.measuredTransitions.zip(consensusRelIntensities)) yield {
					val ys = gaussian.map(_ * relInt)
					new Mz(tr.mz, q3tolerance) -> new Curve2(times, ys, times.map(_ => false), "%.2f".format(tr.mz))
				})
			
			val vds = q3s.filter(curves.contains(_))
			val mappedCurves = vds.map(curves(_))
				
			refGraph.setCurves(mappedCurves)
			refGraph.title = "idealized "+rpc.peptideSequence+" "+rpc.mz
			refGraph.repaint
		}
		
		
		ref.precursors.find(x => x.mz == pi.mz.d && x.peptideSequence == pi.sequence) match {
			case Some(rpc) => {
				if (rpc.fileId < 0 || rpc.fileId >= ref.files.length)
					drawIdealized(rpc, q3s)
					return //error("No reference data file specified for sequence '%s' m/z=%.1f.".format(pi.sequence, pi.mz))
				
				var f = fixFile(ref.files(rpc.fileId))
				if (currRefFile != f) {
					if (f.exists)
						currRefFile = f
					else {
						var relFile = new File(resFiles.head.getParent, f.getName)
						if (relFile.exists)
							currRefFile = relFile
						else {
							relFile = new File(f.getName)
							if (relFile.exists)
								currRefFile = relFile
							else
								return drawIdealized(rpc, q3s)
										//error("Could not find the reference file '" + f.toString + "'.")
						}
					}
					try {
						currRefData = parseFile(currRefFile)
					} catch {
						case e:Exception => {
							e.printStackTrace
							return drawIdealized(rpc, q3s)
									//error("Error opening the file '" + currRefFile.getCanonicalPath + "': "+e.getMessage)
						}
					}
				}
				
				
		
				if (currRefData != null) {
					var cg = 
						currRefData.grouper.extractGroup(
							pi.mz.d, rpc.measuredTransitions.map(_.mz).toArray, true
						) getOrElse {
							return drawIdealized(rpc, q3s)
							//error(rpc.peptideSequence + " not found in reference data file '" + currRefFile.getName + "'.") 
						}
					
					val rt 		= rpc.retentionTime
					val times 	= cg.chromatograms.head.times
					var istart 	= math.max(0, times.indexWhere(_ > rt.start - rt.width))
					var iend 	= times.indexWhere(_ > rt.end + rt.width)
					if (iend < 0) iend = times.length - 1
					
					refGraph.setCurves(cg.chromatograms.map(c => toZoomedCurve(c, istart, iend)))
					refGraph.addAnnotation(new XAnnotation(rt.start))
					refGraph.addAnnotation(new XAnnotation(rt.end))
					refGraph.title = currRefFile.getName
					refGraph.repaint
				}
			}
			case None => println("No ref for '%s'".format(pi.toString))
		}
	}
	
	
	
	def mergeResults(results:Seq[ResultFile]):ResultFile = {
		var mReps = new ArrayBuffer[ResultMzMLFile]
		var mPcs = new ArrayBuffer[ResultPrecursor]
		
		for (res <- results) {
			for (rep <- res.replicateFiles) {
				if (!mReps.exists(_.file.toString == rep.toString))
					mReps += rep
			}
			for (pc <- res.precursors) {
				if (!mPcs.exists(x => x.mz == pc.mz && x.peptideSequence == pc.peptideSequence))
					mPcs += new ResultPrecursor(pc.mz, pc.proteinName, pc.peptideSequence)
				var mPc = mPcs.find(x => x.mz == pc.mz && x.peptideSequence == pc.peptideSequence).get
				for (rr <- pc.replicates) {
					var mid = mReps.indexWhere(_.file.toString == res.replicateFiles(math.max(0, rr.fileId)).file.toString)
					if (!mPc.replicates.exists(_.fileId == mid)) {
						rr.fileId = mid
						mPc.replicates = mPc.replicates :+ rr
					}
				}
			}
		}
		
		return new ResultFile(Array(), mReps.toArray, mPcs.toArray)
	}
	
	
	
	def mergeReferences(refs:Seq[ReferenceFile]):ReferenceFile = {
		import se.lth.immun.anubis.ReferencePrecursor
		
		var mFiles = new ArrayBuffer[File]
		var mPcs = new ArrayBuffer[ReferencePrecursor]
		
		for (ref <- refs) {
			for (f <- ref.files) {
				if (!mFiles.exists(_.toString == f.toString))
					mFiles += f
			}
			for (pc <- ref.precursors) {
				if (!mPcs.exists(x => x.mz == pc.mz && x.peptideSequence == pc.peptideSequence)) {
					var mid = mFiles.indexWhere(_.toString == ref.files(math.max(0, pc.fileId)).toString)
					mPcs += new ReferencePrecursor(pc.mz, mid, -1, -1, -1, pc.peptideSequence, pc.proteinName,
													pc.retentionTime, pc.measuredTransitions, pc.ignoredTransitions,
													pc.ratios)
				}
			}
		}
		
		return new ReferenceFile(mFiles.toArray, mPcs.toArray)
	}
	
	
	
	def findRef(refFile:File):ReferenceFile = {
		var f:File = null
		if (refFile.exists)
			f = refFile
		else {
			var relFile = new File(resFiles.head.getParent, refFile.getName)
			if (relFile.exists)
				f = relFile
			else {
				relFile = new File(refFile.getName)
				if (relFile.exists)
					f = relFile
				else
					println("Could not find the reference file '" + refFile.getCanonicalPath + "'.")
			}
		}
		
		var rf:ReferenceFile = null
		if (f != null) {
			try {
				var r = new XmlReader(new BufferedReader(new FileReader(f)))
    			if (f.getName.toLowerCase.endsWith(".traml"))
    				rf = ReferenceFile.fromTraML(r)
	    		else 
	    			rf = ReferenceFile.fromFile(r)
			} catch {
				case e:Exception => {
					e.printStackTrace
					println("Error opening the reference file '" + f.getCanonicalPath + "'.")
				}
			}
		}
		
		return rf
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
	
	
	
	def fixFile(f:File) = {
		val sep = System.getProperty("file.separator")
		val str = f.toString.replace("\\", sep).replace("/", sep)
		new File(str)
	}
	
	
	
	def exportImage() = {
		if (imgFile == null) {
			imgFileChooser.showDialog(mainFrame.contents.head, "Export png img file")
			imgFile = imgFileChooser.selectedFile
		}
		if (imgFile != null) {
			var bI = new BufferedImage(grid.size.width, grid.size.height, BufferedImage.TYPE_INT_RGB)
			var g2 = bI.createGraphics
			grid.peer.print(g2)
			g2.dispose
			//var names = ImageIO.getWriterFormatNames
			if (!imgFile.getName.toLowerCase.endsWith(".png"))
				imgFile = new File(imgFile.toString + ".png")
			ImageIO.write(bI, "png", imgFile)
			
			status.text = "exported heatmap to "+imgFile.toString
		}		
	}
}
