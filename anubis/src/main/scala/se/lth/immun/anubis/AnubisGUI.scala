package se.lth.immun.anubis

import swing._
import swing.event._ 

import java.awt.Color
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.util.Properties

import se.lth.immun.xml.XmlReader

import se.lth.immun.swing._

import scala.actors.Actor

import javax.swing.SwingUtilities
import java.util.concurrent.Executor


trait RALabel extends Label {
	horizontalAlignment = Alignment.Right
}

/*
object SwingExecutionContext {
	implicit val swingExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(new Executor {
		def execute(command: Runnable): Unit = SwingUtilities invokeLater command
	})
}
*/

class AnubisGUI(
		var properties:Properties
) extends MainFrame {

	println("starting gui...")
	
	val runitPipe = SwingActorPipe(Anubis.runit)
	listenTo(runitPipe)
	
	var mzMLFiles:Seq[File] = Nil
	var mzmlFileB:Button 			= Button("Choose mzML files") {
		mzmlFileFC.showDialog(contents(0), "Select mzML files")
		if (!mzmlFileFC.selectedFiles.isEmpty) {
			consoleTA.text = mzmlFileFC.selectedFiles.mkString("\n")
			mzMLFiles = mzmlFileFC.selectedFiles
			status("changed mzML files")
		}
	}
	var mzmlFileFC:FileChooser 		= 
		new FileChooser(new File("./")) {
			multiSelectionEnabled = true
			fileFilter = new javax.swing.filechooser.FileNameExtensionFilter("MzML file", "mzml")
		}
	var consoleTA			= new TextArea
	consoleTA.foreground 	= new Color(0xfaf8f5)
	consoleTA.background 	= new Color(0x463c3c)
	/*var messageConsole 		= new MessageConsole(consoleTA.peer)
	messageConsole.redirectOut(null, System.out)
	messageConsole.redirectErr(null, System.err)
	messageConsole.setMessageLines(200)
    */var rFile:File 					= null
    var referenceFile:ReferenceFile = null
	var refFileTF:TextField 		= new TextField
	var refFileB:Button 			= Button("Reference file") {
		refFileFC.showDialog(contents(0), "Select a reference file")
		if (refFileFC.selectedFile != null) {
			try {
				referenceFile = ReferenceFile.fromFile(new XmlReader(new FileReader(refFileFC.selectedFile)))
				rFile = refFileFC.selectedFile
				refFileTF.text = rFile.toString
				status("changed reference file to "+refFileTF.text)
			} catch {
				case e:Exception => {
					status("couldn't parse reference file")
					e.printStackTrace
				}
			}
		}
	}
	var refFileFC:FileChooser 		= 
		new FileChooser(new File("./")) {
			multiSelectionEnabled = false
		}
	
	var qValueCutoffTF:TextField 	= new TextField("0.01")
	var nullDistSizeTF:TextField 	= new TextField("300")
	var transLimitTF:TextField 		= new TextField("6")
	var peakMinWidthTF:TextField 	= new TextField("0.1")
	var outputNameTF:TextField 		= new TextField
	var statusL:Label				= new Label("<status field>")
	var analyzeB:Button				= Button("Analyze") {
		if (referenceFile == null) 
			error("no valid reference file chosen")
		else if (mzMLFiles.isEmpty)
			error("no mzML files chosen")
		else if (outputNameTF.text == "")
			error("no output name chosen")
		else {
			var peakMinWidth = 0.0
			try {
				peakMinWidth = peakMinWidthTF.text.toDouble
			} catch {
				case _:Throwable => error("peak min width not parseable as number")
			}
			var qCutoff = 0.0
			try {
				qCutoff = qValueCutoffTF.text.toDouble
			} catch {
				case _:Throwable => error("q value cutoff not parseable as number")
			}
			
			var nullDistSize = 0
			try {
				nullDistSize = nullDistSizeTF.text.toInt
			} catch {
				case _:Throwable => error("null distribution size not parseable as number")
			}
			
			var transLimit = 0
			try {
				transLimit = transLimitTF.text.toInt
			} catch {
				case _:Throwable => error("transition limit not parseable as number")
			}
			
			// RECONFIGURE SWING TO DISPLAY REPIPED CONSOLE AND PROCESS INDICATOR
			consoleTA.text = ""
			//infiniteProgressPanel.start
			
			import Runit._
			
			mzmlFileB.enabled		= false
			refFileTF.enabled 		= false
			refFileB.enabled		= false
			qValueCutoffTF.enabled 	= false
			nullDistSizeTF.enabled 	= false
			transLimitTF.enabled 	= false
			peakMinWidthTF.enabled 	= false
			outputNameTF.enabled 	= false
			analyzeB.enabled 		= false
			
			runitPipe pipe RunAnubis(
					properties, 
					rFile, 
					referenceFile, 
					outputNameTF.text, 
					new ResultFile, 
					mzMLFiles,
					qCutoff,
					new AnubisParams(peakMinWidth, nullDistSize, transLimit)
				)
			
			/*
			val anubisComputation = new scala.swing.SwingWorker {
				def act = {
					Console.setOut(System.out)
					Console.setErr(System.err)
					
					AnubisGUIMain.run(
							properties, 
							rFile, 
							referenceFile, 
							outputNameTF.text, 
							resultFile, 
							mzMLFiles,
							qCutoff,
							new AnubisParams(peakMinWidth, nullDistSize, transLimit)
						)
					infiniteProgressPanel.interrupt
					status("analyzed samples!")
				}
			}
			anubisComputation.start()
			* 
			*/
		}
	}

	
	title = properties.getProperty("pom.name") + " " + properties.getProperty("pom.version")
	contents = new BorderPanel {
		focusable 		= true
		minimumSize 	= new Dimension(1024, 800)
		preferredSize 	= new Dimension(1024, 800)
		
		import BorderPanel.Position._
		layout(new BoxPanel(Orientation.Vertical) {
			contents += new GridPanel(5, 3) {
				maximumSize = new Dimension(1024, 150)
				
				contents += new Label("reference file: ") with RALabel
				contents += refFileTF
				contents += refFileB
				
				contents += new Label("output name: ") with RALabel
				contents += outputNameTF
				contents += new Label
				
				contents += new Label("q-value cutoff: ") with RALabel
				contents += qValueCutoffTF
				contents += new Label
				
				contents += new Label("null distribution size: ") with RALabel
				contents += nullDistSizeTF
				contents += new Label
				
				contents += new Label("transition limit: ") with RALabel
				contents += transLimitTF
				contents += new Label
			}
			contents += new BoxPanel(Orientation.Vertical) {
				contents += mzmlFileB
				contents += new ScrollPane(consoleTA)
			}
		}) = Center
		
		layout(new BorderPanel {
			layout(statusL) = Center
			layout(analyzeB) = East
		}) = South
	}
	
	/*
	val infiniteProgressPanel = new InfiniteProgressPanel(
                     "Analysis ongoing, please wait...",
                     12, 0.3f, 10.0f)
	peer.setGlassPane(infiniteProgressPanel)
	peer.pack
	*/
	
	def status(str:String) = {
		statusL.foreground = Color.BLACK
		statusL.text = str
	}
	def error(str:String) = {
		statusL.foreground = Color.RED
		statusL.text = str
	}
	
	
	import Runit._
	reactions += {
		case ActorEvent(Msg(s)) =>
			print(s)
			consoleTA.peer.setText(consoleTA.text + s)
			this.repaint
			
		case ActorEvent(AnubisDone(ra)) =>
			mzmlFileB.enabled		= true
			refFileTF.enabled 		= true
			refFileB.enabled		= true
			qValueCutoffTF.enabled 	= true
			nullDistSizeTF.enabled 	= true
			transLimitTF.enabled 	= true
			peakMinWidthTF.enabled 	= true
			outputNameTF.enabled 	= true
			analyzeB.enabled 		= true
			
			outputNameTF.text 	= ""
		
	}
}