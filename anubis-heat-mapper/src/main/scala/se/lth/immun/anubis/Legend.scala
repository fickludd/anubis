package se.lth.immun.anubis

import java.awt.Color
import java.util.Properties
import scala.swing._
import scala.swing.event.SelectionChanged
import scala.swing.event.ValueChanged
import scala.swing.event.EditDone

import se.lth.immun.graphs.heatmap.HeatMapParams
import se.lth.immun.graphs.util.ColorTransform
import se.lth.immun.graphs.util.TwoPointGradientCT
import se.lth.immun.graphs.util.ThreePointGradientCT

class Legend(
		var params:HeatMapParams[QualityDataPoint],
		var status:(String => Unit),
		var properties:Properties
) extends BoxPanel(Orientation.Vertical) {

	
	class DataTypeEntry[D](
			var toDouble:(D => Double),
			var transform:(Double => Double),
			var colorTransform:ColorTransform,
			var name:String
	) {
		override def toString = name
	}
	var icColorTransform:ColorTransform = new ThreePointGradientCT(
			Color.RED,
			Color.YELLOW,
			Color.GREEN
	)
	var rtColorTransform:ColorTransform = new TwoPointGradientCT(
			Color.WHITE,
			Color.BLUE
	)
	var widthColorTransform:ColorTransform = new TwoPointGradientCT(
			Color.MAGENTA,
			Color.LIGHT_GRAY
	)
	var dataTypeCB = new ComboBox(Array(
			new DataTypeEntry[QualityDataPoint](
				q => q.quantity,
				d => math.log10(d),
				icColorTransform,
				"Peak total ion count (log10)"
			),
			new DataTypeEntry[QualityDataPoint](
				q => q.apex,
				d => d,
				rtColorTransform,
				"Peak apex retention time (min)"
			),
			new DataTypeEntry[QualityDataPoint](
				q => q.end - q.start,
				d => d,
				widthColorTransform,
				"Peak width (min)"
			)
		))
	
	
	
	import HeatMapParams._
	class ColorSchemeEntry(
			var scheme:ColoringScheme,
			var name:String
	) {
		override def toString = name
	}
	var fixedEntry = new ColorSchemeEntry(
				Fixed(0, 0, 0),
				"define own max/mean/min"
			)
	var colorSchemeCB = new ComboBox(
		Array(
			new ColorSchemeEntry(
				Global(),
				"use global max/mean/min"
			),
			new ColorSchemeEntry(
				Row(),
				"color row with local max/mean/min"
			),
			fixedEntry
		)
	)
	
	
	var minText = new TextField {enabled = false}
	var midText = new TextField {enabled = false}
	var maxText = new TextField {enabled = false}
	var valsBP = new BoxPanel(Orientation.Horizontal) {
		contents += new BoxPanel(Orientation.Vertical) {
			contents += new Label("min")
			contents += minText
		}
		contents += new BoxPanel(Orientation.Vertical) {
			contents += new Label("mid")
			contents += midText
		}
		contents += new BoxPanel(Orientation.Vertical) {
			contents += new Label("max")
			contents += maxText
		}
	}
	

	
	contents += new Label("v"+properties.getProperty("pom.version"))
	contents += dataTypeCB
	contents += colorSchemeCB
	contents += valsBP
	
	
	
	/* interaction */
	private val legend = this
	var repaintPublisher = new Publisher {}
	var dataTypeReactor = new Reactor {
		listenTo(dataTypeCB.selection)
		reactions += {
			case sc:SelectionChanged => {
				var i = dataTypeCB.selection.item
				params.toDouble = i.toDouble
				params.transform = i.transform
				params.colorTransform = i.colorTransform
				repaintPublisher.publish(new ValueChanged(legend))
			}
		}
	}
	var colorSchemeReactor = new Reactor {
		listenTo(colorSchemeCB.selection)
		reactions += {
			case sc:SelectionChanged => {
				var i = colorSchemeCB.selection.item
				i.scheme match {
					case Fixed(min, mid, max) => {
						minText.enabled = true
						midText.enabled = true
						maxText.enabled = true
					}
					case _ 	=> {
						params.coloringScheme = i.scheme
						repaintPublisher.publish(new ValueChanged(legend))
						minText.enabled = false
						midText.enabled = false
						maxText.enabled = false
					}
				}
			}
		}
	}
	listenTo(minText)
	listenTo(midText)
	listenTo(maxText)
	reactions += {
		case ed:EditDone => {
			try {
				params.coloringScheme = Fixed(
										minText.text.toDouble,
										midText.text.toDouble,
										maxText.text.toDouble
										)
			} catch {
				case nfe:NumberFormatException => {
					status("Couldn't parse string as a number: "+nfe.toString)
				}
			}
			repaintPublisher.publish(new ValueChanged(legend))
		}
	}
	
	
	
	override def paintComponent(g:Graphics2D) = {
		params.coloringScheme match {
			case Fixed(a,b,c) => {}
			case _ => {
				minText.text = "%.2f".format(params.dataMin)
				midText.text = "%.2f".format(params.dataMid)
				maxText.text = "%.2f".format(params.dataMax)
			}
		}
		super.paintComponent(g)
	}
}
