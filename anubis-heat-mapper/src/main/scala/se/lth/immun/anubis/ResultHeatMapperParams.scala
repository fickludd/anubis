package se.lth.immun.anubis

import se.jt.Params
import java.io.File

class ResultHeatMapperParams(val name:String, val version:String) extends Params {

  import Params._
  
  val resultFiles = ReqString("The result files to plot")
  val repGroup = ""	 	## "Tsv file with one column per replicate group level. Top-most group to left"
  val protGroup = ""	 	## "Tsv file with one column per protein group level. Top-most group to left" 
  
  def parseProtGroup = parseGroup(protGroup)
  def parseRepGroup = parseGroup(repGroup)
  
  def parseGroup(path:String) = {
    if (path == "") None
    else Some(GroupTree.fromFile(new File(path)))
  }
}