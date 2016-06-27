package se.lth.immun.app

class CLIBar(
		val BAR_LENGTH:Int = 30,
		val newLineAfter:Boolean = false
) {
    var printedLength = 0
    var start = false
    
    def update(x:Int, total:Int):String =
    	update(x.toDouble / total)
    
    def update(percent:Double):String = {
    	val l = (percent * BAR_LENGTH).toInt
    	def newBar:String = 
    		if (l > printedLength) {
    			val sb = new StringBuilder
    			sb ++= (0 until l-printedLength) map (_ => '=')
    			printedLength = l
    			sb.result
    		} else ""
    	
    	if (percent == 0 && !start) {
    		printedLength = 0
    		start = true
    		"["
    	} else if (percent == 1) {
    		start = false
    		newBar + (if (newLineAfter) "]\n" else "]")
        } else 
        	newBar
    }
    
    def reset = {
    	printedLength = 0
    	start = false
    }
    
    def reference = "." + (0 until BAR_LENGTH).map(_ => " ").mkString("") + "."
}