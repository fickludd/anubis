package se.lth.immun.anubis

class CLIBar(
		val BAR_LENGTH:Int = 30,
		val newLineAfter:Boolean = false
) {
    var printedLength = 0
    var start = false
    
    def update(x:Int, total:Int):String =
    	update(x.toDouble / total)
    
    def update(percent:Double):String = {
    	if (percent == 0 && !start) {
    		printedLength = 0
    		start = true
    		"["
    	} else if (percent == 1) {
    		start = false
    		if (newLineAfter) "]\n" else "]"
        } else {
    		var l = (percent * BAR_LENGTH).toInt
    		if (l > printedLength) {
    			val sb = new StringBuilder
    			sb ++= (0 until l-printedLength) map (_ => '=')
    			printedLength = l
    			sb.result
    		} else ""
    	}
    }
    
    def reset = {
    	printedLength = 0
    	start = false
    }
}