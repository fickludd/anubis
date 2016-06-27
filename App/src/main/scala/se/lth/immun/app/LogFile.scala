package se.lth.immun.app

import java.io.File
import java.io.PrintWriter
import java.text.DateFormat
import java.util.Date


class LogFile(
		var name:String,
		var dir:String = "log"
) {
	
	var dateFormat 			= DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM)
	var folder 				= new File(dir)
	var file 				= new File(dir, name + ".log")
	var writer:PrintWriter 	= null
	
	if (!folder.exists) 
		try 	{ folder.mkdir }
		catch 	{ case _:Throwable => println("WARNING: couldn't create log file folder")}
	
	if (folder.exists && file.exists) 
		try 	{ file.delete }
		catch 	{ case _:Throwable => println("WARNING: couldn't delete old log file")}
	
	try 	{file.createNewFile}
	catch 	{case _:Throwable => println("WARNING: couldn't create new log file")}
	
	if (file.exists)
		writer = new PrintWriter(file)
		
	write("===== " + name + " =====\n")
	
	
	def write(mess:Any) = {
		if (writer != null) {
			mess match {
				case str:String => writer.write("[" + dateFormat.format(new Date()) + "]" + mess + "\n")
				case e:Exception => e.printStackTrace(writer)
			}
			
			writer.flush()
		}
	}
}