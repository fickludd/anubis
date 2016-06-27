package se.lth.immun.app

import scala.collection.mutable.ArrayBuffer


class Option(
		var name:String,
		var desc:String,
		var handle:String => Unit,
		var valuePlaceHolder:String = null) {
	
}



class Arg(
		var valuePlaceHolder:String,
		var handle:String => Unit) {
	
}



class Rest(
		var valuePlaceHolder:String,
		var handle:Array[String] => Unit,
		var optional:Boolean) {
	
}



object CLIApplication {
	var log:LogFile 	= null
	var logDir:String 	= "log"
}



trait CLIApplication {
	
	import CLIApplication._
	
	private var _options		= new ArrayBuffer[Option] 
	private var _args			:Seq[Arg] 			= Nil
	private var _optionalArgs	:Seq[Arg] 			= Nil
	private var _rest			:Rest 				= null
	private var args			:Array[String] 		= Array()
	private var applicationName:String 			= ""
	
	
	opt(
			"help",
			"print usage and quit", 
			s => error("", null)
		)
	opt(
			"log-dir", 
			"directory where log file should be stored (default './log')", 
			s => logDir = s, 
			"X"
		)
	
	
	def opt(
			name:String, 
			desc:String, 
			handler:String => Unit, 
			valuePlaceHolder:String = null) = {
		_options += new Option(name, desc, handler, valuePlaceHolder)
	}
	
	
	
	def arg(valuePlaceHolder:String, handler:String => Unit, optional:Boolean = false) = {
		if (optional) {
			if (_rest != null)
				throw new IllegalArgumentException("Cannot have both non-optional arguments and rest.")
			_optionalArgs = _optionalArgs :+ new Arg(valuePlaceHolder, handler)
		} else {
			if (_optionalArgs.length != 0)
				throw new IllegalArgumentException("Cannot have non-optional arguments after optional arguments.")
			_args = _args :+ new Arg(valuePlaceHolder, handler)
		}
	}
	
	
	
	def rest(valuePlaceHolder:String, handler:Array[String] => Unit, optional:Boolean) = {
		if (_optionalArgs.length != 0)
			throw new IllegalArgumentException("Cannot have both non-optional arguments and rest.")
		_rest = new Rest(valuePlaceHolder, handler, optional)
	}
	
	
	
	def parseArgs(applicationName:String, args:Array[String]):Unit = {
		args.find(a => a.startsWith("--log-dir=")) match {
			case Some(a) => logDir = a.dropWhile(_ != '=').tail.toString
			case None => {}
		}
		
		log = new LogFile(applicationName, logDir)
		this.applicationName = applicationName
		this.args = args
		
		try {
			var argList = args.toList
			while (!argList.isEmpty && argList.head.startsWith("--")) {
				var arg:String = argList.head.drop(2)
				argList = argList.tail
				var t = arg.split('=')
				_options.find(opt => opt.name == t(0)) match {
					case Some(opt) => {
						if (opt.valuePlaceHolder != null)
							if (t.length != 2)
								error("Error parsing '"+t(0)+"'. "+opt.name + " option need value of type "+opt.valuePlaceHolder, null)
							else
								opt.handle(t(1))
						else
							opt.handle(null)
					}
					case None => error("Error parsing '"+t(0)+"'. Option does not exist.", null)
				}
			}
			if (_args.length > argList.length)
				error("Not enough arguments!", null)
				
			for (a <- 0 until _args.length) {
				var al = argList(a)
				var _al = _args(a)
				parseArg(al, _al)
			}
				
			if (argList.length > _args.length) {
				if (_rest != null) {
					 if (argList.length > _args.length)
						 _rest.handle(argList.drop(_args.length).toArray)
				} else {
					if (argList.length > _args.length + _optionalArgs.length)
						error("Too many arguments provided!", null)
					if (_optionalArgs.length > 0)
						for (a <- _args.length until argList.length)
							parseArg(argList(a), _optionalArgs(a - _args.length))
				}
			}
			
		} catch {
			case cae:CommandlineArgumentException => throw cae
			case e:Exception => 
				throw new CommandlineArgumentException(e.getLocalizedMessage, e)
		}
	}
	
	
	
	def parseArg(value:String, arg:Arg):Unit = {
		try {
			arg.handle(value)
		} catch {
			case e:Exception => {
				log.write(e)
				error("Error parsing '"+value+"' as <"+arg.valuePlaceHolder+">", e)
			}
		}
	}
	
	
	
	def error(mess:String, e:Exception):Unit = {
		println(mess)
		log.write(mess)
		printUsage
		throw new CommandlineArgumentException(mess, e)
	}
	
	
	
	def printUsage = {
		if (args.length > 0)			println("args: " + args.map("\"" + _ + "\"").reduceRight(_ + " " + _))
		
										println("usage:")
										print("> java -jar "+applicationName+".jar")
		if (_options.length > 0) 		print(" [OPTIONS]")
		for (_arg <- _args) 			print(" <"+_arg.valuePlaceHolder+">")
		for (_oArg <- _optionalArgs) 	print(" <"+_oArg.valuePlaceHolder+">?")
		if (_rest != null) 				print(" <"+_rest.valuePlaceHolder+">"+ (if (_rest.optional) "*" else "+"))
										println()
		
										println("  OPTIONS:")
		for (_opt <- _options) {
										print("  --"+_opt.name)
			if (_opt.valuePlaceHolder != null) 
										print("="+_opt.valuePlaceHolder)
										println(" # " +_opt.desc)
		}
	}
}