package se.lth.immun.anubis

class AnubisInputException(
		var message:String,
		var paramName:String
) extends Exception("["+paramName+"] " + message) {}