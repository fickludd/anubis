package se.lth.immun.app

import java.io.IOException

class CommandlineArgumentException(
		message:String,
		cause:Throwable = null
) extends IOException(message, cause) {}