package se.lth.immun.anubis

class AnubisParams(
		var minPeakWidth			:Double 	= 0.1,
		var nullDistributionSize	:Int 		= 1000,
		var transitionLimit			:Int 		= 6,
		var singleAnswer			:Boolean 	= true,
		var pValueTolerance			:Double 	= 0.01,
		var noSanityCheck			:Boolean	= false,
		var noEstimatedQuantities	:Boolean	= false,
		var q1tolerance				:Double		= 0.7,
		var q3tolerance				:Double		= 0.7
) {
	if (minPeakWidth <= 0.0)
		throw new AnubisInputException(
				"Minimal peak width must be greater than 0, got '"+minPeakWidth, "'")
	if (nullDistributionSize <= 10)
		throw new AnubisInputException(
				"Null distribution size must be greater than 10, got '"+nullDistributionSize, "'")
	if (transitionLimit <= 1)
		throw new AnubisInputException(
				"Transition limit must be greater than 1, got '"+transitionLimit, "'")
	if (pValueTolerance < 0)
		throw new AnubisInputException(
				"p-value tolerance must be greater than 0, got '"+pValueTolerance, "'")
	if (q1tolerance < 0)
		throw new AnubisInputException(
				"q1 tolerance must be greater than 0, got '"+pValueTolerance, "'")
	if (q3tolerance < 0)
		throw new AnubisInputException(
				"q3 tolerance must be greater than 0, got '"+pValueTolerance, "'")
}