# The MIT License (MIT)
# Copyright (c) 2018 Louise AC Millard, MRC Integrative Epidemiology Unit, University of Bristol
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
# documentation files (the "Software"), to deal in the Software without restriction, including without
# limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so, subject to the following
# conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions
# of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
# TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.


# called when day is created
# ensures day has 1441 glucose values (1 per minute plus the last minute of the next day for interpolation)
check_day <- function(object) {
	errors <- character()

	length_glucose <- nrow(object@glucose)

	if (length_glucose != 1441) {
		msg <- paste("Glucose is length ", length_glucose, ".  Should be 1441", sep = "")
		errors <- c(errors, msg)
	}

	if (length(errors) == 0) TRUE else errors

}


setClass("day", 
	slots = c(glucose = "data.frame",
			dayidx = "numeric",
			validday = "logical",
			events = "data.frame",
			bg = "data.frame")
	,validity = check_day
)




setClass("participantData",
	slots = c(nightstart = "POSIXlt",
			daystart = "POSIXlt",
			dayfirst = "logical",
			days = "list",
			numvaliddays = "numeric")
)

setClass("event",
	slots = c(events = "data.frame",
			meantimetopeak = "numeric",
			meanpp1 = "numeric",
			meanpp2 = "numeric")
)



setClass("runSettings",
	slots = c(indir = "character",
		outdir = "character", 
		device = "numeric",
		daystart = "POSIXlt",
		nightstart = "POSIXlt",
		dayPeriodStartTime = "POSIXlt",
		firstvalid = "logical",
		timeformat = "character",
		imputeApproximal = "logical", 
		imputeOther = "logical", 
		freq = "numeric",
		outlierthreshold = "numeric",
		hypothreshold = "numeric",
		hyperthreshold = "numeric",
		save = "logical",
		saveevents = "logical",
		pregnancy = "logical",
		diabetes = "logical",
		epochfrequency = "numeric",
		mgdl = "logical")
)
