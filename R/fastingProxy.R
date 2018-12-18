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



# Calculates a proxy measure of fasting SG - average of the 30 minute period with the lowest average.
# Returns fasting proxy value.
fastingProxy <- function(raw) {

	# get only rows with SG readings
	sgIdx = which(!is.na(raw$sgReading))
	raw = raw[sgIdx,]

	# get position 1-30, for the 30 consecutive values at each idx
	# each row has columns 1-30 for each index starting at the row (and some NAs if <30 after this index in sequence)
	raw$noct1 = raw$sgReading

	colnames=c("noct1")

	for (i in 2:30) {
		colname=paste("noct",i,sep='')

		# shift sgReadings up 1 each time
		colvals=raw$sgReading[i:nrow(raw)]
		colvalsna = rep(NA,i-1)
		colvals = c(colvals,colvalsna)

		raw[,colname] = colvals
		colnames=c(colnames, colname)
	}

	raw$noctMean = rowMeans(raw[,colnames])

	noctNotNA = raw$noctMean[which(!is.na(raw$noctMean))]

	# lowest of the 6 consecutive values (minutes)
	fastProxy = min(noctNotNA)

	return(fastProxy)
}

