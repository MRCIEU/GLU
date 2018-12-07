# The MIT License (MIT)
# Copyright (c) 2017 Louise AC Millard, MRC Integrative Epidemiology Unit, University of Bristol
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



# CGM values are marked as large if they are >k from the previous or subsequent values, where k is 4xSD of the distribution of differences between adjacent points.
markLargeDeviations <- function(raw) {

	if (is.null(raw)) {
		return(NULL)
	}

	# set dummy variable to 0 (false) if it's a SG reading row
	raw$deviationLarge = FALSE


	##
	## get difference of each SG value from the previous and next values in the sequence

	sgReadingAfter = c(raw$sgReading[2:nrow(raw)], NA)
        sgReadingBefore = c(NA, raw$sgReading[1:(nrow(raw)-1)])
        imputeAfter = c(raw$impute[2:nrow(raw)], 'NULL')
        imputeBefore = c('NULL', raw$impute[1:(nrow(raw)-1)])

	sgDiffAfter = sgReadingAfter - raw$sgReading

	ix = which(imputeAfter!=raw$impute)

	if (length(ix)>0) {
		sgDiffAfter[ix] = NA
	}

	sgDiffBefore = raw$sgReading - sgReadingBefore

	ix = which(raw$impute!=imputeBefore)
	if (length(ix)>0) {
	        sgDiffBefore[ix] = NA
	}

	##
	## calculate allowable difference

	k = opt$outlierthreshold
	allowableJump = k*sd(sgDiffAfter, na.rm=TRUE)


	##
	## find jumps larger than allowable

	devlargeLeft = rep(FALSE, nrow(raw))
	devlargeLeft[which(abs(sgDiffBefore) > allowableJump)] = TRUE

	devlargeRight = rep(FALSE, nrow(raw))
        devlargeRight[which(abs(sgDiffAfter) > allowableJump)] = TRUE

	idx = which(devlargeLeft==TRUE & devlargeRight==TRUE)

	
	##
	## mark large deviations

	raw$deviationLarge[idx] = TRUE


	return(raw)
}
