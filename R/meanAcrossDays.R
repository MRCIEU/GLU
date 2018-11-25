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



# Data frame df contains 1 row where the columns correspond to derived statistics for a particular day.
# Extracts all statistics of a particular type.
# Returns the mean value of these extracted values.
meanAcrossDays <- function(colName, df) {

	thisData = df[ ,grepl(colName, colnames(df))]

	# no events for any days so don't calculate the average
	if (length(which(is.na(df))) == ncol(df) ) {
		return(NA)
	}
	else {

		# convert to vector without NAs
		comb = as.vector(unlist(thisData))
		comb = na.omit(comb)

		if (length(comb) == 0 ) {
			return(NA)
		}

		return(mean(comb))
	}
}
