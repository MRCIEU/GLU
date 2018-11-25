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



# For each valid day in validDays, generate the number of peaks
# Returns a data frame - the number of peaks in each day, and the average across all days overall.
numPeaksByDay <- function(validDays) {

	nps = c()

	# column names for each auc value we generate	
	cnames = c()

	count=1

	# for each valid day, calculate the auc
	for (vd in validDays) {
		
		# sequence of timepoints for this day
		raw = collateSequence(vd)

		# auc of this day only
		npVD = numPeaks(raw)

		nps = append(nps, c(npVD))
		
		cnames = append(cnames, c(paste("numpeaks_day", count, sep="")))

                count=count+1
	}

	# set column names for auc values
	res = rbind(nps)
	colnames(res) = cnames

	npAv = meanAcrossDays("numpeaks_day", res)

	othervars = c(npAv)
  	othervars = rbind(othervars)
	colnames(othervars) = c("meannumpeaksperDay")

	res = cbind(res, othervars)

	return(res)

}
