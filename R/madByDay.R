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



# For each valid day in validDays, derive the median absolute deviation (MAD).
# Returns the MAD for each day, and the average across all days overall.
madByDay <- function(validDays) {

	mads = c()

	cnames=c()
	count=1
	for (vd in validDays) {

		raw = collateSequence(vd)

		# MAD of this valid day only
		madVD = mad(raw$sgReading, constant=1, na.rm=TRUE)

		# nighttime, daytime aucs
		madVDn = mad(raw$sgReading[which(raw$daytime==FALSE)], constant=1, na.rm=TRUE)
		madVDd = mad(raw$sgReading[which(raw$daytime==TRUE)], constant=1, na.rm=TRUE)

		mads = append(mads, c(madVD, madVDn, madVDd))
		cnames = append(cnames, c(paste("mad_day", count, sep=""), paste("mad_nt_day", count, sep=""), paste("mad_dt_day", count, sep="")))


                count=count+1
	}


	res = rbind(mads)
        colnames(res) = cnames

	madAv = meanAcrossDays("mad_day", res)
	madAvN = meanAcrossDays("mad_nt_day", res)
	madAvD = meanAcrossDays("mad_dt_day", res)
	othervars = c(madAv, madAvN, madAvD)
  	othervars = rbind(othervars)
	colnames(othervars) = c("meanmadPerDay", "meanmadPerDay_nt","meanmadPerDay_dt")
	res = cbind(res, othervars)

	return(res)

}
