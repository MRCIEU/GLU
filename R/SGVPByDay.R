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



# For each valid day in validDays, derive the SGVP
# Returns the SGVP for each day, and the average across all days overall.
SGVPByDay <- function(validDays) {

	sgvps = c()	

	cnames=c()
	count=1
	for (vd in validDays) {

		raw = collateSequence(vd)

		# SGVP of this valid day only
		sgvpVD = SGVP(raw, TRUE)

		# nighttime, daytime SGVP
		sgvpVDn = SGVP(raw[which(raw$daytime==FALSE),], TRUE)
		sgvpVDd = SGVP(raw[which(raw$daytime==TRUE),], TRUE)

		sgvps = append(sgvps, c(sgvpVD, sgvpVDn, sgvpVDd))

		cnames = append(cnames, c(paste("SGVP_day", count, sep=""), paste("SGVP_nt_day", count, sep=""), paste("SGVP_dt_day", count, sep="")))

                count=count+1
	}


	res = rbind(sgvps)
        colnames(res) = cnames

	sgvpAv = meanAcrossDays("SGVP_day", res)
	sgvpAvN = meanAcrossDays("SGVP_nt_day", res)
	sgvpAvD = meanAcrossDays("SGVP_dt_day", res)

	othervars = c(sgvpAv, sgvpAvN, sgvpAvD)
  	othervars = rbind(othervars)
	colnames(othervars) = c("meanSGVPPerDay", "meanSGVPPerDay_nt","meanSGVPPerDay_dt")
	res = cbind(res, othervars)

	return(res)

}
