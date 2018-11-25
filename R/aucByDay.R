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



# For each valid day in validDays, generate the auc
# Returns a data frame - the auc values of each day, and the average across all days overall.
aucByDay <- function(validDays) {

	aucs = c()

	# column names for each auc value we generate	
	cnames = c()

	count=1

	# for each valid day, calculate the auc
	for (vd in validDays) {
		
		# sequence of timepoints for this day
		raw = collateSequence(vd)

		# auc of this day only
		aucVD = auc(raw)

		# nighttime, daytime aucs
		aucVDn = auc(raw[which(raw$daytime==FALSE),])
		aucVDd = auc(raw[which(raw$daytime==TRUE),])

		aucs = append(aucs, c(aucVD, aucVDn, aucVDd))
		
		cnames = append(cnames, c(paste("auc_day", count, sep=""), paste("auc_nt_day", count, sep=""), paste("auc_dt_day", count, sep="")))

                count=count+1
	}

	# set column names for auc values
	res = rbind(aucs)
	colnames(res) = cnames

	aucAv = meanAcrossDays("auc_day", res)
	aucAvN = meanAcrossDays("auc_nt_day", res)
	aucAvD = meanAcrossDays("auc_dt_day", res)

	othervars = c(aucAv, aucAvN, aucAvD)
  	othervars = rbind(othervars)
	colnames(othervars) = c("meanAUCperDay", "meanAUCperDay_nt", "meanAUCperDay_dt")

	res = cbind(res, othervars)

	return(res)

}
