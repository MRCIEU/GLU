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



# For each valid day in validDays, derive the fasting proxy SG - the mean of the 30 minutes with the lowest SG values.
# Returns a data frame of the fasting proxy for each day, and the average across all days overall
fastingProxyByDay <- function(validDays) {

	nocts = c()

	# column names for each auc value we generate	
	cnames = c()

	count=1

	# for each valid day, calculate the auc
	for (vd in validDays) {
		
		# nocturnal mean of 3 lowest consecutive values of this day only
		noctVD = fastingProxy(vd$nighttime)
		nocts = append(nocts, noctVD)

		cnames = append(cnames, paste("fastingproxy_day", count, sep=""))
                count=count+1
	}

	# set column names for auc values
	res = rbind(nocts)
	colnames(res) = cnames

	# average across days
	nocAv = meanAcrossDays("fastingproxy_day", res)
  	othervars = rbind(c(nocAv))
	colnames(othervars) = c("meanFastingProxyPerDay")

	res = cbind(res, othervars)
	return(res)

}
