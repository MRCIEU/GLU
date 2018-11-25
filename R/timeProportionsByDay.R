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



# For each valid day in validDays, derive the proportion of time spent in low, normal and high SG values.
# The thresholds used are different depending if the study is of the general population or during pregnancy.
# Returns a data frame with the time proportions for each day, and the average across all days overall.
timeProportionsByDay <- function(validDays, lowT, highT) {

	tps = c()	
	cnames = c()
	count=1

	# sums used to calculate the means per day
	lowSum = 0
	normSum = 0
	highSum = 0

	for (vd in validDays) {

		# sequence of timepoints for this day
		raw = collateSequence(vd)

		# auc of this valid day only
		tpVD = timeProportions(raw, lowT, highT)

		# nighttime, daytime aucs
		tpVDn = timeProportions(raw[which(raw$daytime==FALSE),], lowT, highT)
		tpVDd = timeProportions(raw[which(raw$daytime==TRUE),], lowT, highT)

		tps = append(tps, c(tpVD, tpVDn, tpVDd))

		# name of each variable
		cnames = append(cnames, paste("low_day", count, sep=""))
		cnames = append(cnames, paste("norm_day", count, sep=""))
		cnames = append(cnames, paste("high_day", count, sep=""))
		cnames = append(cnames, paste("low_nt_day", count, sep=""))
                cnames = append(cnames, paste("norm_nt_day", count, sep=""))
                cnames = append(cnames, paste("high_nt_day", count, sep=""))
		cnames = append(cnames, paste("low_dt_day", count, sep=""))
                cnames = append(cnames, paste("norm_dt_day", count, sep=""))
                cnames = append(cnames, paste("high_dt_day", count, sep=""))

		count=count+1
		
		lowSum = lowSum + tpVD[1]
		normSum = normSum + tpVD[2]
		highSum = highSum + tpVD[3]

	}

	# set column names for auc values
        res = rbind(tps)
        colnames(res) = cnames

	# calculate means across valid days

	tpLAv = meanAcrossDays("low_day", res)
	tpNAv = meanAcrossDays("norm_day", res)
	tpHAv = meanAcrossDays("high_day", res)
        tpLAvN = meanAcrossDays("low_nt_day", res)
	tpNAvN = meanAcrossDays("norm_nt_day", res)
	tpHAvN = meanAcrossDays("high_nt_day", res)
        tpLAvD = meanAcrossDays("low_dt_day", res)
        tpNAvD = meanAcrossDays("norm_dt_day", res)
        tpHAvD = meanAcrossDays("high_dt_day", res)

	othervars = rbind(c(tpLAv, tpNAv, tpHAv, tpLAvN, tpNAvN, tpHAvN, tpLAvD, tpNAvD, tpHAvD))
        colnames(othervars) = c("meanProportionLowPerDay", "meanProportionNormalPerDay", "meanProportionHighPerDay","meanProportionLowPerDay_nt", "meanProportionNormalPerDay_nt", "meanProportionHighPerDay_nt","meanProportionLowPerDay_dt", "meanProportionNormalPerDay_dt", "meanProportionHighPerDay_dt")

	# add average values to derived statistics
	res = cbind(res, othervars)

	return(res)

}
