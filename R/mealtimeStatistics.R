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



# Derive post-mealtime (postprandial) time-to-peak, and 1hr and 2hr SG levels - the average of the SG readings in the 15 minute period 1 and 2 hrs after each mealtime event, respectively.
# Returns the average of these statistics per day, and across all days overall.
mealtimeStatistics <- function(events, raw) {

        # get rows indexes with meals
	idxMeal = which(events$event == "MEAL")

	if (length(idxMeal)==0) {
		print("No meals")
		return(data.frame(NA,NA,NA))
	}

	tTPs = c()
	pp1s = c()
	pp2s = c()

	if (length(idxMeal)>0) {
	for (i in 1:length(idxMeal)) {

		idxThisMeal = idxMeal[i]

		# time to peak
		tTP = timeToPeak(raw, events$time[idxThisMeal])

		# 1-hr and 2-hr postprandial glucose
		pp1 = postprandial(raw, events$time[idxThisMeal], 1)
                pp2 = postprandial(raw, events$time[idxThisMeal], 2)	

		tTPs = c(tTPs, tTP)
		pp1s = c(pp1s, pp1)
		pp2s = c(pp2s, pp2)
	}
	}

	# average values
	tTPsMean = mean(tTPs, na.rm=TRUE)
        pp1sMean = mean(pp1s, na.rm=TRUE)
        pp2sMean = mean(pp2s, na.rm=TRUE)

        # combine values into one data frame and add column names
        summ = data.frame(tTPsMean, pp1sMean, pp2sMean)

        return(summ)

}

