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



# Derive post-exercise 1hr and 2hr SG levels - the average of the SG readings in the 15 minute period 1 and 2 hrs after each exercise event, respectively.
# Returns the average of these statistics per day, and across all days overall.
exerciseStatistics <- function(events, raw) {

        # get rows indexes with exercise events
	idxEx = which(events$event == "EXERCISE")

	ex = data.frame(time=c(), time_to_peak=c(), postprand_1hr=c(), postprand_2hr=c())

	if (length(idxEx)==0) {
		print("No exercise")
		return(new("event", events = ex, meantimetopeak = NA_real_, meanpp1 = NA_real_, meanpp2 = NA_real_))
	}

	if (length(idxEx)>0) {
	for (i in 1:length(idxEx)) {

		idxThisEx = idxEx[i]
		
		# 1-hr and 2-hr postprandial glucose
		pp1 = postprandial(raw,	events$time[idxThisEx], 1)
		pp2 = postprandial(raw, events$time[idxThisEx], 2)	

		ex_sum = data.frame(time=events$time[idxThisEx], time_to_peak = NA_real_, postprand_1hr=pp1, postprand_2hr=pp2)
		ex = rbind(ex, ex_sum)
	}
	}

	# average values
	pp1sMean = mean(ex$postprand_1hr, na.rm=TRUE)
	pp2sMean = mean(ex$postprand_2hr, na.rm=TRUE)

	events = new("event", events = ex, meantimetopeak = NA_real_, meanpp1 = pp1sMean, meanpp2 = pp2sMean)

        return(events)

}

