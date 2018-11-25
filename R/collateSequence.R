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



# This file contains a series of scripts for retieving the SG sequence of a day (i.e. combining the daytime and nighttime sequences) in different ways.

# Join the day and night of vd including the interpolated end points.
# This is used to derive statistics based on the area under the curve e.g. AUC, proportion of time, etc.
# Returns a data frame containing the data for the whole day (daytime and nighttime) of vd.
collateSequence <- function(vd) {

        # get day and night-time sequence
        dt = vd[["daytime"]]
        nt = vd[["nighttime"]]

	# store day or night time 
	dt$daytime = TRUE
	dt$dayidx = vd[["dayidx"]]

	if (!is.null(nt)) {
		nt$daytime = FALSE
		nt$dayidx = vd[["dayidx"]]
	}

	daySeq = NULL

        if (!is.null(dt) & !is.null(nt) ) {
		# last nt time point and first dt time point are the same
                daySeq = rbind(nt,dt[2:nrow(dt),])
        }
	else if (!is.null(dt)) {
                daySeq = dt
        }
	else if (!is.null(nt)) {
                daySeq = nt
        }

	# check time length of day is correct - should be 86400 seconds
#        timeLengthPeriod = as.numeric(difftime(daySeq$time[nrow(daySeq)], daySeq$time[1], units="secs"))
#	stopifnot(timeLengthPeriod==86400)

	return(daySeq)
}

