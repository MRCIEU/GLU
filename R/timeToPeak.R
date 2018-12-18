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



# Finds the nearest subsequent SG peak, after idxMeal in raw.
# Returns the time in minutes, from the timepoint at idxMeal to this peak.
# Where the peak is a plateau (multiple consecutive timepoints have the same SG value) then the time to the earliest timepoint of the plateau is used.
# Where no peak is found before the end of the sequence is reached, NA is returned.
timeToPeak <- function(raw, eventTime) {


	# get remaining sequence
	raw = raw[max(which(raw$time<=eventTime)):nrow(raw),]

	peakTime = NA

	# parse each time point after meal idx, until peak is reached
	if (nrow(raw)<=1) {
		return(NA)
	}
     
	start = raw$sgReading[1]

	currentIdx1 = 2
	currentIdx2 = NA

	# loop to the end of the sequence but stop early if we find a peak
	while (currentIdx1<nrow(raw) & (is.na(currentIdx2) | currentIdx2<nrow(raw))) {

		current = raw$sgReading[currentIdx1]

		# no plateau
		if (is.na(currentIdx2)) {

			nextIdx = currentIdx1+1
		} else {

			nextIdx = currentIdx2+1
		}

		nextx = raw$sgReading[nextIdx]

		if (current>nextx & current > start) {

                        # time peak after meal occurred
                        peakTime = raw$time[currentIdx1]

                        break
                }
		else if (current == nextx) {
			# in a plateau so set end idx of this plateau
			currentIdx2 = nextIdx
		}
		else {
			# not in a plateau so just set the current position we are looking at
			currentIdx1 = nextIdx
			currentIdx2 = NA
		}

	}


	# if a peak is found the return the time-to-peak
	if (!is.na(peakTime)) {
		timeToPeak = as.numeric(difftime(peakTime, eventTime, units="mins"))
	}
	else {
		timeToPeak = NA
	}


	return(timeToPeak)
}
