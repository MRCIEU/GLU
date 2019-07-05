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



# Calculate proportion of time in low, normal and high glucose levels, using linear interpolation between measurements.
# The thresholds used are different depending if the study is of the general population or during pregnancy or for diabetics.
# Returns a data frame with the time proportions for low, normal and high, for sequence raw.
timeProportions <- function(data, lowT, highT) {

	# init time spent in each range
	lowTime = 0
	normTime = 0
	highTime = 0

	for (idx in 2:nrow(data)) {
		sgmin = min(data$sgReading[idx], data$sgReading[idx-1])
		sgmax = max(data$sgReading[idx], data$sgReading[idx-1])
		timeStart = data$time[idx-1]
		timeEnd = data$time[idx]

		tPart = unlist(timePropPart(sgmin, sgmax, timeStart, timeEnd, lowT, highT))

		lowTime = lowTime + tPart[1]
		normTime = normTime + tPart[2]
		highTime = highTime + tPart[3]
	}

	# get time as proportion of total
	total = lowTime + normTime + highTime
	lowTime = lowTime/total
	normTime = normTime/total
	highTime = highTime/total

	return(c(lowTime, normTime, highTime))

}

# Calculates time proportions for a consecutive pair of SG measurements
# sgmin arg should be <= sgmax arg

# general population thresholds : 3.3<= normal < 10
# pregnancy thresholds: 3.9<= normal < 7.8
# diabetes thresholds: 3.9<= normal < 10
# or thresholds can be set in tool args
timePropPart <- function(sgmin, sgmax, timeStart, timeEnd, lowT, highT) {
	
	# init time in ranges
	lowTime = 0
	normalTime = 0
	highTime = 0

	timeDiffSecs = as.numeric(difftime(timeEnd, timeStart, units="secs"))	

	# both SGmin and SGmax are in same range (either low, normal or high)
	if (sgmin<lowT & sgmax<lowT) { 
		lowTime=lowTime + timeDiffSecs; # both in low range
	}
	else if (sgmin>=lowT & sgmin<highT & sgmax>=lowT & sgmax<highT) {
		 normalTime=normalTime + timeDiffSecs; # both in normal range
	}
	else if (sgmin>=highT & sgmax>=highT) {
		 highTime=highTime + timeDiffSecs; # both in high range
	}
	else if (sgmin<lowT) { # sgmin value is in low range and SGmax is in either mid or high range
	        lowTime=lowTime + timeDiffSecs*(lowT-sgmin)/(sgmax-sgmin)
	        if (sgmax<highT) {
			# sgmin and sgmax span low and normal ranges
	                normalTime=normalTime + timeDiffSecs*(sgmax-lowT)/(sgmax-sgmin)
	        }
		else {
			# sgmin and sgmax span all three ranges
	            	normalTime=normalTime + timeDiffSecs*(highT-lowT)/(sgmax-sgmin)
	                highTime=highTime + timeDiffSecs*(sgmax-highT)/(sgmax-sgmin)
		}
	}
	 else if (sgmin<highT) { # sgmin and sgmax values are in normal and high ranges resp.
        	normalTime=normalTime + timeDiffSecs*(highT-sgmin)/(sgmax-sgmin)
        	highTime=highTime + timeDiffSecs*(sgmax-highT)/(sgmax-sgmin)
	}

	return(list(lowTime, normalTime, highTime))
}




