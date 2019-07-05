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



# Calculate standardised glycemic variability percentage (sGVP) of SG readings in raw.
# Returns the sGVP value.
SGVP <- function(data, stdx) {

	if (stdx==TRUE) {
		# standardise sequence (x - median)/MAD
		# MAD function rescales so MAD=SD if normally distributed
		madsg = mad(data$sgReading, constant=1, na.rm=TRUE)
		
		mediansg = median(data$sgReading)
		data$sgReading = (data$sgReading - mediansg)/madsg
	}


	# for consecutive pair of measurements calculate the GVP for this part
	sgvp = 0
	overallTimeDiffMins = 0

	for (idx in 2:nrow(data)) {

		# don't include variation between non-imputed and imputed regions, and left and right parts of imputed regions
		if (data$impute[idx] == data$impute[idx-1]) {

			timeDiffMins = as.numeric(difftime(data$time[idx], data$time[idx-1], units="mins"))
			
			# sometimes a day might have two night/day segments if the start of the day period isn't exactly on the day-night or night-day boundary
			if (timeDiffMins == 1) {
				sgvpThis = sqrt((data$sgReading[idx-1] - data$sgReading[idx])^2 + timeDiffMins^2)
				sgvp = sgvp + sgvpThis
	
				# need to add here as if imputed segment then it is skipped
				overallTimeDiffMins = overallTimeDiffMins + timeDiffMins
			}
		}
	}
	sgvp = sgvp/overallTimeDiffMins

	# rescaling so it's a % of the minimum length of the line
	sgvp = (sgvp - 1) * 100

	return(sgvp)

}
