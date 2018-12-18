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



# For each valid day store in validDays arg, calculate the number of timepoints that have an 
# invalid SG reading deviation, defined as being >2SD away from the value of the previous timepoint.
# By the time this function is used, invalid deviations have been marked in the deviationLarge column
# Returns a data frame containing the number of deviations that occured on each day.
invalidDeviationsByDay <- function(validDays) {

	invalidDeviations = c()

	# column names for each invalid deviation value we generate	
	cnames = c()

	count=1

	hasInvalidDeviations = FALSE

	# for each valid day, calculate the number of invalid deviations
	for (vd in validDays) {

		# get day and night-time sequences
		dt = vd[["daytime"]]
		nt = vd[["nighttime"]]

		# get number of invalid deviations in this day only
		devVD1 = length(which(dt$deviationLarge == TRUE))
		devVD2 = length(which(nt$deviationLarge == TRUE))

		devs = devVD1+devVD2

		if (devs>0) {
			hasInvalidDeviations = TRUE
		}

                count=count+1
	}

	# add marker indicating if there are any abnormally large deviations
	invdevdf = c(hasInvalidDeviations)
        invdevdf = rbind(invdevdf)
        colnames(invdevdf) = c("hasInvalidDeviations")
	return(invdevdf)

}
