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



# For each day mark large deviations
markLargeDeviationsByDay <- function(days, outlierthreshold=5) {


	alldaysMarked = list()
	countDays=1

	# for each day, mark large deviations
	for (vd in days) {

		raw = collateSequence(vd)
		raw = markLargeDeviations(raw, outlierthreshold)

		# split back into night- and day-time
		numrowsNT = nrow(vd[["nighttime"]])		
		newNT = raw[1:numrowsNT,]
		newDT = raw[numrowsNT:nrow(raw),]

		# check number of minutes in night and day is still correct and update vd
		stopifnot(nrow(newDT) == nrow(vd[["daytime"]]))
		stopifnot(nrow(newNT) == nrow(vd[["nighttime"]]))
		vd[["daytime"]] = newDT
		vd[["nighttime"]] = newNT

		alldaysMarked[[countDays]] = vd

                countDays=countDays+1

	}

	return(alldaysMarked)

}
