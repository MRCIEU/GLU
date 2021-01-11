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



# For each valid day in validDays, generate the start dates/time of each valid day
# Returns a data frame - the date/time string values of each day
datesByDay <- function(validDays) {

	dates = c()

	# column names for each date/time string value we generate	
	cnames = c()

	count=1

	# for each valid day, get the first date/time of the day
	for (vd in validDays) {
		
		# date/time of the start of this day
		glucoseThis = vd@glucose
		dateVD = format(glucoseThis$time[1], "%Y-%m-%d %H:%M:%S %Z")

		dates = append(dates, dateVD)
		cnames = append(cnames, paste("datetime_start_day", count, sep=""))

                count=count+1
	}

	# set column names for date values
	res = rbind(dates)
	colnames(res) = cnames

	return(res)
}
