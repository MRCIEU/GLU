
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


## UNUSED

writeMissingSummaryByDay <- function(alldays, impute, userID, filename, outdir) {

        for (d in alldays) {
		writeMissingSummary(d, impute, userID, filename, outdir)
	}
}


writeMissingSummary <- function(day, impute, userID, filename, outdir) {

        nt = day[["nighttime"]]
        dt = day[["daytime"]]

	daySeq = NULL

        # we don't care there is a duplicate time point (in both daytime and nighttime) because the area between them is zero so this is ignored in calculations
        if (!is.null(dt) & !is.null(nt) ) {
                daySeq = rbind(nt,dt[2:nrow(dt),])
        }
	else if (!is.null(dt)) {
               daySeq = dt
        }
	else if (!is.null(nt)) {
                daySeq = nt
        }

        proportionComplete = length(which(!is.na(daySeq$sgReading)))/nrow(daySeq)
	timediff = as.numeric(difftime(daySeq$time[nrow(daySeq)], daySeq$time[1], units="mins"))

	numNotNA = length(which(!is.na(daySeq$sgReading)))

namePrefix = ""
if (!is.null(filename)) {
        namePrefix = paste('-', userID, sep='')
}
if (impute==TRUE) {
namePrefix = paste(namePrefix, '-imputed', sep='')
}
missingfile=paste(outdir,"missingSummary",namePrefix,".csv", sep="")

        write(paste(userID, day[["validday"]], length(daySeq$sgReading), numNotNA, timediff, day[["daystart"]], day[["dayend"]], proportionComplete, sep=","), file=missingfile, append=TRUE)

}
