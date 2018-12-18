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



# Derives the post event SG - the average of the 15 minute period starting timeGapHours hours after the event.
# Returns the post event SG value.
postprandial <- function(raw, eventTime, timeGapHours) {

	ppStartTime = eventTime + timeGapHours*60*60

	# find the nearest timepoint before the start time with a sg reading
	ppStarts = which(raw$time <= ppStartTime)

	if (length(ppStarts)==0) {
		return(NA)
	}
	
	seq = raw
	
	##
	## get start idx

	ix = max(ppStarts)
	if (ppStartTime == seq$time[ix] & !is.na(seq$sgReading[ix])) {
		startIdx = ix
	}
	else {
		return(NA)
	}

	##
	## get end idx

        ppEndTime = eventTime + timeGapHours*60*60 + 15*60
        ppEnds = which(seq$time <= ppEndTime & !is.na(seq$sgReading))

	ix = max(ppEnds)
        if (ppEndTime == seq$time[ix] & !is.na(seq$sgReading[ix])) {
                endIdx = ix
        }
        else {
		return(NA)
        }

	##
	## sub-sequence for this post-event period

	subseq = seq[startIdx:endIdx,]

	##
	## calculate AUC for this

	subseqAuc = auc(subseq)

	return(subseqAuc)
}





# Returns the next 3 SG readings at and after startIdx
nextThreeSGReadings <- function(raw, startIdx) {

	# remaining sequence
	raw = raw[startIdx:nrow(raw),]

	rawSubset = c()
	idxCurrent = 1
	while(idxCurrent <= nrow(raw)) {

		if (!is.na(raw$sgReading[idxCurrent])) {

			rawSubset = c(rawSubset, raw$sgReading[idxCurrent])
		}

		idxCurrent = idxCurrent + 1

		if (length(rawSubset)==3) {
			return(rawSubset)
		}

	}
	
	return(NA)

}
