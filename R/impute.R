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


# day input is an invalid day (with some missingness)
# Determines if imputation is possible depending on amount of missingness
# impute by matching cgm before and after missingness with other sequence without missingness
impute <- function(day) {


	# get combined sequence - we work on this but then update to the particular days
	combined = collateSequence(day)

	print(paste("IMPUTE day ", day[["dayidx"]], sep=""))
	dt = day[["daytime"]]
	nt = day[["nighttime"]]

	# we can't impute days with no nighttime - too much missingness
	if (is.null(nt)) {
		return(day)
	}

	numMissing = length(which(is.na(nt$sgReading))) + length(which(is.na(dt$sgReading)))
	print(paste('num missing:', numMissing))
	# TODO maybe there are some cases where we can't impute - add here?

	if(numMissing <= 6*60) {
		# only	impute if there	is enough data (at most 6 hrs missing)

		dayx = doImpute(day, combined)

		# if imputation has been performed then update this day and set as valid
		if (!is.null(dayx)) {
			dayx[["validday"]] = TRUE
			day = dayx
			print(paste('************************************ Day ', day[["dayidx"]], ' imputed, now valid', sep=""))
		}
	}

	return(day)
}
	
doImpute <- function(day, combined) {

	dt = day[["daytime"]]
        nt = day[["nighttime"]]

	dt$impute = ""
	nt$impute = ""

	# if there are missing values at the start or end of the sequence then we def cannot impute imbetween
	#if (is.null(raw) | is.na(raw$sgReading[1]) | is.na(raw$sgReading[nrow(raw)])) {
	if (is.na(combined$sgReading[1]) | is.na(combined$sgReading[nrow(combined)])) {
#		print("XXXX")
		return(NULL)
	}
	
	missingBlocks = getMissingBlocks(combined)

	# largest missing block
	largestBlock = getLargestBlock(missingBlocks)
	print(paste('largest block', largestBlock))

	# only impute if all missing blocks are less than 2 hours long
	if (largestBlock<120) {

		print("DO IMPUTATION FOR THIS DAY")
		for (b in missingBlocks) {

			print(paste('block: ', b[["startdate"]], ' - ', b[["enddate"]]))
			# find sequence at similar time with similar CGM summary statistics for adjacent hour (before and after)

			seq = fillWithNearby(b, combined)
			if (is.null(seq)) {
				# not enough nearby data to use for imputing
				return(NULL)
			}

			# replace missing block with identified non-missing block
			
			if (b[["enddate"]] <= nt$time[nrow(nt)]) {
				# block is in the night so update nighttime sequence
				idxstart = which(nt$time == b[["startdate"]])
				idxend = which(nt$time == b[["enddate"]])
				nt$sgReading[idxstart:idxend] = seq
				nt$impute[idxstart:(idxstart-1+floor(length(seq)/2))] = "LEFT"
				nt$impute[(idxstart-1+ceiling(length(seq)/2)):idxend] = "RIGHT"
			}
			else if (b[["startdate"]] >= dt$time[1]) {
				# block	is in the day so update nighttime sequence
				idxstart = which(dt$time == b[["startdate"]])
                                idxend = which(dt$time == b[["enddate"]])
                                dt$sgReading[idxstart:idxend] = seq
				dt$impute[idxstart:(idxstart-1+floor(length(seq)/2))] = "LEFT"
                                dt$impute[(idxstart-1+ceiling(length(seq)/2)):idxend] = "RIGHT"
			}
			else {
				# block	crosses the night/day boundary so update both night and day sequences
				
				# night time part
				idxstart = which(nt$time == b[["startdate"]])
                                idxend = nrow$nt
				nt$sgReading[idxstart:idxend] = seq[idxend - idxstart + 1]
				nt$impute[idxstart:(idxstart-1+floor(length(seq)/2))] = "LEFT"
                                nt$impute[(idxstart-1+ceiling(length(seq)/2)):idxend] = "RIGHT"

				# day time part
				idxstart = 1
				idxend = which(dt$time == b[["enddate"]])
                                dt$sgReading[idxstart:idxend] = seq[length(seq) - (idxend-idxstart+1):length(seq)]
				dt$impute[idxstart:(idxstart-1+floor(length(seq)/2))] = "LEFT"
                                dt$impute[(idxstart-1+ceiling(length(seq)/2)):idxend] = "RIGHT"

			}

		}

		day[["nighttime"]] = nt
                day[["daytime"]] = dt
                return(day)

	}
	else {
#         	     	print('largest missing block too long')
		return(NULL)
	}

}

fillWithNearby <- function(block, daycombined) {

	start = block[["start"]]
	end = block[["end"]]
	blocklength = end - start + 1

	blockhalf1 = floor(blocklength/2)
	blockhalf2 = ceiling(blocklength/2)

	leftstart = start - blockhalf1
	if (leftstart<=0) {
		return(NULL)
	}
	seqLeft = daycombined$sgReading[leftstart:(start-1)]

	rightend = end + blockhalf2
	if (rightend>nrow(daycombined)) {	
		return(NULL)
	}
	seqRight = daycombined$sgReading[(end+1):rightend]

	return(c(seqLeft, seqRight))
}


getMissingBlocks <- function(data) {

	# list of missing blocks, identified by their start and end position (both inclusive)
        mbs = list()

	# block number
	count=1

	idxNA = which(is.na(data$sgReading))
	if (length(idxNA) == 0) {
		return(mbs)
	}

	# start index of first missing block
	idx= min(idxNA)

        if (nrow(data)>1) {
 
               while (idx<=nrow(data)) {

			idxNotNA = which(!is.na(data$sgReading))
			idxend = min(idxNotNA[which(idxNotNA>idx)]) - 1

			leftstart = idx - 60
			leftseq = NULL
			if (leftstart>0) {
				leftseq = data[leftstart:(idx-1),]
			}
			rightend = idxend + 60
                        rightseq = NULL
                        if (rightend<=nrow(data)) {
                                rightseq = data[(idxend+1):rightend,]
                        }

			newMB = list(start=idx, end=idxend, startdate=data$time[idx], enddate=data$time[idxend], left=leftseq, right=rightseq)
			mbs[[count]] = newMB

			# get start of next missing block
			idxNA = which(is.na(data$sgReading))
			idxNA = idxNA[which(idxNA>idxend)]
			if (length(idxNA) == 0) {
				break
			}
			idx = min(idxNA)

			count = count+1
                }
        }

	return(mbs)

}

getLargestBlock <- function(mbs) {

	largest = 0

	for (mb in mbs) {

		idxDiff = mb[["end"]] - mb[["start"]]

		if (idxDiff > largest) {
			largest = idxDiff
		}

        }

	return(largest)

}






