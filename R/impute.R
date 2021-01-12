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
# impute approximal: impute by matching cgm before and after missingness with other sequence without missingness
# imput other: impute by finding a complete time period at the same time on another day
impute <- function(day, alldays, nightstart, imputeOther=FALSE) {

	print(paste("IMPUTE day ", day@dayidx, sep=""))

	numMissing = length(which(is.na(day@glucose$sgReading)))
	# TODO maybe there are some cases where we can't impute - add here?

	if(numMissing <= 6*60) {
		# only	impute if there	is enough data (at most 6 hrs missing)

		if (imputeOther == TRUE) {
			dayx = doImputeOtherDay(day, alldays, nightstart)
		}
		else {
			dayx = doImpute(day)
		}

		# if imputation has been performed then update this day and set as valid
		if (!is.null(dayx)) {
			dayx@validday = TRUE
			day = dayx
			print(paste('************************************ Day ', day@dayidx, ' imputed, now valid', sep=""))
		}
	}
	else {
		print(paste0('More than 6 hours missing (', numMissing, ' minutes) so cannot impute.'))
	}

	return(day)
}




doImputeOtherDay <- function(day, alldays, nightstart) {

	day@glucose$impute = "NO"

	missingBlocks = getMissingBlocks(day@glucose)

	print("DO IMPUTATION FOR THIS DAY")
	for (b in missingBlocks) {
	
		print(paste('block: ', b[["startdate"]], ' - ', b[["enddate"]]))

		# find same time on different day
		startDate = b[["startdate"]]
		endDate = b[["enddate"]]

		candidateDays = NULL
		candidateSeqs = NULL
		for (d in alldays) {
			dx = d@glucose

			# same position across days so use minute indexes
			cSeq = dx$sgReading[b$start:b$end]
				
			if (is.null(candidateSeqs)) {
				candidateSeqs = data.frame(cSeq)
				candidateDays = data.frame(d@dayidx)
			}
			else {
				candidateSeqs = cbind(candidateSeqs, cSeq)
				candidateDays = cbind(candidateDays, d@dayidx)
			}
		}

		# restrict to only seqs with no missingness
		ix = which(colSums(is.na(candidateSeqs))==0)
		candidateSeqs = candidateSeqs[,ix, drop=FALSE]
		candidateDays = candidateDays[,ix, drop=FALSE]
		
		# choose a sequence and impute
		if (!is.null(candidateSeqs) & ncol(candidateSeqs)>0) {
			
			randDayIdx = runif(1, 1, ncol(candidateSeqs))
			seq = candidateSeqs[,randDayIdx]
			dayidx = candidateDays[,randDayIdx]

			print('Found sequence on other day')
			day = updateDayOther(day, b, seq, dayidx)

		}
		else {
			# could not find block in another day to use for imputation
			# so cannot impute this day
			print('Could not find sequence on other day')
			return(NULL)
		}

	}

	return(day)

}


updateDayOther <- function(day, b, seq, dayidx) {

	# update missing block with seq
	idxstart = which(day@glucose$time == b[["startdate"]])
	idxend = which(day@glucose$time == b[["enddate"]])
	day@glucose$sgReading[idxstart:idxend] = seq
        day@glucose$impute[idxstart:idxend] = paste0("OTHER", dayidx)

	return(day)
}
	
doImpute <- function(day) {

	day@glucose$impute = "NO"

	# if there are missing values at the start or end of the sequence then we def cannot impute imbetween
	#if (is.null(raw) | is.na(raw$sgReading[1]) | is.na(raw$sgReading[nrow(raw)])) {
	if (is.na(day@glucose$sgReading[1]) | is.na(day@glucose$sgReading[nrow(day@glucose)])) {
		return(NULL)
	}
	
	missingBlocks = getMissingBlocks(day@glucose)

	# largest missing block
	largestBlock = getLargestBlock(missingBlocks)
	print(paste('largest block', largestBlock))

	# only impute if all missing blocks are less than 2 hours long
	if (largestBlock<120) {

		print("DO IMPUTATION FOR THIS DAY")
		for (b in missingBlocks) {

			print(paste('block: ', b[["startdate"]], ' - ', b[["enddate"]]))
			# find sequence at similar time with similar CGM summary statistics for adjacent hour (before and after)

			seq = fillWithNearby(b, day@glucose)
			if (is.null(seq)) {
				# not enough nearby data to use for imputing
				return(NULL)
			}

			# replace missing block with identified non-missing block
			
			day = updateDay(day, b, seq)

		}

                return(day)

	}
	else {
#         	     	print('largest missing block too long')
		return(NULL)
	}

}


updateDay <- function(day, b, seq) {
	
	# update missing block with seq
	idxstart = which(day@glucose$time == b[["startdate"]])
	idxend = which(day@glucose$time == b[["enddate"]])

	day@glucose$sgReading[idxstart:idxend] = seq
	day@glucose$impute[idxstart:(idxstart-1+floor(length(seq)/2))] = "LEFT"
	day@glucose$impute[(idxstart-1+ceiling(length(seq)/2)):idxend] = "RIGHT"

        return(day)

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

	seq = c(seqLeft, seqRight)

	# if there are NAs in the sequence could not impute with nearby data so return null
	if(length(which(is.na(seq)))>0) {
		return(NULL)
	}

	return(seq)
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
			idxNextNotNA = which(idxNotNA>idx)

			# get end of this NA block (either before a non NA time point or the end of the sequence if there isn't one
			if (length(idxNextNotNA)>=1) {
				idxend = min(idxNotNA[idxNextNotNA]) - 1
			}
			else {
				# last NA position
				idxend = idxNA[length(idxNA)]
			}

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






