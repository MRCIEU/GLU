# The MIT License (MIT)
# Copyright (c) 2018 Louise AC Millard, MRC Integrative Epidemiology Unit, University of Bristol

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



# Returns a list of valid days.
# A day is defined as a period between nightStart time on two consecutive days (exclusive of the first and exclusive of the second)
# A valid day should have a complete set of CGM epochs; 288 values for medtronic 5 minute data)
#
# Each valid day object is a list containing 8 objects:
# 1. An integer value, the day number
# 2. A boolean, whether the day is valid or not
# 3. A daytime data frame - the CGM sequence data for the daytime period of this day
# 4. A nighttime data frame - the CGM sequence data for the nighttime period of this day
# 5. Date: day start time
# 6. Date: day end time
# 7. Data frame of events
# 8. Data frame of blood glucose values
# TODO fix for clocks changing

getDays <- function(raw, nightstart, daystart, epochFrequency) {

	daystartX = daystart
	nightstartX = nightstart

	bg = raw[["bg"]]
	events = raw[["events"]]
	library(data.table)

	rawData = raw[["sg"]]
 	rawData$interp = FALSE

	days = list()

	## first time to consider is the first recorded sensor glucose reading (so we have a complete trajectory - i.e. we cannot interpolate before if no previous SG values)
	idxStart=1
	timestart = rawData$time[idxStart]
	timeend = rawData$time[nrow(rawData)]

	##
	## get day and night start time for this day, assuming night-to-day transition is the day after the day-to-night transition

	# month is zero-based
	t1str = paste(timestart$mday, "/", timestart$mon+1, "/" , year(timestart), " ", nightstart$hour, ":", nightstart$min, sep="")
	t2str = paste(timestart$mday, "/", timestart$mon+1, "/" , year(timestart), " ", daystart$hour, ":", daystart$min, sep="")
	thisDayToNight = strptime(t1str, format='%d/%m/%Y %H:%M')
	thisNightToDay = strptime(t2str, format='%d/%m/%Y %H:%M')

	# if cgm doesn't start until after day-to-night threshold then update thresholds to next day
	if (rawData$time[idxStart]>thisDayToNight) {
		thisDayToNight = incrementDay(thisDayToNight)
	}

	# if cgm doesn't start until after night-to-day threshold then update thresholds to next day
	if (rawData$time[idxStart]>thisNightToDay) {
		thisNightToDay = incrementDay(thisNightToDay)
        }


	nighttime=NULL
	countDays = 1
	currentMinutes = NULL
	daystart=thisDayToNight

	# process each time point in cgm data in turn
	for (idxThis in 2:nrow(rawData)) {
		timePrev=rawData$time[idxThis-1]
		timeThis=rawData$time[idxThis]

		#for each minute between previous timepoint (inclusive) and current time point (exclusive)
		minutesForBlock = getMinutesForBlock(rawData[idxThis-1,], rawData[idxThis,], epochFrequency)
		# check if we passed night-to-day or day-to-night thresholds
		if (length(which(minutesForBlock$time == thisNightToDay)) >0) {
#			print('night-to-day')

			# split minutes block
			idxThresh = which(minutesForBlock$time == thisNightToDay)
			nightRemaining = minutesForBlock[1:idxThresh,]
			dayStart = minutesForBlock[idxThresh:nrow(minutesForBlock),]
			
			# set nighttime and current block as start of daytime
			nighttime = rbind(currentMinutes, nightRemaining)
			currentMinutes = dayStart

			# find next night-to-day threshold
			thisNightToDay = incrementDay(thisNightToDay)
		}
		else if (length(which(minutesForBlock$time == thisDayToNight)) >0) {
#			print('day-to-night')

			# split minutes block (days end and start on the same minute)
                        idxThresh = which(minutesForBlock$time == thisDayToNight)
                        dayRemaining = minutesForBlock[1:idxThresh,]
                        nightStart = minutesForBlock[idxThresh:nrow(minutesForBlock),]

			# set daytime and current block as start of nighttime
			daytime = rbind(currentMinutes, dayRemaining)
			currentMinutes = nightStart

			###
                        ### check day is valid and save if it is

                        vdCheck = isValidDay(nighttime, daytime, nightstartX, daystartX)
                        newDay = list(daytime=daytime, nighttime=nighttime, dayidx=countDays, validday=vdCheck, events=events[which(events$time>=daystart & events$time<thisDayToNight),], daystart=daystart, dayend=thisDayToNight, bg=bg[which(bg$time>=daystart & bg$time<thisDayToNight),])
                        days[[countDays]] = newDay
                        if(is.null(nighttime)) {
                                print(paste("valid day: ", vdCheck, " (no nighttime), ", countDays,": ", daytime$time[1], " - ", daytime$time[nrow(daytime)]," (daytime starts at ", daytime$time[1], ")", sep=""))
                        }
                        else {
                              	print(paste("valid day: ", vdCheck, ", ", countDays,": ", nighttime$time[1], " - ", daytime$time[nrow(daytime)]," (daytime starts at ", daytime$time[1], ")", sep=""))
                        }

                        # transition to new day
                        nighttime = NULL
                        daystart=thisDayToNight
                        thisDayToNight = incrementDay(thisDayToNight)
                        countDays = countDays+1

                }
		else {
			# we didn't pass thresholds so add minutes to current day part
		
#			print('no threshold passed')

			# add current minute to current block
                        if (is.null(currentMinutes)) {
                                currentMinutes = minutesForBlock
                        }
                        else {
			    	currentMinutes = rbind(currentMinutes, minutesForBlock)
                      	}

		}

	}

	return(days)
}


getMinutesForBlock <- function(blockStart, blockEnd, epochFrequency) {

	timegap = epochFrequency	

	timePrev = blockStart$time
	timeThis = blockEnd$time

	currentMinutes = NULL

        # start current block
        timeMinute = strptime(paste(timePrev$mday, "/", timePrev$mon+1, "/" , year(timePrev), " ", timePrev$hour, ":", timePrev$min, sep=""), format='%d/%m/%Y %H:%M')
        if (timeMinute<timePrev) {
 		timeMinute = as.POSIXlt(timeMinute + 60)
        }

        # if end time is a round minute it is part of the next block
        while (timeMinute < timeThis) {

        	##
        	## get new interpolated row for this minute
                newrowInterp = dointerpolate(blockStart, blockEnd, timeMinute)
		timeLengthPeriod = as.numeric(difftime(timeThis, timePrev, units="mins"))

		maxFreq = timegap + (timegap/2)
                if (timeLengthPeriod > maxFreq) {
                	newrowInterp$sgReading = NA
                }

		if (is.null(currentMinutes)) {
                	currentMinutes = newrowInterp
                }
                else {
                 	currentMinutes = rbind(currentMinutes, newrowInterp)
                }

		timeMinute = as.POSIXlt(timeMinute+60)

	}

	if (!is.null(currentMinutes)) {
		currentMinutes$time = as.POSIXlt(currentMinutes$time)
	}

	return(currentMinutes)

}


incrementDay <- function(time) {

	timeNew = as.POSIXlt(time + 60*60*24)

	## hour changes if the clocks change
	if (timeNew$hour!=time$hour) {
		# mon+1 because retrieved zero based but set 1-based
		t2str = paste(timeNew$mday, "/", timeNew$mon+1, "/" , year(timeNew), " ", time$hour, ":", timeNew$min, sep="")
		timeNew = strptime(t2str, format='%d/%m/%Y %H:%M')
	}

	return(timeNew)
}


# dateT: the date/time we want to interpolate at
# endIdx: the last time point idx in rawData before we interpolate
interpolate <- function(rawData, idxleft, dateT) {
	

	# get interpolated sg value

	# get next sg value and time so we can use to interpolate at dateT
	sgsAfter = which(rawData$time > dateT)

	if (length(sgsAfter)==0) {
		newrow = data.frame(sgReading=NA, interp=TRUE, time=dateT, deviationLarge=FALSE, impute="")
		return(newrow)
	}

	idxright = min(sgsAfter)

	# if the break period starts on a non-SG row then we need to go back to the last SG row, to do the interpolation
	if (is.na(rawData$sgReading[idxleft])) {
		prevSGIdx = which(!is.na(rawData$sgReading) & rawData$time <= dateT)
		if (length(prevSGIdx) == 0) {
			# cannot interpolate back because no previous SG
			newrow = data.frame(sgReading=NA, interp=TRUE, time=dateT, deviationLarge=FALSE, impute="")
	                return(newrow)
		}
		
		idxright = max(prevSGIdx)
	}


	if (is.null(idxleft) | is.null(idxright)) {
		stop(" could not interpolate!")
	}

	return(dointerpolate(rawData[idxleft], rawData[idxright], dateT))

}

dointerpolate <-function(blockStart, blockEnd, timeToInterp) {

	##
	## do interpolation

	# time and sg value of start position
	timestart = blockStart$time
	sgThis = blockStart$sgReading

	# time and sg value of end position
	timenext = blockEnd$time
	sgNext = blockEnd$sgReading

	# time length from start to end
	timediff = as.numeric(difftime(timenext, timestart, units="secs"))

	# time length from start to interpolation time
	timeToT = as.numeric(difftime(timeToInterp, timestart, units="secs"))

	# different in SG value from start to end
	sgDiff = sgNext - sgThis

	# to interpolation
	sgInterp = sgThis + sgDiff*(timeToT/timediff)

	##
	## make a new time point to add to the cgm data

	newrow = data.frame(sgReading=sgInterp, interp=TRUE, time=timeToInterp, deviationLarge=FALSE, impute='')
	newrow$impute = as.character(newrow$impute)

	return(newrow)

}

insertRow <- function(rawData, idx, newrowInterp) {

	rawDataBottom =	rawData[(idx):nrow(rawData),]

	# raw data before idx
     	rawData = rawData[1:idx-1,]

	# raw data before and at idx
	rawData = rbind(rawData, newrowInterp)

	# raw data, before, at and after idx
        rawData = rbind(rawData, rawDataBottom)

	return(rawData)
}



isValidDay <- function(nighttime, daytime, nightstart, daystart) {


	isvd=FALSE
	if (!is.null(nighttime)) {

		# does night start at right time
		thisnightstart = nighttime$time[1]
		isvnightstart = nightstart$hour == hour(thisnightstart) & nightstart$min == thisnightstart$min

		# does day start at right time
		thisdaystart = daytime$time[1]
                isvdaystart = daystart$hour == hour(thisdaystart) & daystart$min == thisdaystart$min

		# does night end at right time
                thisnightend = nighttime$time[nrow(nighttime)]
                isvnightend = daystart$hour == hour(thisnightend) & daystart$min == thisnightend$min

                # does day end at right time
                thisdayend = daytime$time[nrow(daytime)]
                isvdayend = nightstart$hour == hour(thisdayend) & nightstart$min == thisdayend$min


		# does the day have NAs
		dayHasNAs = length(which(is.na(nighttime$sgReading)))==0 & length(which(is.na(daytime$sgReading)))==0

		isvd = dayHasNAs & isvnightstart & isvdaystart & isvnightend & isvdayend

	}

	return(isvd)
}



