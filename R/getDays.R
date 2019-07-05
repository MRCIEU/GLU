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



# Returns a participantData object
# A day is defined as a period between dayPeriodStartTime on adjacent days (exclusive of the first and exclusive of the second)
# if either nightstart or daystart are the same as dayPeriodStartTime then there is only one day and one night block.
# if e.g. dayPeriodStartTime is during the daytime period, then there is two day blocks with a night block in the middle.
# A valid day should have a complete set of CGM epochs; 288 values for medtronic 5 minute data)
#
# clocks going back: skip the second lot of the duplicated time points
# clocks going forward: this causes a missing time period that would make the day invalid if using the complete days approach, 
# but could potentially be imputed.

getDays <- function(raw, rs) {

	print('splitting into days ...')

	# all the data for this participant
	participantData = methods::new("participantData", nightstart=rs@nightstart, daystart=rs@daystart, days=list())

	# firstvalid arg - can be used to use the first time point as the start of a day period
	# dayPeriodStartTime arg - can be used to use a fixed time, that is the same across people
	# daytimeStart arg - the start of the daytime part of a day
	# nighttimeStart arg - the start of the nighttime part of a day

	bg = raw[["bg"]]
	events = raw[["events"]]

	rawData = raw[["sg"]]
 	rawData$interp = FALSE

	if (length(which(!is.na(rawData$sgReading)))==0) {
		return(NULL)
	}

	# get the first date/time of our data
	dayPeriodStart = getFirstDayPeriodStart(rawData, rs)

	if (dayPeriodStart < rawData$time[1]) {
		# day period starts before start of person's data so add this timepoint to the start


		# this is a hack TODO FIXME
		if (rs@firstvalid == TRUE) {
			#newRow = data.frame(time=dayPeriodStart,sgReading=rawData$sgReading[1],impute='',interp=FALSE)
			rawData$time[1] = dayPeriodStart
			## TODO if firstValid then dayPeriod start is the next whole minute after the first time point
			## TODO interpolate sg so that it is correct
		}
		else {
			newRow = data.frame(time=dayPeriodStart,sgReading=NA,impute='',interp=FALSE)
			#newRow$time = as.POSIXlt(newRow$time)
			rawData = rbind(newRow, rawData)
		}
	}
	# increment dayPeriodStart
        dayPeriodStart = incrementDay(dayPeriodStart)

	## process each timepoint in accelerometer day, interpolating between time points and splitting into day periods

	countDays = 1
	currentMinutes = NULL

	# idx of last timepoint - usually idxThis-1, except where the clocks went back (so we skip the duplicated time points)
	idxPrev = 1

	# process each time point in cgm data in turn
	for (idxThis in 2:nrow(rawData)) {

		timePrev=rawData$time[idxPrev]
		timeThis=rawData$time[idxThis]

                if (timePrev >= timeThis) {
                        # go to next time point until it is after the last time point
                        print('before last time point, probably clocks went back (skipping)...')
                        next
                }

		#for each minute between previous timepoint (inclusive) and current time point (exclusive)
		minutesForBlock = getMinutesForBlock(rawData[idxPrev,], rawData[idxThis,], rs@epochfrequency)

		## set day / night in block
		minutesForBlock = setDayAndNight(minutesForBlock, rs)

		## combine with current minutes
		currentMinutes = rbind(currentMinutes, minutesForBlock)

		## check new day
		while (length(which(currentMinutes$time == dayPeriodStart)) >0) {

			idxThresh = min(which(currentMinutes$time == dayPeriodStart))
 			dayPeriod = currentMinutes[1:idxThresh,]

			# store new day			
			newDay = makeNewDay(dayPeriod, events, bg, countDays)

			# day is null if clocks change and num minutes is not 1441
			if (!is.null(newDay)) {
				participantData@days[[countDays]] = newDay
				countDays = countDays+1
			}

			# set current minutes as the remaining minutes not in the day just created
			currentMinutes = currentMinutes[(idxThresh):nrow(currentMinutes),]

			# increment dayPeriodStart
			dayPeriodStart = incrementDay(dayPeriodStart)

		}
		idxPrev = idxThis
	}

	## if there is remaining data then make a final day
	if (nrow(currentMinutes) > 1) {

		## make block for rest of this last day
		lastTimePoint = data.frame(time=dayPeriodStart,sgReading=NA,impute='', interp=FALSE, deviationLarge=FALSE)

		minutesForBlock = getMinutesForBlock(rawData[nrow(rawData),], lastTimePoint, rs@epochfrequency)

		# add last timepoint too as minutesForBlock is exclusive of end time point
		minutesForBlock = rbind(minutesForBlock, lastTimePoint)

		minutesForBlock = setDayAndNight(minutesForBlock, rs)
		dayPeriod = rbind(currentMinutes, minutesForBlock)

		newDay = makeNewDay(dayPeriod, events, bg, countDays)
		if (!is.null(newDay)) {
			participantData@days[[countDays]] = newDay
		}                
	}

	return(participantData)
}



makeNewDay <-function(dayPeriod, events, bg, dayidx) {

	eventsForDay = events[which(events$time >= dayPeriod$time[1] & events$time < dayPeriod$time[nrow(dayPeriod)]),]
	bgsForDay = bg[which(bg$time >= dayPeriod$time[1] & bg$time < dayPeriod$time[nrow(dayPeriod)]),]


	# make empty data frames if these are null
	if (is.null(eventsForDay)) {
		eventsForDay = data.frame(time=c(), event=c())
	}
	if (is.null(bgsForDay)) {
		bgsForDay = data.frame(time=c(), bgReading=c())
	}
	
	# check if day is valid
	vdCheck = isValidDay(dayPeriod)

	# error when day doesn't have 1441 values (if clocks change)
	newDay = tryCatch({
		newDay = methods::new('day', glucose=dayPeriod, dayidx=dayidx, validday=vdCheck, events=eventsForDay, bg=bgsForDay)
	}, error = function(e) {
		print('Invalid day')
		print(e)
		return(NULL)
	}, finally = {
	})

	if(!is.null(newDay)) {
		print(paste0("valid day: ", vdCheck, ", ", dayidx,": ", dayPeriod$time[1], " - ", dayPeriod$time[nrow(dayPeriod)]))
	}

	return(newDay)

}

# get the first date/time of our data, i.e. the start time of the first day
# if firstvalid is set then use the first sg reading timepoint as the start time
# else use the latest value of the starttime arg, such that it is at the latest on the first valid sg reading time point
# this is so that the first bit of data before the dayPeriodStartTime is included as a day, in case the missing bit can be imputed
getFirstDayPeriodStart <- function(rawData, rs) {

	if (rs@firstvalid == TRUE) {
                # first timepoint is first valid value
                firstTime = rawData$time[1]

		# but rounded to nearest minute
		t2str = paste0(firstTime$mday, "/", firstTime$mon+1, "/" , data.table::year(firstTime), " ", firstTime$hour, ":", firstTime$min)
                dayPeriodStart = strptime(t2str, format='%d/%m/%Y %H:%M')

		# increment minute so it is the first whole minute after the first time point
		if (dayPeriodStart < firstTime) {
			dayPeriodStart = as.POSIXlt(dayPeriodStart + 60)
		}

        }
        else {

                # first timepoint is previous day period start time

		firstTime = rawData$time[1]		
		t2str = paste0(firstTime$mday, "/", (firstTime$mon+1), "/" , data.table::year(firstTime), " ", rs@dayPeriodStartTime$hour, ":", rs@dayPeriodStartTime$min)
        	dayPeriodStart = strptime(t2str, format='%d/%m/%Y %H:%M')

        	if (rawData$time[1]<dayPeriodStart) {
        	        dayPeriodStart = incrementDay(dayPeriodStart, -1)
        	}

       	}

	dayPeriodStart = as.POSIXlt(dayPeriodStart)

	return(dayPeriodStart)

}

setDayAndNight <- function(minutesForBlock, rs) {
	
	minutesForBlock$isnight = FALSE

	dtHourThresh = rs@daystart$hour
	dtMinThresh = rs@daystart$min
	ntHourThresh = rs@nightstart$hour
        ntMinThresh = rs@nightstart$min

	if (ntHourThresh > dtHourThresh | (ntHourThresh == dtHourThresh & ntMinThresh>dtMinThresh)) {

		## nt after dt, so night-time is before day time start and after night time start

		# nighttime: before daytime hour
		ix = which(minutesForBlock$time$hour < dtHourThresh)
		minutesForBlock$isnight[ix] = TRUE

		# nighttime: in daytime hour but before daytime min
	        ix = which(minutesForBlock$time$hour == dtHourThresh & minutesForBlock$time$min < dtMinThresh)
	        minutesForBlock$isnight[ix] = TRUE
	
		# nighttime: after nighttime hour
	        ix = which(minutesForBlock$time$hour >= ntHourThresh)
	        minutesForBlock$isnight[ix] = TRUE

	        # nighttime: in nighttime hour but after nighttime min
	        ix = which(minutesForBlock$time$hour == ntHourThresh & minutesForBlock$time$min >= ntMinThresh)
	        minutesForBlock$isnight[ix] = TRUE
	}
	else {

		## nt before dt, so night-time is after night time start but before day time start

		ix = which(minutesForBlock$time$hour > ntHourThresh & minutesForBlock$time$hour < dtHourThresh)
		minutesForBlock$isnight[ix] = TRUE

		ix = which(minutesForBlock$time$hour == ntHourThresh & minutesForBlock$time$min >= ntMinThresh & minutesForBlock$time$hour < dtHourThresh)
		minutesForBlock$isnight[ix] = TRUE

		ix = which(minutesForBlock$time$hour == ntHourThresh & minutesForBlock$time$min >= ntMinThresh & minutesForBlock$time$hour == dtHourThresh & minutesForBlock$time$min < dtMinThresh)
                minutesForBlock$isnight[ix] = TRUE

	}


	return(minutesForBlock)

}


getMinutesForBlock <- function(blockStart, blockEnd, epochFrequency) {

	timegap = epochFrequency	

	timePrev = blockStart$time
	timeThis = blockEnd$time
	timePrev = as.POSIXlt(timePrev)
	timeThis = as.POSIXlt(timeThis)

	currentMinutes = NULL

        # start current block
        timeMinute = strptime(paste(timePrev$mday, "/", timePrev$mon+1, "/" , data.table::year(timePrev), " ", timePrev$hour, ":", timePrev$min, sep=""), format='%d/%m/%Y %H:%M')

	# make sure next minute epoch is after the previous time point
	timeDiffMins = as.numeric(difftime(timeMinute,timePrev, units="mins"))
        if (timeDiffMins < 0) {
 		timeMinute = as.POSIXlt(timeMinute + 60)
        }

	# see if current epoch is before end of this block
	timeDiffMins = as.numeric(difftime(timeThis,timeMinute, units="mins"))

        # if end time is a round minute it is part of the next block
        while (timeDiffMins > 0) {

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
		timeDiffMins = as.numeric(difftime(timeThis,timeMinute, units="mins"))
	}

	if (!is.null(currentMinutes)) {
		currentMinutes$time = as.POSIXlt(currentMinutes$time)
	}
	else {
              	timeMinute = strptime(paste(timeThis$mday, "/",timeThis$mon+1, "/" , data.table::year(timeThis), " ", timeThis$hour, ":", timeThis$min, sep=""), format='%d/%m/%Y %H:%M')
                currentMinutes = data.frame(time=timeMinute, sgReading=NA, impute='', interp=FALSE, deviationLarge=FALSE)
                currentMinutes$time = as.POSIXlt(currentMinutes$time)
        }

	return(currentMinutes)

}


incrementDay <- function(time, n=1) {

	timeNew = as.POSIXlt(time + n*60*60*24)

	## hour changes if the clocks change
	if (timeNew$hour!=time$hour) {
		# mon+1 because retrieved zero based but set 1-based
		t2str = paste(timeNew$mday, "/", timeNew$mon+1, "/" , data.table::year(timeNew), " ", time$hour, ":", timeNew$min, sep="")
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



isValidDay <- function(dayPeriod) {

	# does the day have NAs
	noNAs = length(which(is.na(dayPeriod$sgReading)))==0 

	return(noNAs)
}



