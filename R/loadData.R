# The MIT License (MIT)
# Copyright (c) 2017 Louise AC Millard, MRC Integrative Epidemiology Unit, University of Bristol
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



# Loads one person's data from file.
# Returns CGM data with the columns: bgReading, sgReading, meal, Exercise, Medication, isigReading, usedInCal, date, time.
loadData <- function(dir, fileName, userID, timeFormat) {

	rawData <- read.table(paste(dir,fileName,sep=""), sep="\t", header=1)

	## REQUIRED COLUMNS
	## check it has required fields
	## and rename them

	colName = "Sensor.Glucose..mmol.L."
        idxSGReading = which(names(rawData) == colName)
        if (length(idxSGReading)==0) {
                stop(paste("Column missing from input file:", colName),     call.=FALSE)
        }
	names(rawData)[idxSGReading]="sgReading"

	colName = "Timestamp"
        idxT = which(names(rawData) == colName)
        if (length(idxT)==0) {
                stop(paste("Column missing from input file:", colName),     call.=FALSE)
        }
        names(rawData)[idxT]="time"
#        rawData$time = strptime(rawData$time, format='%d/%m/%y %H:%M:%S')
	rawData$time = strptime(rawData$time, format=timeFormat)

	if (length(which(!is.na(rawData$time)))==0) {
		stop(paste("Timestamp column format is not right"))
	}

	## OPTIONAL COLUMNS
	## rename optional columns if they exist

	colName = "Excluded"
	idxExcluded = which(names(rawData) == colName)
	if (length(idxExcluded)>0) {

		# if excluded column exists then remove all excluded rows
		ix = which(rawData[,idxExcluded]!="TRUE")
		rawData = rawData[ix,]
        }

	colName = "BG.Reading..mmol.L."
	idxBGReading = which(names(rawData) == colName) 
	if (length(idxBGReading)>0) {
        names(rawData)[idxBGReading]="bgReading"
	}

	colName = "Used.in.Calibration"
        idxCal = which(names(rawData) == colName)
        if (length(idxCal)>0) {
		names(rawData)[idxCal]="usedInCal"

		# convert usedInCal to binary
	        rawData$usedInCalBinary = FALSE
	        rawData$usedInCalBinary[which(rawData$usedInCal=="True")] = TRUE
	        rawData$usedInCal = rawData$usedInCalBinary
	        rawData = rawData[, !(names(rawData) == "usedInCalBinary")]

	}

	colName = "Meal"
        idxT = which(names(rawData) == colName)
        if (length(idxT)>0) {
		names(rawData)[idxT]="meal"
	} else {

		colName = "Meal.Size"
		idxT = which(names(rawData) == colName)
	        if (length(idxT)>0) {
			names(rawData)[idxT]="meal"
        	}
	}
	
	colName = "Exercise"
        idxT = which(names(rawData) == colName)
        if (length(idxT)>0) {
		names(rawData)[idxT]="Exercise"
	} else {
                colName = "Exercise.Level"
                idxT = which(names(rawData) == colName)
                if (length(idxT)>0) {
			names(rawData)[idxT]="Exercise"
                }
        }


	## CREATE OUR DATA OBJECT
	## seperate data into types
	
	sgReadings = rawData[which(!is.na(rawData$sgReading)),c("time", "sgReading")]
	if (nrow(sgReadings)>0) {
		sgReadings$impute = ""

	}

	bgReadings=NULL
	if (length(which(names(rawData) == 'bgReading')>0)) {
		bgReadings = rawData[which(!is.na(rawData$bgReading)),c("time", "bgReading")]
	}

	events = NULL
	if (length(which(names(rawData) == 'meal')>0)) {
		eventsMeal = rawData[which(!is.na(rawData$meal) & rawData$meal!=""),]

		if (nrow(eventsMeal)>0) {

			# make sure all events are rounded to the nearest minute   
			eventsMeal$time = trunc(eventsMeal$time, 'secs')
			eventsMeal$event = "MEAL"
			events = eventsMeal
		}
	}
	if (length(which(names(rawData) == 'Exercise')>0)) {
		eventsEx = rawData[which(!is.na(rawData$Exercise) & rawData$Exercise!=""),]
		
		if (nrow(eventsEx)>0) {

			# make sure all	events are rounded to the nearest minute         
			eventsEx$time = trunc(eventsEx$time, 'secs')

	        	eventsEx$event = "EXERCISE"
			if (is.null(events)) {
		         	events = eventsEx
			}	
			else {
			events = rbind(events, eventsEx)
			}
		}
        }
	if (length(which(names(rawData) == 'Medication')>0)) {
		eventsMed = rawData[which(!is.na(rawData$Medication) & rawData$Medication!=""),]

		if (nrow(eventsMed)>0) {

			# make sure all events are rounded to the nearest minute   
			eventsMed$time = trunc(eventsMed$time, 'secs')

                	eventsMed$event = "MEDICATION"
                	if (is.null(events)) {
                	        events = eventsMed
                	}
                	else {
                	        events = rbind(events, eventsMed)
                	}
		}
        }

	participantData = list(id=userID, sg=sgReadings, bg=bgReadings, events=events)


	return(participantData)
}


