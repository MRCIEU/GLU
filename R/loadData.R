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



# Loads one person's data from cleaned CGM data file.
# Input data has columns: bgReading, sgReading, meal, Exercise, Medication, time.
# Returns list object containing participant ID and 3 data frames: sg, bg and events.

loadData <- function(fileName, userID, rs) {

	dir = rs@outdir

	rawData <- read.table(paste(dir,'/',fileName,sep=""), sep=",", header=1)

	## REQUIRED COLUMNS
	## check it has required fields
	## and rename them

	colName = "sgReading"
        idxSGReading = which(names(rawData) == colName)
        if (length(idxSGReading)==0) {
                stop(paste("Column missing from input file:", colName),     call.=FALSE)
        }

	colName = "time"
        idxT = which(names(rawData) == colName)
        if (length(idxT)==0) {
                stop(paste("Column missing from input file:", colName),     call.=FALSE)
        }
	rawData$time = strptime(rawData$time, format=rs@timeformat)

	if (length(which(!is.na(rawData$time)))==0) {
		stop(paste("Timestamp column format is not right"))
	}

	## OPTIONAL COLUMNS
	## rename optional columns if they exist


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

	events = events[,c('time', 'event')]

	participantData = list(id=userID, sg=sgReadings, bg=bgReadings, events=events)


	return(participantData)
}


