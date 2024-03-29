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


# preprocessing for three specific CGM data formats from: Medtronic ipro2, Dexcom G2, and Abbott Freestyle Libre
convertFileFormat <- function(filename, rs) {

	print("converting file format ...")

	inFile = paste0(rs@indir, '/', filename)
	outFile = paste0(rs@outdir, '/', filename)

	if (rs@device == 0) {
		convertFileMedtronic(inFile, outFile, filename)
	   }
	else if (rs@device == 1) {
		convertFileDexcom(inFile, outFile, filename)
	   }
	else if (rs@device == 2) {
		convertFileAbbotFreestyleLibre(inFile, outFile, filename)
	   }
	else if (rs@device == 3) {
		## already in correct format
		file.copy(inFile,outFile)
	   }


}

convertFileMedtronic <- function(inFile, outFile, filename) {


	# read lines using correct encoding
	lines = readLines(inFile, encoding="utf-16", skipNul = TRUE)

	# remove header lines and quotes
	lines = lines[12:length(lines)]
	lines = gsub("\"", "", lines)

	## convert lines to data frame
	data = read.table(text = lines, sep='\t', quote="", header=1)

	## convert to consistent format
	# time, sgReading, meal, exercise, medication, bgReading

	## timestamp - date and time
	data = checkAndRename('Timestamp', '', 'time', data)

	## Sensor glucose

	# glucose column name might contain mmol/L or mg/dL so we get it this way
        sensGlucColName = colnames(data)[which(grepl('^Sensor.Glucose', colnames(data)))]

	data = checkAndRename(sensGlucColName, '', 'sgReading', data)

	## blood glucose reading (from finger prick) needed to calibrate this device

	# glucose column name might contain mmol/L or mg/dL so we get it this way
        bloodGlucColName = colnames(data)[which(grepl('^BG.Reading', colnames(data)))]

	data = checkAndRename(bloodGlucColName, '', 'bgReading', data)

	## excluded rows
	colName = "Excluded"
	idxExcluded = which(names(data) == colName)
	if (length(idxExcluded)>0) {

		# if excluded column exists then remove all excluded rows
		ix = which(data[,idxExcluded]!="TRUE")
		data = data[ix,]
	}

	####
	#### Optional columns (might not have any data)

	colName = 'Meal'
	ixmeal = which(colnames(data) == colName)
	if (length(ixmeal)==1) {
		colnames(data)[ixmeal] = 'meal'
		#stop(paste("Column missing from input file:", colName),	call.=FALSE)
	} else {
		colName = "Meal.Size"
		ixmeal = which(names(data) == colName)
		if (length(ixmeal)>0) {
			names(data)[ixmeal]="meal"
		}
		else {
			print("No meal column found in Medtronic data")
		}
	}

	colName = 'Exercise'
	ixex = which(colnames(data) == colName)
	if (length(ixex)==1) {
		colnames(data)[ixex] = 'exercise'
	} else {
		colName = "Exercise.Level"
		ixex = which(names(data) == colName)
		if (length(ixex)>0) {
			names(data)[ixex]="exercise"
		}
		else {
		 	print("No exercise column found in Medtronic data")
		}
	}

	## medication
	data = checkAndRename('Medication', '', 'medication', data)


	####
	#### extract columns we need

	cols = c('time', 'sgReading', 'bgReading', 'exercise', 'medication', 'meal')
	data = data[,cols]
	write.table(data, outFile, row.names=FALSE, sep=',', quote=FALSE)

}

convertFileDexcom <- function(inFile, outFile, filename) {

	# read lines using correct encoding
	lines = readLines(inFile, encoding="utf-16", skipNul = TRUE)


	## convert lines to data frame
	data = read.table(text = lines, sep=',', quote="", header=1)

	# glucose column name might contain mmol/L or mg/dL so we get it this way
	glucColName = colnames(data)[which(grepl('^Glucose.Value', colnames(data)))]

	# convert CGM values to numeric, needed because sometimes it contains text values e.g. 'Low'
	# Note this can produce a 'NAs introduced by coercion' warning message but that's OK, the non-numeric text values are converted to NAs
	data[,glucColName] = as.numeric(as.character(data[,glucColName]))

	## remove rows with no timestamp
	ix = which(is.na(data$Timestamp..YYYY.MM.DDThh.mm.ss.) | data$Timestamp..YYYY.MM.DDThh.mm.ss.=="" | is.na(data[,glucColName]))
	if (length(ix)>0) {
		data = data[-ix,]
	}
	
	## timestamp - date and time
	data = checkAndRename('Timestamp..YYYY.MM.DDThh.mm.ss.', '', 'time', data)

	## change time to consistent format
	timeFormat = '%Y-%m-%dT%H:%M:%S'
	data$time = strptime(data$time, format=timeFormat)
	data$time = format(data$time, '%d/%m/%y %H:%M:%S')

	## Sensor glucose
	data = checkAndRename(glucColName, '', 'sgReading', data)


	####
	#### extract columns we need

	cols = c('time', 'sgReading')
	data = data[,cols]

	write.table(data, outFile, row.names=FALSE, sep=',', quote=FALSE)

}


convertFileAbbotFreestyleLibre <- function(inFile, outFile, filename) {

	# read lines using correct encoding
	lines = readLines(inFile, encoding="utf-16", skipNul = TRUE)

	# remove header lines and quotes
	lines = lines[3:length(lines)]
	lines = gsub("\"", "", lines)

	## convert lines to data frame
	data = read.table(text = lines, sep='\t', quote="", header=1)

	# keep only zero record type - the SG values every 15 minutes and event rows (i.e. exclude the scan glucose values)
	ix = which(data$Record.Type != 1)
	data = data[ix,]

	## timestamp - date and time

	data = checkAndRename('Time', 'Device.Timestamp', 'time', data)

	## change time to consistent format
	timeFormat = '%Y/%m/%d %H:%M'
	data$time = strptime(data$time, format=timeFormat)
	data$time = format(data$time, '%d/%m/%y %H:%M:%S')
	
	## Sensor glucose
        
	# glucose column name might contain mmol/L or mg/dL so we get it this way
        sensGlucColName = colnames(data)[which(grepl('^Historic.Glucose', colnames(data)))]
	data = checkAndRename(sensGlucColName, '', 'sgReading', data)

	## blood glucose reading (from finger prick)

	# glucose column name might contain mmol/L or mg/dL so we get it this way
        bloodGlucColName = colnames(data)[which(grepl('^Strip.Glucose', colnames(data)))]
	data = checkAndRename(bloodGlucColName, '', 'bgReading', data)

	## meals
	data = checkAndRename('Non.numeric.Food','', 'meal', data)


	####
	#### extract columns we need

	cols = c('time', 'sgReading', 'bgReading', 'meal')
	data = data[,cols]
	write.table(data, outFile, row.names=FALSE, sep=',', quote=FALSE)

}


# checks column exists and renames it
checkAndRename <- function(oldname, oldname2, newname, data) {
	
	ixT = which(colnames(data) == oldname)
	if (length(ixT)==0) {

		ixT2 = which(colnames(data) == oldname2)

		if (length(ixT2)==0) {
			stop(paste("Column missing from input file:", oldname, oldname2), call.=FALSE)
		}

		colnames(data)[ixT2] = newname
	}
	else {
		colnames(data)[ixT] = newname
	}
	
	return(data)
}
