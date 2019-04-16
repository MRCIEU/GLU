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


runGLUForFiles <- function(files, indir, outdir=NULL, device=0, daystart='HH:MM', nightstart='HH:MM', timeformat='%d/%m/%y %H:%M:%S', impute=FALSE, freq=5, outlierthreshold=5, hypothreshold=NULL, hyperthreshold=NULL, save=FALSE, pregnancy=FALSE, diabetes=FALSE) {


vars = validateOptions(files, indir, outdir, device, daystart, nightstart, timeformat, impute, freq, outlierthreshold, hypothreshold, hyperthreshold, save, pregnancy, diabetes)
outdir = vars$outdir
hypothreshold = vars$hypothreshold
hyperthreshold = vars$hyperthreshold
nightstart = vars$nightstart
daystart = vars$daystart


namePrefix = derivedFilePrefix(impute)


# refresh impute logging file
if (impute == TRUE) {
        sink(paste0(outdir, 'logging-impute.txt'))
        sink()
}



first=TRUE

# process CGM file (data for one participant) and add row with derived variables to summaryVariables data frame
for (f in files) {

	print("---------")
	print(paste("File name:", f))

	# get userID
        userID <<- getUserIDFromFileName(f)
        userIDdf = c(userID)
        userIDdf = rbind(userIDdf)
        colnames(userIDdf) = c("ID")


	######
	###### convert file format depending on device type

	convertFileFormat(indir, f, outdir, device)


	#####
	##### load data

	raw <- loadData(outdir, f, userID, timeformat)

	if (nrow(raw[["sg"]]) == 0) {
		print("No SG readings")
		next
	}


	######
	###### QC

#	plotCGMTrace(raw[["sg"]], outdir, userID)

	# get a list of days
	alldays = getDays(raw, nightstart, daystart, freq)


#	plotCGM(alldays, outdir, userID)
#	writeMissingSummaryByDay(alldays, impute, userID, filename, outdir)

	if (impute == TRUE) {
		print('imputing missing timepoints ...')
		
		sink(paste0(outdir, 'logging-impute.txt'), append=TRUE)
		print(paste0('*********** IMPUTING ', userID, ' ***********'))
		alldays = imputeByDay(alldays)
		sink()
	}

	validDays = getValidDays(alldays)

	# number of valid days is size of list
	numValidDays = c(length(validDays)) # for adding to results data frame

	if (numValidDays == 0) {
		print(paste("No valid days for userID:", userID))
		next
	}
	
	print(paste0("Number of valid days: ", numValidDays))

#	plotCGM(alldays, outdir, userID)

	# mark invalid deviations per day
        validDays = markLargeDeviationsByDay(validDays, outlierthreshold)

	# num invalid deviations per day
        deviationsInvalid = invalidDeviationsByDay(validDays)

	print(paste('Invalid deviations?', deviationsInvalid[1,1]))

	#  save validDays to a CSV files
	saveCleanData(validDays, outdir, userID, impute, save)


	######
	###### PLOTTING

	print(hypothreshold)
	plotCGM(validDays, outdir, userID, hypothreshold, hyperthreshold)


	######
	###### GENERATE VARIABLES

	print(paste("Generating variables for file:", f,", with user ID:", userID))
	summThis = deriveCharacteristics(validDays, userIDdf, hypothreshold, hyperthreshold)


	# add correlation and invalid deviations
	summThis = cbind.data.frame(summThis, deviationsInvalid)

	# add to summaryVar data frame, or create if first person
        if (first==TRUE) {
                summaryVariables = summThis
                first=FALSE
        }
	else {
              	# we use merge rather than rbind because users have different columns depending on how many valid days they have
                summaryVariables = merge(summaryVariables,summThis, all=TRUE)
        }

	print("---------")
}


# if there are no files then we have no derived characteristics to save
if (exists("summaryVariables")==TRUE) {

	print("Saving results to file ...")

	# overall summaries
	summaryVariablesBrief = summaryVariables[,c("ID", "numValidDays", "meanmadPerDay", "meanProportionLowPerDay", "meanProportionNormalPerDay", "meanProportionHighPerDay", "meanAUCperDay", "meanSGVPPerDay", "meanFastingProxyPerDay", "hasInvalidDeviations")]
	write.table(summaryVariablesBrief, file=paste(outdir, "cgmSummary", namePrefix, ".csv",sep=""), sep=",", quote=FALSE, row.names=FALSE)

	# verbose version with summaries per day
	write.table(summaryVariables, file=paste(outdir, "cgmSummaryVerbose", namePrefix, ".csv",sep=""), sep=",", quote=FALSE, row.names=FALSE)


} else {
	print("No results were generated")
}

# warnings
wx = warnings()
if (!is.null(wx)) {
	print('Warnings:')
	print(wx)
}

print('GLU has finished.')

}
