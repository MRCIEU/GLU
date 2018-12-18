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


library("ggplot2")
#library("stringr")
#library(data.table)


####
#### This is the main script used to run the GLU tool

options(warn=2)

source("addSource.R")
addSource()


# parse arguments - directory is required, if filename is not specified then all files in directory are processed
parseArgs()

# if user specifies a filename then process this, otherwise process all other files in indir
if (!is.null(opt$filename)) {
	files=c(opt$filename)
} else {
	files = list.files(opt$indir, pattern="data_export-.*\\..*")
}

# prefix for derived data file
namePrefix = derivedFilePrefix(opt$filename, opt$impute)

## clear missingness summary
#missingfile=paste(opt$outdir,"missingSummary",namePrefix,".csv", sep="")
#sink(missingfile)
#sink()


first=TRUE

# process CGM file (data for one participant) and add row with derived variables to summaryVariables data frame
for (f in files) {

	print("---------")
	print(paste("File name:", f))

	# get userID
        userID <<- getUserIDFromFileName(f)
        userIDdf = c(userID)
        userIDdf = rbind(userIDdf)
        colnames(userIDdf) = c("userID")

	# load data
	raw <- loadData(opt$indir, f, userID, opt$timeformat)

	if (nrow(raw[["sg"]]) == 0) {
		print("No SG readings")
		next
	}


	######
	###### QC

#	plotCGMTrace(raw[["sg"]], opt$outdir, userID)

	# get a list of days
	alldays = getDays(raw, opt$nightstart, opt$daystart, opt$freq)


#	plotCGM(alldays, opt$outdir, userID)
#	writeMissingSummaryByDay(alldays, opt$impute, userID)

	if (opt$impute == TRUE) {
		alldays = imputeByDay(alldays)
	}

	validDays = getValidDays(alldays)

	# number of valid days is size of list
	numValidDays = c(length(validDays)) # for adding to results data frame

	if (numValidDays == 0) {
		print(paste("No valid days for userID:", userID))
		next
	}

#	plotCGM(alldays, opt$outdir, userID)

	# mark invalid deviations per day
        validDays = markLargeDeviationsByDay(validDays)

	# num invalid deviations per day
        deviationsInvalid = invalidDeviationsByDay(validDays)

	print(paste('Invalid deviations?', deviationsInvalid[1,1]))

	#  save validDays to a CSV files
	saveCleanData(validDays, opt$outdir, userID, opt$impute)


	######
	###### PLOTTING

	plotCGM(validDays, opt$outdir, userID)


	######
	###### GENERATE VARIABLES

	print(paste("Generating variables for file:", f,", with user ID:", userID))
	summThis = deriveCharacteristics(validDays, userIDdf)


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

	namePrefix = ""
	if (!is.null(opt$filename)) {
        	namePrefix = paste('-', userID, sep='')
	}
	if (opt$impute==TRUE) {
		namePrefix = paste(namePrefix, '-imputed', sep='')
	}

	print("Saving results to file ...")

	# overall summaries
	summaryVariablesBrief = summaryVariables[,c("userID", "numValidDays", "meanmadPerDay", "meanProportionLowPerDay", "meanProportionNormalPerDay", "meanProportionHighPerDay", "meanAUCperDay", "meanSGVPPerDay", "meanFastingProxyPerDay", "hasInvalidDeviations")]
	write.table(summaryVariablesBrief, file=paste(opt$outdir, "cgmSummary", namePrefix, ".csv",sep=""), sep=",", quote=FALSE, row.names=FALSE)

	# verbose version with summaries per day
	write.table(summaryVariables, file=paste(opt$outdir, "cgmSummaryVerbose", namePrefix, ".csv",sep=""), sep=",", quote=FALSE, row.names=FALSE)


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

