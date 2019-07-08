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


#' Run GLU for given set of files
#'
#' @param files CGM data files for which variables will be derived.
#' @param indir Path of input directory containing input files.
#' @param outdir Path of output directory where derived data will be stored.
#' @param device Device type: 0 (medtronic ipro2); 1 (Dexcom G2); 2 (Abbott Freestyle Libre), default 0.
#' @param daystart Time of night to day transition in format HH:MM, default 06:30.
#' @param nightstart Time of day to night transition in format HH:MM, default 23:00.
#' @param dayPeriodStartTime Start time of day period, default is nightstart.
#' @param timeformat Time format in the CGM data, default \%d/\%m/\%y \%H:\%M:\%S.
#' @param imputeApproximal Logical. If TRUE then the 'approximal imputation' approach to dealing with missing data is used. If both imputeApproximal and imputeOther are FALSE then the 'complete days' approach is used.
#' @param imputeOther Logical. If TRUE then the 'other day imputation' approach to dealing with missing data is used. If both imputeApproximal and imputeOther are FALSE then the 'complete days' approach is used.
#' @param freq Integer. CGM epoch data frequency (minutes).
#' @param outlierthreshold Numeric. The value k used for outlier detection threshold d=k*SD.
#' @param hypothreshold Numeric. Threshold between hypo- and normo- glycaemia.
#' @param hyperthreshold Numeric. Threshold between normo- and hyper- glycaemia.
#' @param save Logical. If TRUE then derived CGM sequence(s) are stored.
#' @param pregnancy Logical. If TRUE then data is for pregnancy study, so pregnancy specific statistics should be derived.
#' @param diabetes Logical. If TRUE then data is for diabetes study, so pregnancy specific statistics should be derived.
#' @export
runGLUForFiles <- function(files, indir, outdir=NULL, device=0, daystart='06:30', nightstart='23:00', dayPeriodStartTime=NULL, firstvalid=FALSE, timeformat='%d/%m/%y %H:%M:%S', imputeApproximal=FALSE, imputeOther=FALSE, freq=5, outlierthreshold=5, hypothreshold=NULL, hyperthreshold=NULL, save=FALSE, saveevents=FALSE, pregnancy=FALSE, diabetes=FALSE, epochfrequency=5) {


print('Running GLU...')
print(paste0('GLU package version: ', packageVersion("GLU")))



# run settings
rs = validateOptions(indir, outdir, device, daystart, nightstart, dayPeriodStartTime, firstvalid, timeformat, imputeApproximal, imputeOther, freq, outlierthreshold, hypothreshold, hyperthreshold, save, saveevents, pregnancy, diabetes)


# Save run settings for reporting in publications
saveRunSettings(rs)

namePrefix = derivedFilePrefix(rs)


# refresh impute logging file
if (rs@imputeApproximal == TRUE | rs@imputeOther == TRUE) {
        sink(paste0(outdir, '/logging-impute.txt'))
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

	convertFileFormat(f, rs)

	#####
	##### load data

	raw <- loadData(f, userID, rs)

	if (nrow(raw[["sg"]]) == 0) {
		print("No SG readings")
		next
	}


	######
	###### QC

	# get a list of days
	participantData = getDays(raw, rs)

	if (rs@imputeApproximal == TRUE | rs@imputeOther == TRUE) {
		print('imputing missing timepoints ...')
		
		sink(paste0(outdir, '/logging-impute.txt'), append=TRUE)
		print(paste0('*********** IMPUTING ', userID, ' ***********'))
		participantData@days = imputeByDay(participantData@days, rs)
		sink()
	}

	validDays = getValidDays(participantData)

	# number of valid days is size of list
	numValidDays = c(length(validDays)) # for adding to results data frame

	if (numValidDays == 0) {
		print(paste("No valid days for userID:", userID))
		next
	}
	
	print(paste0("Number of valid days: ", numValidDays))

	# mark invalid deviations per day
        validDays = markLargeDeviationsByDay(validDays, rs@outlierthreshold)

	# num invalid deviations per day
        deviationsInvalid = invalidDeviationsByDay(validDays)

	print(paste('Invalid deviations?', deviationsInvalid[1,1]))

	#  save validDays to a CSV files
	saveCleanData(validDays, userID, rs)


	######
	###### PLOTTING

	plotCGM(validDays, outdir, userID, rs@hypothreshold, rs@hyperthreshold)


	######
	###### GENERATE VARIABLES

	print(paste("Generating variables for file:", f,", with user ID:", userID))
	summThis = deriveCharacteristics(validDays, userIDdf, rs)


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
	write.table(summaryVariablesBrief, file=paste(outdir, "/", "cgmSummary", namePrefix, ".csv",sep=""), sep=",", quote=FALSE, row.names=FALSE)

	# verbose version with summaries per day
	write.table(summaryVariables, file=paste(outdir, "/", "cgmSummaryVerbose", namePrefix, ".csv",sep=""), sep=",", quote=FALSE, row.names=FALSE)


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
