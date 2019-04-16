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



addSource <- function() {

        source("parseArgs.R")
	source("validateOptions.R")
	source("convertFileFormat.R")
	source("runGLUForDirectory.R")
	source("runGLUForFiles.R")

	source("derivedFilePrefix.R")
	source("loadData.R")
	source("getUserIDFromFileName.R")
	source("getDays.R")
	source("getValidDays.R")

	source("impute.R")
	source("imputeByDay.R")

	# QC
	source("markLargeDeviations.R")
	source("markLargeDeviationsByDay.R")
	source("invalidDeviationsByDay.R")
	source("saveCleanData.R")
#	source("missingSummary.R")

	# plotting
	source("plotCGM.R")

	# deriving characteristics
	source("deriveCharacteristics.R")
	source("meanAcrossDays.R")
	source("auc.R")
	source("aucByDay.R")
	source("timeProportions.R")
	source("timeProportionsByDay.R")
	source("madByDay.R")
	source("SGVP.R")
	source("SGVPByDay.R")
	source("fastingProxy.R")
	source("fastingProxyByDay.R")
	source("mealtimeStatistics.R")
	source("exerciseStatistics.R")
	source("medicationStatistics.R")
	source("eventStatisticsByDay.R")
	source("postprandial.R")
	source("timeToPeak.R")
	source("collateSequence.R")
	source("numPeaks.R")
	source("numPeaksByDay.R")
}
