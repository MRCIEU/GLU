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



# Derives variables that describe characteristics of the SG sequence.
# Returns a data frame containing these characteristics values.
deriveCharacteristics <- function(validDays, userIDdf, hypothreshold, hyperthreshold) {

	print('MAD')
	# Median absolute deviation for each day
	madx = madByDay(validDays)

	# num peaks - this was just to check if the number of undulations decreases as mad increases
#	nps = numPeaksByDay(validDays)

	print('AUC')
        # AUC, on each complete day
        aucValues = aucByDay(validDays)

	print('Time proportions')
        # Proportion of time spent in low, medium and high glucose ranges, on each complete day
        proportions = timeProportionsByDay(validDays, hypothreshold, hyperthreshold)

	print('sGVP')
        # SGVP, on each complete day
        lability = SGVPByDay(validDays)

	print('Fasting proxy')
        # fasting glucose levels
        fastingProxy = fastingProxyByDay(validDays)


	## add number of valid days to derived statistics
        othervars = c(length(validDays))
        othervars = rbind(othervars)
	colnames(othervars) = c("numValidDays")


	print('Events')
        # meal time statistics
        eventValues = eventStatisticsByDay(validDays)

        # whole result row for this participant
#	summThis = cbind.data.frame(userIDdf, madx, aucValues, proportions, lability, fastingProxy, othervars, nps)
	summThis = cbind.data.frame(userIDdf, madx, aucValues, proportions, lability, fastingProxy, othervars)

	if (!is.null(eventValues)) {
		summThis = cbind.data.frame(summThis, eventValues)
	}

	return(summThis)

}
