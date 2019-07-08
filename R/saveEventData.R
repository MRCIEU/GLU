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



# Saves data for each event to file.
saveEventData <- function(events, userID, rs) {

	if (rs@saveevents == TRUE & nrow(events)>0) {

		print("Saving events ...")

		namePrefix = userID
		if (rs@imputeApproximal==TRUE) {
	                namePrefix = paste(namePrefix, '-impute-approximal', sep='')
	        }
		else if (rs@imputeOther==TRUE) {
	                namePrefix = paste(namePrefix, '-impute-other', sep='')
	        }

	        # save event data
	        write.table(events, file=paste(rs@outdir, "/events/events-", namePrefix, ".csv",sep=""), sep=",", quote=FALSE, row.names=FALSE, na="")
	
	}

}
