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



# process arguments supplied to GLU by the user
validateOptions <- function(files, indir, outdir=NULL, device=0, daystart='HH:MM', nightstart='HH:MM', timeformat='%d/%m/%y %H:%M:%S', impute=FALSE, freq=5, outlierthreshold=5, hypothreshold=NULL, hyperthreshold=NULL, save=FALSE, pregnancy=FALSE, diabetes=FALSE) {


print(hypothreshold)

	if (device<0 | device>3) {
		stop("Device index must be 0 (medtronic ipro2), 1 (Dexcom g2), 2: (Abbott freestyle libre), or 3 (other device, generic format).")
	}

	if (device == 0) {
		print("Processing data from Medtronic iPro2 device.")
	}
	else if (device == 1) {
		print("Processing data from Dexcom g2 device.")
	}
	else if	(device == 2) {
                print("Processing data from Abbott freestyle libre device.")
        }
	else if (device == 3) {
                print("Other device. Checking data in generic format.")
        }


        if (is.null(indir)){
                print_help(opt_parser)
                stop("indirectory argument must be supplied", call.=FALSE)
        }
	else if (!file.exists(indir)) {
                stop(paste("Input data directory indirectory=", indir, " does not exist", sep=""), call.=FALSE)
        }

	if (is.null(outdir)){
                outdir = indir
        }
	else if (!file.exists(outdir)) {
                stop(paste("Output data directory outdir=", outdir, " does not exist", sep=""), call.=FALSE)
        }

	validDaysDir = file.path(outdir, "validdays/")
        if (save ==TRUE & !file.exists(validDaysDir)) {

                # make valid days subdirectory if it doesn't exist

                dir.create(validDaysDir)
        }

#	if (!is.null(filename)){
#                if (!file.exists(paste(indir, filename, sep=""))) {
#                        stop(paste("Input file filename=", indirectory, " in directory ", indir, "does not exist", sep=""), call.=FALSE)
#                }
#        }

	if (pregnancy == TRUE) {
                print("Generating statistics for pregnancy study")
        }

#	if (missingsummary == TRUE)	{
#		print("Generating missing data summary")
#		write("cgmID, validDay, length, daystart, dayend, numSG, numISIG, eventSummary", file=paste(outdir,"missingSummary.csv", sep=""), append=FALSE)
#	}



	if (is.null(daystart)){
		daystart = '06:30'
		daystart = strptime(daystart, format='%H:%M')
        }
	else {

		###############################
		# TBC validate time

		daystart <<- strptime(daystart, format='%H:%M')
        }


	if (is.null(nightstart)){
		nightstart = '23:00' # 23:00
                nightstart = strptime(nightstart, format='%H:%M')
        }
	else {
		nightstart = strptime(nightstart, format='%H:%M')
              	# TBC validate time
        }

	print(paste0("Night start: ", format(nightstart, '%H:%M')))
	print(paste0("Day start: ", format(daystart, '%H:%M')))



	# both thresholds need to be set or neither
	if ((is.null(hypothreshold) & !is.null(hyperthreshold)) | (is.null(hyperthreshold) & !is.null(hypothreshold))) {
		stop("Both hypo and hyper thresholds need to be set (or neither)", call.=FALSE)
	}
	else if (is.null(hypothreshold) & is.null(hyperthreshold)) {

		# default settings for low and high thresholds

		if (pregnancy == TRUE) {
			hypothreshold = 3.9
			hyperthreshold = 7.8
			print(paste("Using pregnancy hypo-glycaemia threshold: ", hypothreshold, sep=""))
	                print(paste("Using pregnancy hyper-glycaemia threshold: ", hyperthreshold, sep=""))
		}
		else if (diabetes == TRUE) {
			hypothreshold = 3.9
                        hyperthreshold = 10.0
			print(paste("Using diabetes hypo-glycaemia threshold: ", hypothreshold, sep=""))
                        print(paste("Using diabetes hyper-glycaemia threshold: ", hyperthreshold, sep=""))
		}
		else {
			hypothreshold = 3.3
                        hyperthreshold = 10.0
			print(paste("Using default hypo-glycaemia threshold: ", hypothreshold, sep=""))
                        print(paste("Using default hyper-glycaemia threshold: ", hyperthreshold, sep=""))
		}
	}
	else {
		print(paste("Using hypo-glycaemia threshold: ", hypothreshold, sep=""))
		print(paste("Using hyper-glycaemia threshold: ", hyperthreshold, sep=""))

	}

	return(list(outdir=outdir, hypothreshold=hypothreshold, hyperthreshold=hyperthreshold, nightstart=nightstart, daystart=daystart))

}
