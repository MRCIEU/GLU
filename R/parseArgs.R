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



# process arguments supplied to GLU by the user.
parseArgs <- function() {

	#install.packages("optparse");
	library("optparse")

	option_list = list(
  	make_option(c("-f", "--filename"), type="character", default=NULL, help="Optional filename to process specific file", metavar="character"),
  	make_option(c("-i", "--indir"), type="character", default=NULL, help="Directory where CGM raw data files are stored", metavar="character"),
  	make_option(c("-o", "--outdir"), type="character", default=NULL, help="Directory where derived data should be stored", metavar="character"),
	make_option(c("-n", "--nightstart"), type="character", default=NULL, help="Time of day to night transition, HH:MM", metavar="character"),
	make_option(c("-d", "--daystart"), type="character", default=NULL, help="Time of night to day transition, HH:MM", metavar="character"),
#	make_option(c("-m", "--missingsummary"), action="store_true", default=FALSE, help="Option to generate missing data summary [default= %default]"),
	make_option(c("-l", "--hypothreshold"), type="character", default=NULL, help="Threshold between hypo- and normo- glycaemia", metavar="number"),
	make_option(c("-u", "--hyperthreshold"), type="character", default=NULL, help="Threshold between normo- and hyper- glycaemia", metavar="number"),
	make_option(c("-a", "--impute"), action="store_true", default=FALSE, help="Perform matched imputation [default= %default]"),
	make_option(c("-s", "--save"), action="store_true", default=FALSE, help="Save derived CGM sequence(s) [default= %default]"),
	make_option(c("-e", "--freq"), type="integer", default=5, help="CGM epoch data frequency (minutes) [default= %default]", metavar="number"),
	make_option(c("-t", "--timeformat"), type="character", default='%d/%m/%y %H:%M:%S', help="Time format in the CGM data [default= %default]", metavar="character"),
	make_option(c("-k", "--outlierthreshold"), type="integer", default=5, help="Value k used for outlier detection threshold d=k*SD [default= %default]", metavar="number"),
	make_option(c("-p", "--pregnancy"), action="store_true", default=FALSE, help="Data is for pregnancy study, so pregnancy specific statistics should be derived [default= %default]"),
  	make_option(c("-q", "--diabetes"), action="store_true", default=FALSE, help="Data is for diabetes study, so diabetes specific statistics should be derived [default= %default]")
	)


	opt_parser = OptionParser(option_list=option_list)
	opt<<- parse_args(opt_parser)


        if (is.null(opt$indir)){
                print_help(opt_parser)
                stop("indirectory argument must be supplied", call.=FALSE)
        }
	else if (!file.exists(opt$indir)) {
                stop(paste("Input data directory indirectory=", opt$indir, " does not exist", sep=""), call.=FALSE)
        }

	if (is.null(opt$outdir)){
                opt$outdir <<- opt$indir
        }
	else if (!file.exists(opt$outdir)) {
                stop(paste("Output data directory outdir=", opt$outdir, " does not exist", sep=""), call.=FALSE)
        }

	validDaysDir = file.path(opt$outdir, "validdays/")
        if (!file.exists(validDaysDir)) {

                # make valid days subdirectory if it doesn't exist

                dir.create(validDaysDir)
        }

	if (!is.null(opt$filename)){
                if (!file.exists(paste(opt$indir, opt$filename, sep=""))) {
                        stop(paste("Input file filename=", opt$indirectory, " in directory ", opt$indir, "does not exist", sep=""), call.=FALSE)
                }
        }

	if (opt$pregnancy == TRUE) {
                print("Generating statistics for pregnancy study")
        }

#	if (opt$missingsummary == TRUE)	{
#		print("Generating missing data summary")
#		write("cgmID, validDay, length, daystart, dayend, numSG, numISIG, eventSummary", file=paste(opt$outdir,"missingSummary.csv", sep=""), append=FALSE)
#	}



	if (is.null(opt$daystart)){
		opt$daystart <<- '06:30'
		opt$daystart <<- strptime(opt$daystart, format='%H:%M')
        }
	else {

		###############################
		# TBC validate time

		opt$daystart <<- strptime(opt$daystart, format='%H:%M')
        }


	if (is.null(opt$nightstart)){
		opt$nightstart <<- '23:00' # 23:00
                opt$nightstart <<- strptime(opt$nightstart, format='%H:%M')
		print(opt$nightstart)

        }
	else {
		opt$nightstart <<- strptime(opt$nightstart, format='%H:%M')
              	# TBC validate time
        }

	print(paste("Night start:", opt$nightstart))
        print(paste("Day start:", opt$daystart))



	# both thresholds need to be set or neither
	if ((is.null(opt$hypothreshold) & !is.null(opt$hyperthreshold)) | (is.null(opt$hyperthreshold) & !is.null(opt$hypothreshold))) {
		stop("Both hypo and hyper thresholds need to be set (or neither)", call.=FALSE)
	}
	else if (is.null(opt$hypothreshold) & is.null(opt$hyperthreshold)) {

		# default settings for low and high thresholds

		if (opt$pregnancy == TRUE) {
			opt$hypothreshold <<- 3.9
			opt$hyperthreshold <<- 7.8
			print(paste("Using pregnancy hypo-glycaemia threshold: ", opt$hypothreshold, sep=""))
	                print(paste("Using pregnancy hyper-glycaemia threshold: ", opt$hyperthreshold, sep=""))
		}
		else if (opt$diabetes == TRUE) {
			opt$hypothreshold <<- 3.9
                        opt$hyperthreshold <<- 10.0
			print(paste("Using diabetes hypo-glycaemia threshold: ", opt$hypothreshold, sep=""))
                        print(paste("Using diabetes hyper-glycaemia threshold: ", opt$hyperthreshold, sep=""))
		}
		else {
			opt$hypothreshold <<- 3.3
                        opt$hyperthreshold <<- 10.0
			print(paste("Using default hypo-glycaemia threshold: ", opt$hypothreshold, sep=""))
                        print(paste("Using default hyper-glycaemia threshold: ", opt$hyperthreshold, sep=""))
		}
	}
	else {
		print(paste("Using hypo-glycaemia threshold: ", opt$hypothreshold, sep=""))
		print(paste("Using hyper-glycaemia threshold: ", opt$hyperthreshold, sep=""))

	}

}
