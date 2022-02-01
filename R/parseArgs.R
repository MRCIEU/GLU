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



#' Process arguments supplied to GLU by the user.
#' @return List of options specified by the user.
#' @export
parseArgs <- function() {

	option_list = list(
  	optparse::make_option(c("-f", "--filename"), type="character", default=NULL, help="Optional filename to process specific file", metavar="character"),
  	optparse::make_option(c("-i", "--indir"), type="character", default=NULL, help="Directory where CGM raw data files are stored", metavar="character"),
  	optparse::make_option(c("-o", "--outdir"), type="character", default=NULL, help="Directory where derived data should be stored", metavar="character"),
	optparse::make_option(c("-n", "--nightstart"), type="character", default=NULL, help="Time of day to night transition, HH:MM", metavar="character"),
	optparse::make_option(c("-d", "--daystart"), type="character", default=NULL, help="Time of night to day transition, HH:MM", metavar="character"),
	optparse::make_option(c("-g", "--dayPeriodStartTime"), type="character", default=NULL, help="Time of day period start, HH:MM", metavar="character"),
	optparse::make_option(c("-c", "--firstvalid"), action="store_true", default=FALSE, help="Day period starts at time of first valid glucose time point [default= %default]"),
#	optparse::make_option(c("-m", "--missingsummary"), action="store_true", default=FALSE, help="Option to generate missing data summary [default= %default]"),
	optparse::make_option(c("-l", "--hypothreshold"), type="character", default=NULL, help="Threshold between hypo- and normo- glycaemia", metavar="number"),
	optparse::make_option(c("-u", "--hyperthreshold"), type="character", default=NULL, help="Threshold between normo- and hyper- glycaemia", metavar="number"),
	optparse::make_option(c("-a", "--impute_approximal"), action="store_true", default=FALSE, help="Perform approximal imputation [default= %default]"),
	optparse::make_option(c("-z", "--impute_other_day"), action="store_true", default=FALSE, help="Perform other day imputation [default= %default]"),
	optparse::make_option(c("-s", "--save"), action="store_true", default=FALSE, help="Save derived CGM sequence(s) [default= %default]"),
	optparse::make_option(c("-x", "--saveevents"), action="store_true", default=FALSE, help="Save derived variables for each event [default= %default]"),
	optparse::make_option(c("-e", "--freq"), type="integer", default=5, help="CGM epoch data frequency (minutes) [default= %default]", metavar="number"),
	optparse::make_option(c("-t", "--timeformat"), type="character", default='%d/%m/%y %H:%M:%S', help="Time format in the CGM data [default= %default]", metavar="character"),
	optparse::make_option(c("-k", "--outlierthreshold"), type="integer", default=5, help="Value k used for outlier detection threshold d=k*SD [default= %default]", metavar="number"),
	optparse::make_option(c("-p", "--pregnancy"), action="store_true", default=FALSE, help="Data is for pregnancy study, so pregnancy specific statistics should be derived [default= %default]"),
	optparse::make_option(c("-q", "--diabetes"), action="store_true", default=FALSE, help="Data is for diabetes study, so diabetes specific statistics should be derived [default= %default]"),
	optparse::make_option(c("-b", "--device"), type="integer", default=0, help="CGM device used. 0: medtronic ipro2, 1: dexcom G2, 2: Abbott freestyle libre, 3: other device (data provided in generic format). [default= %default]")
	)

	opt_parser = optparse::OptionParser(option_list=option_list)
	opt =  optparse::parse_args(opt_parser)

	if (is.null(opt$indir)) {
		optparse::print_help(opt_parser)
	}

	return(opt)
}
