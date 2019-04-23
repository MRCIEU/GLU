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


#' Main function to run GLU from the command line
#'
#' @param opt List containing command line arguments.
#' @export
runGLU <- function(opt) {

# if user specifies a filename then process this, otherwise process all other files in indir
if (!is.null(opt$filename)) {

	fullfilename=paste0(opt$indir, opt$filename)

	if (!file.exists(fullfilename)) {
		stop(paste0('File ', fullfilename, ' does not exist'))
	}
	files=c(opt$filename)

} else {
	files = list.files(opt$indir, pattern=".*\\..*", full.names=FALSE)
}

runGLUForFiles(files=files, indir=opt$indir, outdir=opt$outdir, device=opt$device, daystart=opt$daystart, nightstart=opt$nightstart, timeformat=opt$timeformat, impute=opt$impute, freq=opt$freq, outlierthreshold=opt$outlierthreshold, hypothreshold=opt$hypothreshold, hyperthreshold=opt$hyperthreshold, save=opt$save, pregnancy=opt$pregnancy, diabetes=opt$diabetes)

}

