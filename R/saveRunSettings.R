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



# save arguments supplied to GLU by the user, for reporting in publications
saveRunSettings <- function(runSettings) {

	sink(paste0(runSettings@outdir, '/run-settings.txt'))

	print(paste0('indir: ', runSettings@indir))
	print(paste0('outdir: ', runSettings@outdir))
	print(paste0('device: ', runSettings@device))
	print(paste0('daystart: ', runSettings@daystart))
	print(paste0('nightstart: ', runSettings@nightstart))
	print(paste0('timeformat: ', runSettings@timeformat))
	print(paste0('imputeApproximal: ', runSettings@imputeApproximal))
	print(paste0('imputeOther: ', runSettings@imputeOther))
	print(paste0('freq: ', runSettings@freq))
	print(paste0('outlierthreshold: ', runSettings@outlierthreshold))
	print(paste0('hypothreshold: ', runSettings@hypothreshold))
	print(paste0('hyperthreshold: ', runSettings@hyperthreshold))
	print(paste0('save: ', runSettings@save))
	print(paste0('pregnancy: ', runSettings@pregnancy))
	print(paste0('diabetes: ', runSettings@diabetes))

	sink()

}
