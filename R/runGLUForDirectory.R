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

#' Run GLU for all files in a directory
#'
#' @param indir Path of input directory containing input files.
#' @param outdir Path of output directory where derived data will be stored.
#' @param device Device type: 0 (medtronic ipro2); 1 (Dexcom G2); 2 (Abbott Freestyle Libre), default 0.
#' @param daystart Time of night to day transition in format HH:MM, default 06:30.
#' @param nightstart Time of day to night transition in format HH:MM, default 23:00.
#' @param timeformat Time format in the CGM data, default %d/%m/%y %H:%M:%S.
#' @param impute Logical. If TRUE then the 'approximal imputation' approach to dealing with missing data is used. If FALSE then the 'complete days' approach is used. 
#' @param freq Integer. CGM epoch data frequency (minutes).
#' @param outlierthreshold Numeric. The value k used for outlier detection threshold d=k*SD.
#' @param hypothreshold Numeric. Threshold between hypo- and normo- glycaemia.
#' @param hyperthreshold Numeric. Threshold between normo- and hyper- glycaemia.
#' @param save Logical. If TRUE then derived CGM sequence(s) are stored.
#' @param pregnancy Logical. If TRUE then data is for pregnancy study, so pregnancy specific statistics should be derived.
#' @param diabetes Logical. If TRUE then data is for diabetes study, so pregnancy specific statistics should be derived.
#' @export
runGLUForDirectory <- function(indir, outdir=NULL, device=0, daystart='06:30', nightstart='23:00', timeformat='%d/%m/%y %H:%M:%S', impute=FALSE, freq=5, outlierthreshold=5, hypothreshold=NULL, hyperthreshold=NULL, save=FALSE, pregnancy=FALSE, diabetes=FALSE) {


	files = list.files(indir, pattern=".*\\..*", full.names=FALSE)

	runGLUForFiles(files, indir, outdir, device, daystart, nightstart, timeformat, impute, freq, outlierthreshold, hypothreshold, hyperthreshold, save, pregnancy, diabetes)

}

