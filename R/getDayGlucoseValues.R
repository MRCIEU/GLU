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




# gets the appropriate timepoints - either the whole day, daytime or nighttime
# if interp then:
# for whole day: include also the last timepoint that completes the day
# for daytime: include also the first nighttime timepoint after a daytime period
# for nighttime: include also the first daytime timepoint after a nighttime period
#interp: whether to include the (exclusive) last timepoint, that is needed if interpolating, to finish off the day
getDayGlucoseValues <- function(dayObj, night=FALSE, day=FALSE, interp=FALSE) {

        # remove last time point - this time point is only for interpolation functionality, it's value really belongs to the next day
        glucose = dayObj@glucose

        if (night==TRUE) {

                if (interp == TRUE) {
                        # include first day timepoint after night, for interpolation
                        gx = c(NA, glucose$isnight[1:(nrow(glucose)-1)])
                        ix = which(glucose$isnight == TRUE | (glucose$isnight == FALSE & (!is.na(gx) & gx == TRUE)))
                        glucose = glucose[ix,]
                }
                else {

                        glucose = glucose[1:(nrow(glucose)-1),]
                        glucose = glucose[which(glucose$isnight==TRUE),]
                }


        }
        else if (day==TRUE) {

                if (interp == TRUE) {
                        # include first night timepoint after day, for interpolation
                        gx = c(NA, glucose$isnight[1:(length(glucose$isnight)-1)])
                        ix = which(glucose$isnight == FALSE | (glucose$isnight == TRUE & (!is.na(gx) & gx == FALSE)))
                        glucose = glucose[ix,]
                }
                else {
                        glucose = glucose[1:(nrow(glucose)-1),]
                        glucose = glucose[which(glucose$isnight==FALSE),]
                }
        }
        else {
                # getting whole day but only values for this day

                if (interp == FALSE) {
                        glucose = glucose[1:(nrow(glucose)-1),]
                }

        }

        return(glucose)

}
