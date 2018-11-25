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



# Plot 3 different graphs to visualise the CGM data of a participant:
# 1. Histogram of SG values.
# 2. Poincare plot (showing how values vary from one timepoint to the next.
# 3. Time-series plots of each days sequence (anotated with events and BG readings)
plotCGM <- function(validDays, dir, userID) {
	options(warn=1)
	print("Plotting ...")


	# create plots dir if it doesn't exist
	plotsdir = paste(dir, 'plots/',sep='')
	if (!file.exists(plotsdir)) {
		print(paste('Plot directory created:', plotsdir))
		dir.create(plotsdir)
	}


	library("ggplot2")
	library("stringr")


	for (vd in validDays) {

		#############
	        ## collate valid day into a single sequence
	        #############

		rawV = collateSequence(vd)
	
		#############
		## plot histogram
		#############
	
		pdf(paste(dir, 'plots/cgm-hist-',userID,'.pdf', sep=""))
		hist(rawV$sgReading, breaks=10, xlab="Sensor glucose", ylab="Number of readings", main=NULL)
		dev.off()

# histogram of differences to see if it's normally distributed
#		pdf(paste(dir, 'plots/cgm-diff-hist-',userID,'.pdf', sep=""))
#		sgReading2 = c(rawV$sgReading[2:nrow(rawV)], NA)
#		sgdiff = sgReading2 - rawV$sgReading
#               hist(sgdiff, breaks=10, xlab="Sensor glucose diff", ylab="Number of readings", main=NULL)
#               dev.off()
	
		#############
		## plot poincare
		#############
	
		plotPoincare(rawV, dir, userID)
	
	
		#############
		## time-series CGM plot
		#############

		plotCGMTrace(rawV, dir, userID, vd)

	}

}

plotPoincare <- function(rawV, dir, userID) {
	xaxis = rawV$sgReading[1:(length(rawV$sgReading)-1)]
        yaxis = rawV$sgReading[2:length(rawV$sgReading)]
        imputeFrom = rawV$impute[1:(length(rawV$impute)-1)]
        imputeTo = rawV$impute[2:(length(rawV$impute))]

        # remove transitions going from / to imputed regions
        ix = which((imputeFrom==TRUE & imputeTo==FALSE) | (imputeFrom==FALSE & imputeTo==TRUE))
        if (length(ix)>0) {
                xaxis = xaxis[-ix]
                yaxis = yaxis[-ix]
        }

	pdf(paste(dir, 'plots/poincare-',userID,'.pdf', sep=""))
        plot(xaxis, yaxis, type='p', pch='+',xlim=c(3,7), ylim=c(3,7), xlab="SG(t)", ylab="SG(t+1)", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
        lines(c(2,9),c(2,9), type='l')
        dev.off()

}

plotCGMTrace <- function(rawV, dir, userID, vd) {

	# plot each segment so that imputed and non-imputed segments are color coded
	groupidx=1
	startx=1
	segcolx = c()

	while(startx <=nrow(rawV)) {

		# set last row of this current segment
		if (rawV$impute[startx] == "") {
			segcolx = cbind(segcolx, '#990099')
	        }
                else {
			segcolx	= cbind(segcolx, '#99CCFF')
		}

		# get first idx of next block
		ix = which(rawV$impute != rawV$impute[startx])
		ixx = which(ix>startx)
		ix = ix[ixx]

                if (length(ix)>0) {
			endx = min(ix)-1
		}
		else {
			endx = nrow(rawV)
		}

		# get this current segment

		segmentname=paste("segment",groupidx,sep='')
		rawV[,segmentname] = NA
		rawV[startx:endx,segmentname] = rawV$sgReading[startx:endx]

		# move to next block
		startx=endx+1
		groupidx=groupidx+1

	}

	p=ggplot(rawV, aes(x=time))

	## add coloured band to background indicating normal range
        p=p+ geom_rect(ymin = opt$hypothreshold, ymax = opt$hyperthreshold, xmin = -Inf, xmax = Inf, fill = '#BBFFBB')

	## line of each segment on the plot
	for (i in 1:(groupidx-1)) {
		segstr=paste("segment",i,sep='')
		p=p+geom_line(aes_string(y = segstr), size=0.1, colour=segcolx[i])+geom_point(aes_string(y = segstr), size=0.1, shape=3, stroke=0.1)
	}


	## set axis labels
	p=p+ xlab("Date / time") + ylab("Glucose value (mmol/L)")

	## rotate x tick labels
	p=p + theme(axis.text.x = element_text(angle = 60, hjust = 1))

	## plot circle around points where deviations are unusually large
        idxDev = which(rawV$deviationLarge == TRUE)
	if (length(idxDev)>0) {
	        pointsDev = rawV[idxDev,]
		p=p+ geom_point(data=pointsDev, aes(x=time, y=sgReading), shape=1, color="red",size=0.5, stroke=0.5)
	}

	## from here on (i.e. plotting meal times etc) we include all the rows in valid days not just those with SG readings


	## plot meal times - square
	events = vd[["events"]]

	if (!is.null(events)) {
		## plot meal times - square
		idxMeal = which(events$event == "MEAL")
		if (length(idxMeal)>0) {
		        meals = events[idxMeal,]
			meals$mealIndicator = 0;
			filCol = "#66FFFF"
			p=p+ geom_point(data=meals, aes(x=time, y=mealIndicator), shape=22, color="gray21", fill=filCol, size=1, stroke=1)
		}

		## plot exercise times - triangle
	        idxExercise = which(events$event == "EXERCISE")
	        if (length(idxExercise)>0) {
	                exerciseOccur = events[idxExercise,]
	                exerciseOccur$exerciseIndicator = 0;
	                filCol = "#66FFFF"
			p=p+ geom_point(data=exerciseOccur, aes(x=time, y=exerciseIndicator), shape=24, color="gray21", fill=filCol, size=1, stroke=1)
	        }

		## plot medication times - star
		idxMeds = which(events$event == "MEDICATION")
	        if (length(idxMeds)>0) {
	                meds = events[idxMeds,]
	                meds$medIndicator = 0;
	                filCol = "#66FFFF"
			p=p+ geom_point(data=meds, aes(x=time, y=medIndicator), shape=8, color="gray21", fill=filCol, size=1, stroke=1)
        	}
	}

	# max y value is highest of sg values and bg values and at least 10
	maxY = max(rawV$sgReading, na.rm = TRUE)

	bgs = vd[["bg"]]
	if (!is.null(bgs)) {
		## plot blood glucose readings - diamonds
	        idxBG = which(!is.na(bgs$bgReading))
	        if (length(idxBG)>0) {
	                bgs = bgs[idxBG, ]
	
	                filCol = "#66FFFF"

			if (max(bgs$bgReading) > maxY) {
				maxY = max(bgs$bgReading)
			}

	                p=p+ geom_point(data=bgs, aes(x=time, y=bgReading), shape=23, color="gray21", fill=filCol, size=1, stroke=1)
	        }
	}

	## set y axis range
	if (maxY<10) {
		maxY = 10
	}

	minY = min(0, min(rawV$sgReading, na.rm = TRUE))
	p=p+scale_y_continuous(limits = c(minY, maxY))


	ggsave(plot=p, file=paste(dir, 'plots/cgm-day',vd$dayidx,'-',userID,'.pdf', sep=""), device="pdf", width=7,height=3)


}






