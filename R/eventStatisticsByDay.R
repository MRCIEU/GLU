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



# For each valid day store in validDays, derive event statistics (meals, medication and exercise).
# Returns a data frame containing the event statistics for each day, and average across days.
eventStatisticsByDay <- function(validDays) {

	mtVDs = data.frame()
	exVDs = data.frame()
	medVDs = data.frame()	

        cnamesMT=c()
	cnamesEx=c()
	cnamesMed=c()

        count=1
        for (vd in validDays) {

		events = vd@events

		if (is.null(events)) {
			next
		}

		raw = getDayGlucoseValues(vd)

                # event (meals, exercise, medication) statistics of this valid day only
                mtVD = mealtimeStatistics(events, raw)
		exVD = exerciseStatistics(events, raw)
		medVD = medicationStatistics(events, raw)

		if (count==1) {
			mtVDs = mtVD
			exVDs = exVD
			medVDs = medVD
		} else {
			mtVDs = cbind.data.frame(mtVDs, mtVD)
			exVDs = cbind.data.frame(exVDs, exVD)
			medVDs = cbind.data.frame(medVDs, medVD)
		}

	        cnamesMT = append(cnamesMT, c(paste("meal_timeToPeak_day", count, sep=""), paste("meal_1hr_postprandial_day", count, sep=""), paste("meal_2hr_postprandial_day", count, sep="")))
		cnamesEx = append(cnamesEx, c(paste("exerc_1hr_postprandial_day", count, sep=""), paste("exerc_2hr_postprandial_day", count, sep="")))
		cnamesMed = append(cnamesMed, c(paste("medic_1hr_postprandial_day", count, sep=""), paste("medic_2hr_postprandial_day", count, sep="")))

                count=count+1

        }


	# average each statistic across days
	colnames(mtVDs) = cnamesMT
	colnames(exVDs)	= cnamesEx
	colnames(medVDs) = cnamesMed

	mt_ttp = meanAcrossDays("meal_timeToPeak_day", mtVDs)
	mt_pp1 = meanAcrossDays("meal_1hr_postprandial_day", mtVDs)
	mt_pp2 = meanAcrossDays("meal_2hr_postprandial_day", mtVDs)
	ex_pp1 = meanAcrossDays("exerc_1hr_postprandial_day", exVDs)
       	ex_pp2 = meanAcrossDays("exerc_2hr_postprandial_day", exVDs)
	med_pp1 = meanAcrossDays("medic_1hr_postprandial_day", medVDs)
       	med_pp2 = meanAcrossDays("medic_2hr_postprandial_day", medVDs)


	res = NULL
	if (nrow(mtVDs)>0 | nrow(exVDs)>0 | nrow(medVDs)>0) {

		othervars = c(mt_ttp, mt_pp1, mt_pp2, ex_pp1, ex_pp2, med_pp1, med_pp2)
	  	othervars = rbind(othervars)
		colnames(othervars) = c("mean_meal_timeToPeak", "mean_meal_pp1", "mean_meal_pp2", "mean_exerc_pp1", "mean_exerc_pp2", "mean_medic_pp1", "mean_medic_pp2")

		res = cbind(mtVDs, exVDs, medVDs, othervars)
		
	}

        return(res)

}


