

library("testthat")
source("../getDays.R")
source("../classes.R")
source("genTestTimeSeq5mins.R")


options(warn=1)

print('getDays')

########
## test 1 

# 2 days exactly

test1 <- function() {

print('Test 1')

## generate test data - 5 minute epochs

time = genTestTimeSeq5mins(578)
sgReading = seq(1, length(time)*5, by=5)
raw = data.frame(sgReading)
raw$time = time

## set day and night thresholds - night threshold is time of first timepoint
nightstart = strptime('22:00', format='%H:%M')
daystart = strptime('08:00', format='%H:%M')


## get days using GLU function
rawx = list(sg=raw, bg=NULL, events=NULL, dayidx=1, valid=TRUE)


rs = new('runSettings', indir='', outdir='', device=0, daystart=daystart, nightstart=nightstart, dayPeriodStartTime=nightstart, firstvalid=FALSE, timeformat='HH:MM', imputeApproximal=FALSE, imputeOther=FALSE, freq=5, outlierthreshold=5, hypothreshold=8, hyperthreshold=12, save=FALSE, pregnancy=FALSE, diabetes=FALSE, epochfrequency=5)
participantData = getDays(rawx, rs)


# num minute value in a day (plus 1 cause of start / end bounds)
numMinutes = 1440

#### make correct data for day 1 - one minute epochs

print('Day 1')
timeS = strptime('01/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(from=1, to=numMinutes+1, by=1)
correctSG = data.frame(sgReading, time)

doCompare(participantData@days[[1]], correctSG)


#### make correct data for day 2 - one minute epochs

print('Day 2')
timeS = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('03/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')

time = seq(timeS, timeE, 60)
sgReading = seq(numMinutes+1, numMinutes+numMinutesNight+1, 1)
correctSG = data.frame(sgReading, time)

doCompare(participantData@days[[2]], correctSG)

}


########
## test 2

# 2 days exactly - night and day thresholds are inbetween time points

test2 <- function() {

print('Test 2')

## generate test data

time = genTestTimeSeq5mins(578)

# so that epochs aren't exactly on night/day time thresholds or 1 minute epoch times
time = as.POSIXlt(time - 30)

sgReading = seq(1, length(time)*5, by=5)
raw = data.frame(sgReading)
raw$time = time
raw$interp=FALSE
raw$impute=FALSE

## set day and night thresholds - night threshold is time of first timepoint
nightstart = strptime('22:00', format='%H:%M')
daystart = strptime('08:00', format='%H:%M')


rs = new('runSettings', indir='', outdir='', device=0, daystart=daystart, nightstart=nightstart, dayPeriodStartTime=nightstart, firstvalid=FALSE, timeformat='HH:MM', imputeApproximal=FALSE, imputeOther=FALSE, freq=5, outlierthreshold=5, hypothreshold=8, hyperthreshold=12, save=FALSE, pregnancy=FALSE, diabetes=FALSE, epochfrequency=5)

## get days using GLU function
rawx = list(sg=raw, bg=NULL, events=NULL, dayidx=1, valid=TRUE)
participantData = getDays(rawx, rs)


# num minute value in a day (plus 1 cause of start / end bounds)
numMinutes = 1440

#### make correct data for day 1 - one minute epochs

print('Day 1')
timeS = strptime('01/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(from=1, to=numMinutes+1, by=1)
sgReading = sgReading + 0.5
correctSG = data.frame(sgReading, time)

doCompare(participantData@days[[2]], correctSG)



#### make correct data for day 2 - one minute epochs


print('Day 2')
timeS = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('03/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
#sgReading = seq(from=numMinutes+1, to=2*numMinutes+1, by=1)
sgReading = seq(from=1441, to=2*numMinutes+1, by=1)
sgReading = sgReading +	0.5
correctSG = data.frame(sgReading, time)


doCompare(participantData@days[[3]], correctSG)

}




##
## compare sg sequence in day with correct sequence

doCompare <- function(day, correctGlucose) {

	## compare each part of the CGM data

	# check night times
	expect_equal(day@glucose$time, correctGlucose$time)
	print('Times are equal')

	# check night sg readings
	expect_equal(day@glucose$sgReading, correctGlucose$sgReading)
	print('SG readings are equal')

}



test1()
test2()

print('getValidDays OK')

