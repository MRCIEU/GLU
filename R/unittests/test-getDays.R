

library("testthat")
source("../getDays.R")
source("genTestTimeSeq5mins.R")
source("../collateSequence.R")


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
days = getDays(rawx, nightstart, daystart, 5)


# num minute value in a day (plus 1 cause of start / end bounds)
numMinutes = 1440
numMinutesNight = 600
numMinutesDay = 800

#### make correct data for day 1 - one minute epochs

print('Day 1')
timeS = strptime('01/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(from=1, to=numMinutesNight+1, by=1)
correctSGNightTime = data.frame(sgReading, time)

timeS = strptime('02/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(numMinutesNight+1, numMinutes+1, 1)
correctSGDayTime = data.frame(sgReading, time)

doCompare(days[[2]], nightstart, daystart, correctSGNightTime, correctSGDayTime)


#### make correct data for day 2 - one minute epochs

print('Day 2')
timeS = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('03/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(numMinutes+1, numMinutes+numMinutesNight+1, 1)
correctSGNightTime = data.frame(sgReading, time)

print('xxx')
timeS = strptime('03/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('03/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(numMinutes+numMinutesNight+1, 2*numMinutes+1, 1)
correctSGDayTime = data.frame(sgReading, time)

doCompare(days[[3]], nightstart, daystart, correctSGNightTime, correctSGDayTime)

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


## set day and night thresholds - night threshold is time of first timepoint
nightstart = strptime('22:00', format='%H:%M')
daystart = strptime('08:00', format='%H:%M')


## get days using GLU function
rawx = list(sg=raw, bg=NULL, events=NULL, dayidx=1, valid=TRUE)
days = getDays(rawx, nightstart, daystart, 5)


# num minute value in a day (plus 1 cause of start / end bounds)
numMinutes = 1440
numMinutesNight = 600
numMinutesDay = 800

#### make correct data for day 1 - one minute epochs

print('Day 1')
timeS = strptime('01/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(from=1, to=numMinutesNight+1, by=1)
sgReading = sgReading + 0.5
correctSGNightTime = data.frame(sgReading, time)

timeS = strptime('02/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(numMinutesNight+1, numMinutes+1, 1)
sgReading = sgReading + 0.5
correctSGDayTime = data.frame(sgReading, time)

doCompare(days[[2]], nightstart, daystart, correctSGNightTime, correctSGDayTime)

#### make correct data for day 2 - one minute epochs

print('Day 2')
timeS = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('03/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(numMinutes+1, numMinutes+numMinutesNight+1, 1)
sgReading = sgReading +	0.5
correctSGNightTime = data.frame(sgReading, time)

print('xxx')
timeS = strptime('03/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('03/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
time = seq(timeS, timeE, 60)
sgReading = seq(numMinutes+numMinutesNight+1, 2*numMinutes+1, 1)
sgReading = sgReading + 0.5
correctSGDayTime = data.frame(sgReading, time)

doCompare(days[[3]], nightstart, daystart, correctSGNightTime, correctSGDayTime)



}




##
## compare sg sequence in day with correct sequence

doCompare <- function(d, nightstart, daystart, correctSGNT, correctSGDT) {

## compare each part of the CGM data

nt = d[["nighttime"]] 
dt = d[["daytime"]]


# check night times

expect_equal(nt$time, correctSGNT$time)
print('Night-time times are equal')


# check night sg readings

expect_equal(nt$sgReading, correctSGNT$sgReading)
print('Night-time sg readings are equal')


# check day times

expect_equal(dt$time, correctSGDT$time)
print('Day-time times are equal')


# check day sg readings

expect_equal(dt$sgReading, correctSGDT$sgReading)
print('Day-time sg readings are equal')

}



test1()
test2()

print('getValidDays OK')

