

library("testthat")
source("../invalidDeviationsByDay.R")
source("../collateSequence.R")
source("genTestTimeSeq.R")

print('invalidDeviationsByDay')

########
## test 1 

# 2 valid days no invalid deviations

print('Test 1')

timeBoth = genTestTimeSeq(30)

sgReading = c(rep(5,times=15))

time = timeBoth[1:15]
nighttime = data.frame(time, sgReading)
time = timeBoth[16:30]
daytime = data.frame(time, sgReading)

daytime$deviationLarge = FALSE
nighttime$deviationLarge = FALSE

newDay = list(daytime=daytime, nighttime=nighttime, dayidx=1, valid=TRUE, bg=NULL)
vds = list(newDay, newDay)


inv = invalidDeviationsByDay(vds)

expect_equal(inv[1,1], FALSE)


########
## test 2

# 2 valid days, 1 invalid deviations in day 2

print('Test 2')

timeBoth = genTestTimeSeq(30)

sgReading = c(rep(5,times=15))

time = timeBoth[1:15]
nighttime = data.frame(time, sgReading)
time = timeBoth[16:30]
daytime = data.frame(time, sgReading)

daytime$deviationLarge = FALSE
nighttime$deviationLarge = FALSE

newDay = list(daytime=daytime, nighttime=nighttime, dayidx=1, valid=TRUE, bg=NULL)


## day 2 has 2 invalid timepoint in the nighttime
nighttimeD2 = nighttime
nighttimeD2$deviationLarge[4] = TRUE
nighttimeD2$deviationLarge[10] = TRUE
newDay2 = list(daytime=daytime, nighttime=nighttimeD2, dayidx=2, valid=TRUE, bg=NULL)


vds = list(newDay, newDay2)

inv = invalidDeviationsByDay(vds)
expect_equal(inv[1,1], TRUE)


print('invalidDeviationsByDay OK')

