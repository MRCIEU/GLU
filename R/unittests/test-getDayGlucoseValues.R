

library("testthat")
source("../getDayGlucoseValues.R")
source("../classes.R")
source("genTestTimeSeq.R")

print('getDayGlucoseValues')

########
## test 1 

# valid day starting at 22:00, which is the start of night time, so that the last
# time point in the day if the first night time point also

print('Test 1')

# night time seq
timeS = strptime('01/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 07:59:00', format='%d/%m/%y %H:%M:%S')
times = seq(timeS, timeE, by=60)
glucoseN = data.frame(times)
glucoseN$sgReading = 5
glucoseN$isnight = TRUE

# day time seq
timeS = strptime('02/01/18 08:00:00', format='%d/%m/%y %H:%M:%S')
timeE = strptime('02/01/18 21:59:00', format='%d/%m/%y %H:%M:%S')
times = seq(timeS, timeE, by=60)
glucoseD = data.frame(times)
glucoseD$sgReading = 8
glucoseD$isnight = FALSE

# last time point, start of next day
last = strptime('02/01/18 22:00:00', format='%d/%m/%y %H:%M:%S')
glucoseL = data.frame(times=last)
glucoseL$sgReading = 10
glucoseL$isnight = TRUE

#print(glucoseL)

glucose = rbind(glucoseN, glucoseD, glucoseL)

#print(glucose)


#### make day object
day = new('day', glucose=glucose, dayidx=1, validday=TRUE, events=data.frame(), bg=data.frame())




# whole seq, no interp
whole = getDayGlucoseValues(day)
expect_equal(whole, glucose[1:(nrow(glucose)-1), ])


print('all day no interp OK')


# whole seq, with interp
whole = getDayGlucoseValues(day, interp=TRUE)
expect_equal(whole, glucose)

print('all day with interp OK')


# day time values, no interp
daytime = getDayGlucoseValues(day, day=TRUE)
expect_equal(daytime$time, glucoseD$time)
expect_equal(daytime$sgReading, glucoseD$sgReading)
expect_equal(daytime$isnight, glucoseD$isnight)


print('daytime no interp OK')


# daytime values, with interp
daytime = getDayGlucoseValues(day, day=TRUE, interp=TRUE)
correct=rbind(glucoseD, glucoseL)
expect_equal(daytime$time, correct$time)
expect_equal(daytime$sgReading, correct$sgReading)
expect_equal(daytime$isnight, correct$isnight)

print('daytime with interp OK')


# night time values, no interp
nighttime = getDayGlucoseValues(day, night=TRUE)
expect_equal(nighttime$time, glucoseN$time)
expect_equal(nighttime$sgReading, glucoseN$sgReading)
expect_equal(nighttime$isnight, glucoseN$isnight)

print('nighttime no interp OK')


# night time values, with interp
nighttime = getDayGlucoseValues(day, night=TRUE, interp=TRUE)
correct=rbind(glucoseN, glucoseD[1,], glucoseL)
expect_equal(nighttime$time, correct$time)
expect_equal(nighttime$sgReading, correct$sgReading)
expect_equal(nighttime$isnight, correct$isnight)


print('nighttime with interp OK')






print('getDayGlucoseValues OK')

