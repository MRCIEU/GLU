

library("testthat")
source("../markLargeDeviations.R")
source("genTestTimeSeq.R")

print('markLargeDeviations')

########
## test 1 

# No large deviations

print('Test 1')

time = c('01/01/18 12:00:00', '01/01/18 12:01:00')
time = strptime(time, format='%d/%m/%y %H:%M:%S')

sgReading = c(5,6)
raw = data.frame(time, sgReading)
raw$impute = FALSE

rawNoDevs = raw
rawNoDevs$deviationLarge = FALSE

rawWithDevs = markLargeDeviations(raw)

expect_equal(rawWithDevs, rawNoDevs)



########
## test 2

# 1 large deviation

print('Test 2')

time = genTestTimeSeq(12)
sgReading = c(8.5,8.4,8.3,8.2,8.1,8.0,8.1,8.2,8.3,8.3,8.4,8.5,8.6,8.7,8.6,8.5,8.5,8.6,8.7,8.8,9.0,25,9.0,9.1,9.2,9.3,9.3,9.4,9.5,9.6,9.7,9.8,9.9,10,9.9,9.8,9.7,9.6,9.5,9.4,9.3,9.2,9.1,9.2,9.3,9.3,9.2,9.1,9.2,9.3,9.3,9.2,9.1,9.2,9.3,9.3,9.2,9.1,9.2,9.3)
raw = data.frame(time, sgReading)
raw$deviationLarge = FALSE
raw$impute = FALSE

rawWithDevs = markLargeDeviations(raw)

raw$deviationLarge[22] = TRUE

expect_equal(rawWithDevs, raw)



print('markLargeDeviations OK')

