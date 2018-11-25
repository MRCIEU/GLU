

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

rawNoDevs = raw
rawNoDevs$deviationLarge = FALSE

rawWithDevs = markLargeDeviations(raw)
expect_equal(rawWithDevs, rawNoDevs)



########
## test 2

# 1 large deviation

print('Test 2')

time = genTestTimeSeq(9)
sgReading = c(5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,11,10,9,8,7,6,5,4,3,2,1,11,1,2,3,4,5,6,7,8,9,10,11,10,9.9,9.8,9.7,9.6,9.5,9.4,9.3,9.2,9.1,11,10)
raw = data.frame(time, sgReading)
raw$deviationLarge = rep(FALSE, 45)

rawWithDevs = markLargeDeviations(raw)

raw$deviationLarge[21] = TRUE
raw$deviationLarge[22] = TRUE
raw$deviationLarge[23] = TRUE

expect_equal(rawWithDevs, raw)



print('markLargeDeviations OK')

