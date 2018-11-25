

library("testthat")
source("../postprandial.R")
source("genTestTimeSeq.R")
source("../auc.R")

print('postprandial')

########
## test 1 

# 1 hr post-event

print('Test 1')

time = genTestTimeSeq(17)
sgReading = rep(5,times=(12*5))
sgReading = c(sgReading, rep(6,25))
raw = data.frame(time, sgReading)

postEvent = postprandial(raw, time[1], 1)
expect_equal(postEvent, 6)

########
## test 2

# 1 hr post-event - starts after end of sequence

print('Test 2')

time = genTestTimeSeq(15)
sgReading = rep(5,times=12)
sgReading = c(sgReading, 6,6,6)
raw = data.frame(time, sgReading)

postEvent = postprandial(raw, time[5], 1)
expect_equal(postEvent, NA)


########
## test 3

# 1 hr post-event - starts before end of seq but there aren't 3 consecutive values

print('Test 3')

time = genTestTimeSeq(15)
sgReading = rep(5,times=12)
sgReading = c(sgReading, 6,6,6)
raw = data.frame(time, sgReading)

postEvent = postprandial(raw, time[3], 1)
expect_equal(postEvent, NA)




print('postprandial OK')
