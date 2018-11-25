

library("testthat")
source("../timeToPeak.R")
source("genTestTimeSeq.R")

print('timeToPeak')

########
## test 1 

# No peak - always decreasing

print("test 1")

time = genTestTimeSeq(2)
sgReading = c(5,8,7,6,5,4,3,2,2,1)
raw = data.frame(time, sgReading)

ttp = timeToPeak(raw, time[2])
expect_equal(ttp, NA)




#######
## test 2

# No peak - always increasing

print("test 2")

time = genTestTimeSeq(2)
sgReading = c(5,6,6,6,7,8,8,9,9,9)
raw = data.frame(time, sgReading)

ttp = timeToPeak(raw, time[2])
expect_equal(ttp, NA)



#######
## test 3

# peak after event index

print("test 3")

time = genTestTimeSeq(2)
sgReading = c(5,6,6,6,7,6,6,6,5,4)
raw = data.frame(time, sgReading)

ttp = timeToPeak(raw, time[2])
expect_equal(ttp, 3)


#######
## test 4

# plateaued peak after event index

print("test 4")

time = genTestTimeSeq(2)
sgReading = c(5,6,6,7,7,7,6,4,4,4)
raw = data.frame(time, sgReading)

ttp = timeToPeak(raw, time[2])
expect_equal(ttp, 2)


#######
## test 5

# plateau then a peak after event index

print("test 5")

time = genTestTimeSeq(2)

sgReading = c(5,6,6.5,7,7,7,8,7,6,4)
raw = data.frame(time, sgReading)

ttp = timeToPeak(raw, time[2])
expect_equal(ttp, 5)





print('timeToPeak OK')
