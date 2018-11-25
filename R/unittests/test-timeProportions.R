

library("testthat")
source("../timeProportions.R")
source("genTestTimeSeq.R")

print('timeProportions')

low = 3.9
high = 7.8

########
## test 1 

# 1 5-minute epochs - crossing low and normal levels

print("test 1")

time = genTestTimeSeq(1)
sgReading = c(3.8,3.85,3.9,3.95,4)
raw = data.frame(time, sgReading)

tp = timeProportions(raw, low, high)
expect_equal(tp, c(0.5, 0.5,0))



########
## test 2

# 1 5-minute epochs - crossing low, normal and high levels

print("test 2")

time = c('01/01/18 12:00:00', '01/01/18 12:05:00')
time = strptime(time, format='%d/%m/%y %H:%M:%S')

time = genTestTimeSeq(1)
sgReading = c(2,4,6,8,10)
raw = data.frame(time, sgReading)
tp = timeProportions(raw, low, high)
expect_equal(tp, c((low-2)/(10-2), (high-low)/(10-2),(10-high)/(10-2)))


########
## test 3

# all within same level, on boundary

print("test 3")

time = genTestTimeSeq(1)

sgReading = c(3.9,4.0,4.1,4.5,7.8)
raw = data.frame(time, sgReading)
tp = timeProportions(raw, low, high)
expect_equal(tp, c(0,1,0))





print('timeProportions OK')
