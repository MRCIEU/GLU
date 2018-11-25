

library("testthat")
source("../fastingProxy.R")
source("genTestTimeSeq.R")

print('fasting proxy')

########
## test 1 

# simple case - lowest 3 are in the middle of the seq

print("test 1")

time = genTestTimeSeq(15)

sgReading = c(5, rep(3,times=length(time)-2),5)
raw = data.frame(time, sgReading)

fp = fastingProxy(raw)
expect_equal(fp, 3)


########
## test 2

# mix of values	that form 30 mins with lowest consecutive values

print("test 2")

time = genTestTimeSeq(7)

sgReading = c(rep(1,times=length(time)-30), rep(3,times=30))
raw = data.frame(time, sgReading)

fp = fastingProxy(raw)
expect_equal(fp, (3*25+5)/30)




print('fastingProxy OK')
