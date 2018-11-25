

library("testthat")
source("../SGVP.R")
source("genTestTimeSeq.R")

print('SGVP')

########
## test 1 

# 1 5-minute epochs

print('Test 1')
time = genTestTimeSeq(1)
sgReading = c(5,5.2,5.4,5.6,5.8)
raw = data.frame(time, sgReading)
raw$impute=""

sgvp = SGVP(raw, FALSE)
expect_equal(sgvp, ((4*sqrt(0.2^2+1))/4-1)*100)


#######
## test 2

# constant value

print('Test 2')
time = genTestTimeSeq(1)

sgReading = c(5,5,5,5,5)
raw = data.frame(time, sgReading)
raw$impute=""

li = SGVP(raw,FALSE)
expect_equal(li, 0)


#######
## test 3

# plateau

print('Test 3')
time = genTestTimeSeq(1)
sgReading = c(5,7,7,6,6)
raw = data.frame(time, sgReading)
raw$impute=""

li = SGVP(raw,FALSE)
expect_equal(li, ((sqrt(2^2+1) + 1 + sqrt(1+1) + 1)/4-1)*100)


print('SGVP OK')
