

library("testthat")
source("../auc.R")
source("genTestTimeSeq.R")

print('auc')

########
## test 1 

# 1 5-minute epochs

print('Test 1')

time = genTestTimeSeq(2)
sgReading = c(5,6)
raw = data.frame(time, sgReading)
aucVal = auc(raw)

# times by 60 because AUC is per minute not second
expect_equal(aucVal, 5.5)


#######
## test 2

# constant value

print('Test 2')

time = genTestTimeSeq(4)
sgReading = c(5,5,5,5)
raw = data.frame(time, sgReading)
aucVal = auc(raw)

expect_equal(aucVal, 5)


#######
## test 3

# plateau

print('Test 3')

time = genTestTimeSeq(4)
sgReading = c(4,5,6,7,8,9,9,9,9,9,9,9,9,9,9,8,7,6,5,4)
raw = data.frame(time, sgReading)
aucVal = auc(raw)

expect_equal(aucVal, (4.5*2+5.5*2+6.5*2+7.5*2+8.5*2+9*9)/19)



print('AUC OK')


