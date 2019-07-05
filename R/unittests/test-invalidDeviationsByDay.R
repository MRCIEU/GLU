

library("testthat")
source("../invalidDeviationsByDay.R")
source("../classes.R")
source("genTestTimeSeq.R")

print('invalidDeviationsByDay')

########
## test 1 

# 2 valid days no invalid deviations

print('Test 1')

time = genTestTimeSeq(30)
sgReading = c(rep(5,times=30))
glucose = data.frame(time, sgReading)
glucose$deviationLarge = FALSE

#### make day object
newDay = new('day', glucose=glucose, dayidx=1, validday=TRUE, events=data.frame(), bg=data.frame())

vds = list(newDay, newDay)

inv = invalidDeviationsByDay(vds)

expect_equal(inv[1,1], FALSE)


########
## test 2

# 2 valid days, 1 invalid deviations in day 2

print('Test 2')

time = genTestTimeSeq(30)
sgReading = c(rep(5,times=30))
glucose = data.frame(time, sgReading)
glucose$deviationLarge = FALSE

newDay = new('day', glucose=glucose, dayidx=1, validday=TRUE, events=data.frame(), bg=data.frame())


## day 2 has 2 invalid timepoints

glucose$deviationLarge[4] = TRUE
glucose$deviationLarge[10] = TRUE
newDay2 = new('day', glucose=glucose, dayidx=1, validday=TRUE, events=data.frame(), bg=data.frame())

vds = list(newDay, newDay2)

inv = invalidDeviationsByDay(vds)
expect_equal(inv[1,1], TRUE)


print('invalidDeviationsByDay OK')

