

library("testthat")
source("../getUserIDFromFileName.R")

print('getUserIDFromFileName')

########
## test 1 

print('Test 1')
id1 = getUserIDFromFileName('data_export-123.csv')
expect_equal(id1, '123')

########
## test 2

print('Test 2')
id1 = getUserIDFromFileName('data_export-123.tsv')
expect_equal(id1, '123')


print('getUserIDFromFileName OK')


