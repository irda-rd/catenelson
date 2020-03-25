context("group_crit")
#--------------------------------------------------------------------
test_that("critical values is a matrix of the same dimension than division", {
    x <- sample(1:100, 10)
    division <- group_division(x, n_group = 3, min_group = 2)

    #Dimension
    result <- group_crit(x, division)
    expect_identical(dim(result), dim(division))

    #Class
    expect_true(is(result,"matrix"))
})
#--------------------------------------------------------------------
test_that("the function throw an error for inversed division", {
  x <- sample(1:100, 10)
  division <- matrix(c(3,2),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_crit(x = x, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error for duplicated value division", {
  x <- sample(1:100, 10)
  division <- matrix(c(2,2),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_crit(x = x, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error if division index is greater than n_row", {
  x <- sample(1:100, 10)
  division <- matrix(c(2,11),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_crit(x = x, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error if division index is not greater or equal to 2", {
  x <- sample(1:100, 10)
  division <- matrix(c(-1,2),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_crit(x = x, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error if division index is not an integer", {
  x <- sample(1:100, 10)
  division <- matrix(c(2.5,4),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_crit(x = x, division = division), msg)
})
#--------------------------------------------------------------------

