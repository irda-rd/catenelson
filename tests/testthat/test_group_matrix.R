context("group_matrix")
#--------------------------------------------------------------------
test_that("dimension of the matrix respect the number of row and partition", {
  for(i in 1:3){
    division <- matrix(rep(c(2,4,6),i), nrow = 3)
    M <- group_matrix(n_row = 10, division = division)
    result <- dim(M)
    answer <- c(10L, as.integer(i))
    expect_identical(result, answer)
  }
})
#--------------------------------------------------------------------
test_that("dimension of the matrix respect the number of group", {
  for(i in 1:3){
    division <- matrix(rep(sort(sample(2:10,i)),3), ncol = 3)
    M <- group_matrix(n_row = 10, division = division)
    result <- apply(M,MARGIN = 2, unique)
    answer <- matrix(rep(seq(1L,i+1),3), ncol = 3)
    expect_identical(result, answer)
  }
})
#--------------------------------------------------------------------
test_that("the matrix have appropriate values", {
  #Case with one division
  division <- matrix(c(2,4,6),ncol = 3)
  result <- group_matrix(n_row = 10, division = division)
  answer <- matrix(as.integer(c(1,rep(2,9),rep(1,3),rep(2,7), rep(1,5), rep(2,5))), ncol = 3)
  expect_identical(result, answer)
})
#--------------------------------------------------------------------
test_that("the function throw an error for inversed division", {
  division <- matrix(c(3,2),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_matrix(n_row = 10, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error for duplicated value division", {
  division <- matrix(c(2,2),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_matrix(n_row = 10, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error if division index is greater than n_row", {
  division <- matrix(c(2,11),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_matrix(n_row = 10, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error if division index is not greater or equal to 2", {
  division <- matrix(c(-1,2),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_matrix(n_row = 10, division = division), msg)
})
#--------------------------------------------------------------------
test_that("the function throw an error if division index is not an integer", {
  division <- matrix(c(2.5,4),ncol = 1)
  msg <- "inapropriate division vector"
  expect_error(group_matrix(n_row = 10, division = division), msg)
})
#--------------------------------------------------------------------
