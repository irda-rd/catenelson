context("group_division")
#--------------------------------------------------------------------
test_that("non duplicated values return appropriate division while varying min_group", {
  for(i in 1:5){
    x <- sample(1:100, 10)
    n_group = 2
    min_group = i
    result <- group_division(x, n_group, min_group)
    answer <- matrix(seq(1+i,10+1-i), nrow = 1)
    expect_identical(result, answer)
  }
})
#--------------------------------------------------------------------
test_that("non duplicated values return appropriate division while varying n_group", {
  #Case with 3 groups
  x <- sample(1:100, 5)
  n_group <- 3
  min_group <- 1
  result <- group_division(x, n_group, min_group)
  answer <- matrix(as.integer(c(2,3,2,4,2,5,3,4,3,5,4,5)), nrow = 2)
  expect_identical(result, answer)

  #Case with 4 groups
  x <- sample(1:100, 5)
  n_group <- 4
  min_group <- 1
  result <- group_division(x, n_group, min_group)
  answer <- matrix(as.integer(c(2:4, c(2,3,5), c(2,4:5), 3:5)), nrow = 3)
  expect_identical(result, answer)
})
#--------------------------------------------------------------------
test_that("duplicated values return appropriate division", {
  #Data
  n_group <- 2
  min_group <- 2
  x <- vector("list", length = 3)
  x[[1]] <- 100*c(1:3, 3, 3:5)
  x[[2]] <- 100*c(rep(1,3), 2:5)
  x[[3]] <- 100*c(1:4, rep(5,3))

  #Answer
  answer <- vector("list", length = 3)
  answer[[1]] <- matrix(c(3L, 6L), nrow = 1)
  answer[[2]] <- matrix(c(5L, 6L), nrow = 1)
  answer[[3]] <- matrix(c(3L, 4L), nrow = 1)

  #Tests
  for(i in seq_along(x)){
    result <- group_division(x[[i]], n_group = n_group, min_group = min_group)
    expect_identical(result, answer[[i]])
  }
})
#--------------------------------------------------------------------
test_that("case with one group return appropriate error ", {
  x <- sample(1:100, 10)
  n_group <- 1
  min_group <- 1
  msg <- "at least two groups are required to properly define a group division"
  expect_error(group_division(x, n_group = n_group, min_group = min_group), msg)
})
#--------------------------------------------------------------------
test_that("case with too large n_group for the data return appropriate error", {
  x <- sample(1:100, 10)
  n_group = 10
  min_group = 2
  msg <- "no group division meet the conditions imposed"
  expect_error(group_division(x, n_group = n_group, min_group = min_group), msg)
})
#--------------------------------------------------------------------
test_that("case with too large min_group for the data return appropriate error", {
  x <- sample(1:100, 10)
  n_group = 2
  min_group = 6
  msg <- "no group division meet the conditions imposed"
  expect_error(group_division(x, n_group = n_group, min_group = min_group), msg)
})
#--------------------------------------------------------------------
test_that("case with no solution return the appropriate error (min_group)", {
  x <- 100*c(1:3, 3, 3:5)
  n_group = 3
  min_group = 2
  msg <- "no group division meet the conditions imposed"
  expect_error(group_division(x, n_group = n_group, min_group = min_group), msg)
  })
#--------------------------------------------------------------------
test_that("case with no solution return the appropriate error (min_crit, max_crit)", {
  x <- sample(1:100, 10)
  n_group = 3
  min_group = 2
  msg <- "no group division meet the conditions imposed"
  expect_error(group_division(x, n_group = n_group, min_group = min_group, max_crit = c(0)), msg)
  expect_error(group_division(x, n_group = n_group, min_group = min_group, min_crit = c(101)), msg)
})
#--------------------------------------------------------------------
