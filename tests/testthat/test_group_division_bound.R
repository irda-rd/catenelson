context("group_division_bound")
#--------------------------------------------------------------------
test_that("partition with critical values outside bounds are excluded", {
  x <- c(9,11, sample(12:19, 3), 20,22)
  division <- group_division(x, n_group = 3, min_group = 1)
  logical_keep <- group_division_bound(x, division, min_crit = c(11,11), max_crit = c(20,20))
  result <- all(division[,logical_keep] %in% 3:6)
  expect_true(result)
  })
#--------------------------------------------------------------------
test_that("partition with critical values equal to bounds are included", {
  x <- c(9,11, sample(12:19, 3), 20,22)
  division <- group_division(x, n_group = 3, min_group = 1)
  logical_keep <- group_division_bound(x, division, min_crit = c(10,10), max_crit = c(21,21))
  result <- all(division[,logical_keep] %in% 2:7)
  expect_true(result)
})
#--------------------------------------------------------------------
test_that("partition with critical values outside maximum sequence are excluded", {
  x <- c(9,11, sample(12:19, 3), 20,22)
  division <- group_division(x, n_group = 3, min_group = 1)
  logical_keep <- group_division_bound(x, division, max_crit = c(20,Inf))
  result <- all(logical_keep)
  expect_true(result)
})
#--------------------------------------------------------------------
test_that("partition with critical values outside minimum sequence are excluded", {
  x <- c(9,11, sample(12:19, 3), 20,22)
  division <- group_division(x, n_group = 3, min_group = 1)
  logical_keep <- group_division_bound(x, division, min_crit = c(-Inf,11))
  result <- all(logical_keep)
  expect_true(result)
})
#--------------------------------------------------------------------
