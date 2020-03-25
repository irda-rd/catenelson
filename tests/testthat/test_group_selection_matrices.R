context("group_selection_matrices")
#--------------------------------------------------------------------
test_that("the sum of all matrices is a matrix of ones", {
  #Data
  group <- t(gtools::permutations(3,3,1:3))
  S <- group_selection_matrices(group)
  #Result and answer
  result <- matrix(0L, ncol = ncol(group), nrow = nrow(group))
  for(i in seq_along(S)){
    result <- result + S[[i]]
  }
  answer <- matrix(1L, ncol = ncol(group), nrow = nrow(group))
  #Test
  expect_identical(result, answer)
})
#--------------------------------------------------------------------
test_that("selection matrices are unaffected by the order of appearance of groups", {
  group1 <- matrix(c(1,2,1,2), ncol = 2)
  result1 <- group_selection_matrices(group1)

  group2 <- matrix(c(2,1,2,1), ncol = 2)
  result2 <- group_selection_matrices(group2)

  #Verify that group 1 in the first call is the same group 1 as in the second (idem for group 2)
  expect_true(all(result1[[1]] == result2[[2]]) & all(result1[[2]] == result2[[1]]) & all(names(result1) == names(result2)))
})
#--------------------------------------------------------------------
test_that("the function throw an error if group names are inapropriate", {
  group <- matrix(c(2,3,2,3), ncol = 2)
  msg <- "groups in the group matrix must be integers incremented from one"
  expect_error(group_selection_matrices(group), msg)

  group <- matrix(letters[c(2,3,2,3)], ncol = 2)
  msg <- "groups in the group matrix must be integers incremented from one"
  expect_error(group_selection_matrices(group), msg)
})
#--------------------------------------------------------------------
