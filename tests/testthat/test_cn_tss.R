context("cn_tss")
#--------------------------------------------------------------------
test_that("the function return the same result as anova", {
  #Data
  group <- matrix(c(rep(1L,5), rep(2L,5)), ncol = 1)
  S <- group_selection_matrices(group)
  y <- rnorm(10)

  #Answer
  group_factor <- as.factor(as.character(group))
  model <- lm(y ~ group_factor)
  answer <- sum(anova(model)[,"Sum Sq"])

  #Test
  expect_equal(cn_tss(y), answer)

})
#--------------------------------------------------------------------
