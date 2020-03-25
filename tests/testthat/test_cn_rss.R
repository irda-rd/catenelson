context("cn_rss")
#--------------------------------------------------------------------
test_that("the function return the same result as anova", {
  #Data
  group <- matrix(c(rep(1L,4), rep(2L,6), rep(1L,6), rep(2L,4)), ncol = 2)
  S <- group_selection_matrices(group)
  y <- rnorm(10)

  #Answer (perform 1 regression per group)
  answer <- vector("numeric", length = ncol(group))
  for(i in seq_len(ncol(group))){
    group_factor <- as.factor(as.character(group[,i]))
    model <- lm(y ~ group_factor)
    answer[i] <- as.numeric(anova(model)["Residuals","Sum Sq"])
  }

  #Test
  expect_equal(as.numeric(cn_rss(S, y)), answer)
})
#--------------------------------------------------------------------

