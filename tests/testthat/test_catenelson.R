context("cate_nelson")
#--------------------------------------------------------------------
#Data from Cate and Nelson (1971) does not seem to correspond to the one analyzed, and therefore cannot be used for testing.
#--------------------------------------------------------------------
test_that("the function return the same result as the package rcompanion for 2 groups", {
  #Data
  n = 30
  x <- rnorm(n)
  y <- x + rnorm(n)

  #Model from the rcompanion package (renamed to be compared with cate_nelson)
  model_rcompanion <- rcompanion::cateNelson(x,y,plotit = FALSE)
  model_rcompanion_renamed <- model_rcompanion %>% rename(crit_x_1 = CLx, crit_y_1 = CLy, ess = SS, n_11 = Q.IV, n_21 = Q.III, n_12 = Q.I, n_22 = Q.II, p_pred = p.Model, p_err = p.Error, n_pred = Q.Model, n_err = Q.Error, cramer_V = Cramer.V, fisher_p = Fisher.p.value)

  #Model from the rcompanion package (renamed to be compared with cate_nelson)
  model_catenelson <- cate_nelson(x, y, n_group = 2)$model
  model_catenelson_subset <- model_catenelson[,names(model_rcompanion_renamed)]

  #Test that both models are almost equals
  expect_equivalent(model_catenelson_subset, model_rcompanion_renamed )
})
#--------------------------------------------------------------------
test_that("the function does not generate error for duplicated values on extremities", {
  #Repeated x at the low end
  ##Generate data
  n = 30
  x <- rnorm(n-2)
  x <- c(x, rep(min(x), 2))
  y <- x + rnorm(n)
  #Test that no error occurs
  expect_error(cate_nelson(x, y, n_group = 2), regexp = NA)

  #Repeated x at the high end
  ##Generate data
  n = 30
  x <- rnorm(n-2)
  x <- c(x, rep(max(x), 2))
  y <- x + rnorm(n)
  #Test that no error occurs
  expect_error(cate_nelson(x, y, n_group = 2), regexp = NA)
})
#--------------------------------------------------------------------
test_that("the function does not generate error when using various parameters", {
  #Generic example
  n = 30
  x <- rnorm(n-2)
  x <- c(x, rep(min(x), 2))
  y <- x + rnorm(n)
  label <- LETTERS[(seq_len(n)-1)%%4+1]
  #label = NULL
  n_group <- 3
  crit_x_index = 2
  crit_y_index = 3
  min_group_x = 5
  min_group_y = 5
  x_lab = "test_x_lab"
  y_lab = "test_y_lab"
  legend = "none"
  trend = "negative"

  expect_error(cate_nelson(x, y, label = label,
                           n_group = n_group, crit_x_index = crit_x_index, crit_y_index = crit_y_index, trend = trend,
                           min_group_x = min_group_x, min_group_y = min_group_y,
                           details = TRUE, details_prop = 1,
                           x_lab = x_lab, y_lab = y_lab, legend = legend),
               regexp = NA)
})
#--------------------------------------------------------------------
