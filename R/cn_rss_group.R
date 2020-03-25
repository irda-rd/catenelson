#' @title Residual sum of squares (grouped data, one group)
#' @description Compute the residual sum of squares for grouped data (i.e. the within group ss) for one group only. Group belonging is defined by a selection matrix.
#' @param y \code{numeric}, vector on which calculate the sum of squares
#' @param selection_matrix \code{matrix} which elements represent if the element belong to the group (1) or not (0). The rows of the matrices are associated to elements of \code{y}; while column represent different partitions.
#' @details The residual sum of squares is computed through matrix multiplication, which considerably fasten its calculation when the number of partition is high. This is an alternative to looping on regression and anova for each partition.
#' @return Return a (\code{numeric}) vector of residual sum of squares of length corresponding to the number of partition, determined by the \code{selection_matrices}.
#' @export
#' @examples
#' group <- matrix(c(rep(1L,4), rep(2L,6), rep(1L,6), rep(2L,4)), ncol = 2)
#' S <- group_selection_matrices(group)
#' y <- rnorm(10)
#' cn_rss(S, y)
cn_rss_group <- function(selection_matrix, y){
  #Convert as vector matrix
  y <- matrix(y, ncol = 1, nrow = length(y))

  #Group mean (vector matrix)
  group_n <- matrix(apply(selection_matrix, MARGIN = 2, sum), ncol = 1)
  group_mean <- (1/group_n * t(selection_matrix) %*% y)

  #Sum of square
  ##Generate matrix of y values (1 row pervalue) and mean (1 column per division)
  y_matrix <- y %*% matrix(1, ncol = ncol(selection_matrix))
  group_y_matrix <- y_matrix * selection_matrix

  group_mean_matrix <- t(group_mean %*% matrix(1, ncol = length(y))) * selection_matrix
  ##Take (diagonal elements correspond to the ss of each division)
  diff_matrix <- group_y_matrix-group_mean_matrix
  group_ss <- diag(t(diff_matrix) %*% diff_matrix)

  return(group_ss)
}
