#' @title Residual sum of squares (grouped data)
#' @description Compute the residual sum of squares for grouped data (i.e. the within group ss). The groups are defined by selection matrices.
#' @param y \code{numeric}, vector on which calculate the sum of squares
#' @param selection_matrices \code{list} of \code{matrix} which elements, named after the group, represent if the element belong to the group (1) or not (0). The rows of the matrices are associated to elements of \code{y}; while column represent different partitions.
#' @details The residual sum of squares is computed through matrix multiplication, which considerably fasten its calculation when the number of partition is high. This is an alternative to looping on regression and anova for each partition.
#' @return Return a (\code{numeric}) vector of residual sum of squares of length corresponding to the number of partition (possible groupings), determined by the \code{selection_matrices}.
#' @export
#' @examples
#' group <- matrix(c(rep(1L,4), rep(2L,6), rep(1L,6), rep(2L,4)), ncol = 2)
#' S <- group_selection_matrices(group)
#' y <- rnorm(10)
#' cn_rss(S, y)
#'
cn_rss <- function(selection_matrices, y){
  ss <- rep(0, length = ncol(selection_matrices[[1]]))
  for(i in seq_along(selection_matrices)){
    ss <- ss + cn_rss_group(selection_matrices[[i]],y)
  }
  return(ss)
}

