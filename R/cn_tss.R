#' @title Total sum of squares
#' @description Compute the total sum of squares.
#' @param y \code{numeric} vector on which to calculate the sum of squares.
#' @details Computed through matrix multiplication.
#' @return Return the sum of squares (\code{numerical}).
#' @export
#' @examples
#' y <- rnorm(10)
#' cn_tss(y)
#'
cn_tss <- function(y){
  y <- matrix(y, ncol = 1, nrow = length(y))
  y_mean <- matrix(mean(y), ncol = 1, nrow = length(y))
  y_diff <- y - y_mean
  ss_tot <- as.numeric(t(y_diff) %*% y_diff)
  return(ss_tot)
}
