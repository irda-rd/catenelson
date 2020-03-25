#' @title Quadrat contingency
#' @description Form a contingency table (\code{matrix}) from quadrat counts.
#' @param Q \code{numeric} vector corresponding to counts of quadrats.
#' @details The order of elements in \code{Q} are assumed to increment by \code{i} first, then \code{j} (e.g. 11, 12, 12, 21, 22, ...), with i designating quadrat indexes along the the x-axis and j the y-axis. As a supplementary constraint, i and j must range over the same values and therefore Q must have a square number of elements (quadrats).
#' @return Return the corresponding contingency \code{matrix}.
#' @export
#' @importFrom methods is
#' @examples
#' Q <- rnorm(9)
#' quadrat_contingency(Q)
quadrat_contingency <- function(Q){
  #
  if(is(Q, "matrix")){
    if(nrow(Q) == 1){
      Q <- as.numeric(Q)
    }else{
      stop("Q must possess only one line if a matrix")
    }
  }
  #Identify groups
  n_group <- sqrt(length(Q))
  M <- matrix(as.numeric(Q), ncol = n_group, byrow = TRUE)
  return(M)
}
