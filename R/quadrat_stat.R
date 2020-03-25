#' @title Quadrat statistics
#' @description Compute the Cramer's V and Fisher p values of the contingency matrix made by the quadrats.
#' @param Q \code{numeric} vector corresponding to counts of quadrats.
#' @details The order of elements in \code{Q} are assumed to increment by \code{i} first, then \code{j} (e.g. 11, 12, 12, 21, 22, ...), with i designating quadrat indexes along the the x-axis and j the y-axis. As a supplementary constraint, i and j must range over the same values and therefore Q must have a square number of elements (quadrats).
#' @importFrom rcompanion cramerV
#' @importFrom stats fisher.test
#' @export
#' @examples
#' #Generate data
#' ##Data
#' n = 10
#' x <- rnorm(n)
#' y <- rnorm(n)
#' ##Group x, only one partition
#' division_x  <- group_division(y, 2, min_group = 2)
#' group_x <- group_matrix(n, division_x[,1 , drop = FALSE])
#'
#' ##Group y, multiple partition
#' division  <- group_division(y, 2, min_group = 2)
#' group_y <- group_matrix(n, division)
#'
#' #Compute the number of elements in each quadrat
#' Q <- quadrat_count(group_x, group_y)
#' quadrat_stat(Q)
quadrat_stat <- function(Q){
  #Initialisation
  cramer_V <- vector("integer", length = nrow(Q))
  fisher_p <- vector("integer", length = nrow(Q))

  #Compute contengency matrix and associated statistics for each line of Q (partition)
  for(i in seq_len(nrow(Q))){
    M <- quadrat_contingency(Q[i,])
    cramer_V[i] = as.numeric(cramerV(M))
    fisher_p[i] = fisher.test(M)$p.value
  }

  df <- data.frame(cramer_V = cramer_V, fisher_p = fisher_p)
  return(df)
}

