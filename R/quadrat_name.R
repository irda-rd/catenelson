#' @title Quadrat name
#' @description Generate names of quadrats.
#' @param n_group \code{integer}, the number of groups.
#' @details The order of elements are incremented by \code{i} first, then \code{j} (e.g. 11, 12, 12, 21, 22, ...), with i designating quadrat indexes along the the x-axis and j the y-axis. As a supplementary constraint, i and j range from 1 to the \code{n_group}, representing a squared number of quadrats. Names are non ambiguous for n_group smaller or equal to 10
#' @return Return a vector of name for the quadrats (\code{character}).
#' @export
#' @examples
#' n_group = 10
#' quadrat_name(n_group)
quadrat_name <- function(n_group){
  df <- expand.grid(y = seq_len(n_group), x = seq_len(n_group))
  quadrat_name <- paste0(df$x,df$y)
  return(quadrat_name)
}
