#' @title Quadrat name error
#' @description Generate name of quadrats that does not correspond to the diagonal specified by \code{trend}. Complement of the function \code{quadrat_name_pred}.
#' @param n_group \code{integer}, the number of groups.
#' @param trend \code{character}, either \code{positive} or \code{negative}.
#' @return Return a vector of quadrat names.
#' @export
#' @examples
#' quadrat_name_pred(n_group = 3, trend = "positive")
#' quadrat_name_err(n_group = 3, trend = "positive")
quadrat_name_err <- function(n_group, trend){
  quadrat_pred <- quadrat_name_pred(n_group, trend)
  quadrat_tot <- quadrat_name(n_group)
  quadrat_err <- setdiff(quadrat_tot , quadrat_pred)
  return(quadrat_err)
}
