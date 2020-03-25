#' @title Quadrat name prediction
#' @description Generate name of quadrats that correspond to the diagonal specified by \code{trend}.
#' @param n_group \code{integer}, the number of groups.
#' @param trend \code{character}, either \code{positive} or \code{negative}.
#' @return Return a vector of quadrat names.
#' @export
#' @examples
#' quadrat_name_pred(n_group = 3, trend = "positive")
#' quadrat_name_pred(n_group = 3, trend = "negative")
quadrat_name_pred <- function(n_group, trend){
  #Define name of quadrat where points are expected to be
  if(trend == "positive"){
    group_pred_x <- seq_len(n_group)
    group_pred_y <- seq_len(n_group)
    quadrat_pred <- paste0(group_pred_x, group_pred_y)

  }else if (trend == "negative"){
    group_pred_x <- seq_len(n_group)
    group_pred_y <- seq(n_group,1)
    quadrat_pred <- paste0(group_pred_x, group_pred_y)
  }else{
    stop("trend must be either the string positive or the string negative")
  }

  return(quadrat_pred)
}
