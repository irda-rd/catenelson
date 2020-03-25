#' @title Quadrat count
#' @description Count the number of points that is present in quadrat defined by \code{group_x} in x and \code{group_y} in y.
#' @param group_x group \code{matrix} containing only one partition (column).
#' @param group_y group \code{matrix} containing one or multiple partitions (column).
#' @return Return a \code{data.frame} with columns named \code{ij}, after the quadrat, with i the quadrat index on the x-axis and j the quadrat index on the y-axis. Each line correspond to a partition of \code{group_y}.
#' @import dplyr
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
#' quadrat_count(group_x, group_y)
#'
quadrat_count <- function(group_x, group_y){
  #Identify groups
  group_name_x <- unique(as.numeric(group_x))
  group_name_y <- unique(as.numeric(group_y))

  #Ensure group_x is only of one column
  if(ncol(group_x) != 1){
    stop("group_x must possess only one column")
  }
  group_x <- matrix(rep(group_x, ncol(group_y)), nrow = nrow(group_y), ncol = ncol(group_y))

  Q <- list()
  for(i in group_name_x){
    for(j in group_name_y){
      logical_quadrat <- (group_x == i & group_y == j)
      Q[[paste0(i,j)]] <- apply(logical_quadrat, MARGIN = 2, sum)
    }
  }
  df <- as.data.frame(bind_cols(Q))
  return(df)
}
