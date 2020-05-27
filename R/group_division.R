#' @title Group division
#' @description Find the possible ways to separate an ordered numerical vector in groups.
#' @param x \code{numeric}, an ordered vector of elements.
#' @param n_group \code{integer}, the number of groups (minimum 2).
#' @param min_group \code{integer}, the minimum number of values in each group (not elements).
#' @param min_crit,max_crit \code{numeric} vectors of length corresponding to the number of group (number of rows of \code{division}) minus one, representing inequality constraints on each critical values in \code{x} \code{(min <= x <= max)}. The default value is \code{NULL} and represent no constraint.
#' @return Return a \code{matrix} of position that define new groups (divisions). Rows represent the \code{n_group - 1} divisions for a given partition, while columns represent different partitions.
#' @details Repeated values are kept in the same group and only count as one value with respect to the \code{min_group} constraint. An error is thrown if no group division meet the conditions imposed.
#' @export
#' @importFrom utils combn
#' @examples
#' #General example
#' x <- sample(1:100, 10)
#' group_division(x, n_group = 2, min_group = 2)
#'
#' #Example with a repeated value
#' x <- 100*c(1:3, 3, 3:5)
#' group_division(x, n_group = 2, min_group = 2)
#'
group_division <- function(x, n_group, min_group, min_crit = NULL, max_crit = NULL){
  #Verify condition on n_group
  if(n_group == 1){
    stop("at least two groups are required to properly define a group division")
  }

  #Generate possible place where to divide group (the position define a new group)
  ##Identify duplicated values (duplicated x must belong to the same group)
  dupl <- which(duplicated(x))
  division <- seq(2, length(x))
  division <- setdiff(division, dupl)

  #Generate possible combinations to generate n groups
  C <- combn(division, n_group - 1)

  #Verify there is at least one solution (before generating group matrix)
  if(ncol(C) == 0){
    stop("no group division meet the conditions imposed")
  }

  #Preserve only combinations that generate at least 2 different points in each group
  ##Generate group matrix, while not considering duplicated values in count
  M <- group_matrix(length(x), C)
  i_not_dupl <- setdiff(seq_along(x), dupl)
  M <- M[i_not_dupl,,drop = FALSE]

  ##Verify there is at least one solution (before generating selection matrices)
  if(ncol(M) == 0){
    stop("no group division meet the conditions imposed")
  }
  ##Count the number of different values in each group for each partition and compare to the minimum
  S <- group_selection_matrices(M)
  logical_min_group <- vector("list", length = n_group)
  for(i in seq_len(n_group)){
    logical_min_group[[i]] <- (apply(S[[i]], MARGIN = 2, sum) >= min_group)
  }

  #Keep partitions that respect the minimum number of element in all groups
  logical_keep <- rep(TRUE, ncol(M))
  for(i in seq_along(S)){
    logical_keep <- logical_keep & logical_min_group[[i]]
  }
  C <- C[,logical_keep, drop = FALSE]
  
  #Verify there is at least one solution (before applying further restriction)
  if(ncol(C) == 0){
    stop("no group division meet the conditions imposed")
  }
  
  #Preserve only combinations which critical values respect bounding conditions
  logical_bound <- group_division_bound(x, division = C, min_crit = min_crit, max_crit = max_crit)
  C <- C[,logical_bound, drop = FALSE]
  
  #Verify there is at least one solution (before exporting)
  if(ncol(C) == 0){
    stop("no group division meet the conditions imposed")
  }

  return(C)
}
