#' @title Group matrix
#' @description Generate a vector of group, from a division vector.
#' @param division_vector \code{integer}, vector of position that define a new group (row).
#' @param n_row \code{integer}, the number of row of the matrix to build, which should equal the length of the vector to be classified.
#' @return Return a vector of \code{integer} representing the group to which elements of a vector might belong.
#' @details Division is checked for validity before generating the vector.
#' @export
#' @examples
#' #General example
#' x <- sample(1:100, 10)
#' division <- group_division(x, n_group = 2, min_group = 2)
#' group_vector(n_row = length(x), division[,1])
group_vector <- function(n_row, division_vector){
  #Check if division_vector is appropriate
  group_division_check(n_row, division_vector)

  #Convert division (position that define a new group) into a vector of the corresponding groups
  x_index <- seq(1:n_row)
  x_group <- rep(1L,n_row)
  for (i in seq_along(division_vector)){
    logical_group <- x_index >= division_vector[i]
    x_group[logical_group] <- i+1L
  }
  x_group <- as.integer(x_group)
  return(x_group)
}
