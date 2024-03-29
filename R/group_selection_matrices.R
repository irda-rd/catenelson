#' @title Selection matrices
#' @description Generate a list of selection matrices from a group matrix.
#' @param group \code{matrix} of \code{integer} indicating the group elements belong (rows) for various partition (columns), as generated by the function \code{group_matrix}.
#' @return Return a list of matrix which elements, named after the group, represent if the element belong to the group (1) or not (0).
#' @details The groups in the \code{group} matrix must be integers, incremented from 1.
#' @export
#' @examples
#' group <- t(gtools::permutations(3,3,1:3))
#' group_selection_matrices(group)
#'
group_selection_matrices <- function(group){
  #Verify that groups go
  group_name <- unique(sort(group))
  if(any(group_name != seq_along(group_name))){
    stop("groups in the group matrix must be integers incremented from one")
  }

  #Determining the number of group
  n_group <- group_nb(group)

  L <- vector("list",length = n_group)
  names(L) <- group_name
  for(i in group_name){
    M <- matrix(0L, nrow = nrow(group), ncol = ncol(group))
    M[which((group == i))] <- 1L
    L[[i]] <- M
  }

  return(L)
}

