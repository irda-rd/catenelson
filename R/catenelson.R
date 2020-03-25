#' @description The package allows to perform a Cate-Nelson analysis, developped to partition a set of concentrations of a particular soil nutrient, on the basis of yield. The package extend the function \code{cateNelson} from the \code{rcompanion} package.
#' @details
#' In particular, the function \code{cateNelson} from the \code{rcompanion} package has been rewritten to:
#' (1) be quicker,
#' (2) allow to divide data into more than two groups (as proposed in Cate and Nelson (1971)) and
#' (3) accept constraints on the minimal number of different values in each group.
#' \cr\cr
#' The package also corrects for minor inconvenients of the original function such as the inability to handle repeated measures on extremities of the \code{x} vector and also provides the graph as a \code{ggplot2} object, which can be further manipulated.
#' Most functionalities and output have been retained from the original function; names of parameters and output might however differ, and now follow the underscore separated naming convention.
#' \cr\cr
#' The function \code{\link{cate_nelson}} is the wrapper function to be used to perform the analysis. It's documentation provides details on the method and its implementation, as well as an example.

#' @references
#' Cate RB, Nelson LA. 1971. A simple statistical procedure for partitioning soil test correlation data into two classes. Soil. Sci. Soc. Amer. Proc. 35: 658-660.
"_PACKAGE"
