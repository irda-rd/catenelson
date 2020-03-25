#' @title Cate-Nelson analysis
#' @description Perform a Cate-Nelson analysis, that partition a set of concentrations of a particular soil nutrient (\code{x}), on the basis of yield (\code{y}).
#' The package documentation (\code{\link{catenelson}}) describes the aspects that were improved from the function \code{cateNelson} of the \code{rcompanion} package.
#' @param x \code{numeric} vector of a predictor variable (e.g. nutrient concentration).
#' @param y \code{numeric} vector of the associated response variable (e.g. yield, relative yield, ...).
#' @param label \code{character} characterizing the point (e.g. the site names where the sample were collected). It serves to automatically set color of points on the graph; if \code{label = NULL} (by default), black and white are used.
#' @param n_group \code{integer}, the number of groups in which to partition data in x and in y (possible values: between 2 and 10).
#' @param crit_x_index,crit_y_index \code{integer}, the index of the partition to select. The default value (\code{crit_x_index = 1} or \code{crit_y_index = 1}) correspond to the best partition, see the \code{details} section.
#' @param trend \code{character}, the expected trend of the data, either \code{positive} or \code{negative}.
#' @param min_group_x,min_group_y \code{integer}, the minimum number of different values in each group for the x and y partitioning; \code{min_group_x} must be at least two, \code{min_group_y} can be one.
#' @param details \code{logical} indicating if details about partitioning in x and y should be provided in the output.
#' @param details_prop \code{numeric}, indicating the proportion of the total partition to show in the output (between 0 and 1), used if \code{details = TRUE}.
#' @param x_lab,y_lab,legend \code{character} specifying graphical options, repectively: the name of the x-axis, the name of the y-axis and the position of the legend (\code{none} to remove, look into \code{ggplot2} options).
#' @details
#' The analysis is divided in two parts: the partitioning in \code{x}, followed by the partitioning in \code{y}.
#' Partitioning in \code{x} follows the procedure described in Cate and Nelson (1971), in which:
#' \enumerate{
#' \item Data is sorted according to \code{x}.
#' \item Data is partitioned into groups of contiguous points in \code{x}. At least two points in each groups must be chosen, duplicated data in \code{x} must not be separated and count as one value. Associated critical values in \code{x} are computed from the mid values between the two adjacent points belonging to different groups (not in the original paper).
#' \item The partition that maximize R2 is selected. The model would correspond to a step function with the average value for each group.
#' }
#'
#' Partitioning in \code{y} was mentioned as part of an earlier graphical method in the original paper of Cate and Nelson (1971), but no algorithm was suggested.
#' The function reproduces the approach used in the \code{rcompanion} package, in which:
#' \enumerate{
#' \item Data is sorted according to \code{y}.
#' \item Given the partition in \code{x}, the partition in \code{y} that maximise the number of point on diagonal quadrats, either positive or negative depending on the data \code{trend}, is selected.
#' \item Cramer's V and Ficher's p values associated with the contingency table formed by the quadrats are also calculated as supplementary information. The actual package sort partition in decreasing order of Cramer's V for a same number of point in the diagonal quadrats.
#' }
#' As allowed in the \code{rcompanion} package, one can select the ith best partition in x (\code{crit_x_index}) and in y (\code{crit_y_index}).
#' In both partitioning, one can impose additional constraint on the minimum number of distinct values per group through \code{min_group_x} and \code{min_group_y}.
#' \cr\cr
#' Quadrat names are defined as \code{ij} with \code{i} the index along the x-axis and \code{j} the index along the y-axis. The number of groups has been restricted to a maximum of 10 to avoid ambiguities with the quadrat names, and its repercussion when counting the number of points. The code might be improve in the future to allow a greater number of groups, but the constraint is not likely to be reached because covering all possible partitions would then require a considerable time to compute.
#' \cr\cr
#' The function cate_nelson is a wrapper around the function \code{cate_nelson_x} and \code{cate_nelson_y}, which perform the partitioning respectively in \code{x} and \code{y}.
#'
#' @return
#' The function return a \code{list} with the following elements:
#' \describe{
#' \item{\code{x_partition}}{the partitions in \code{x} and their statistics (\code{data.frame}), sorted by R2, in proportion defined by \code{details_prop}.}
#' \item{\code{y_partition}}{the partitions in \code{y} and their statistics (\code{data.frame}), sorted by \code{p_pred} then \code{cramer_V}, in proportion defined by \code{details_prop}.}
#' \item{\code{model}}{the selected partitions in x and y and their statistics (\code{data.frame}).}
#' \item{\code{group}}{the group in x and y, to which each point belong according to the \code{model}'s partition (\code{data.frame}).}
#' \item{\code{graph}}{a \code{ggplot2} object representing the points and the quadrats, delimited by the critical values of the \code{model}. Empty circles correspond to points falling outside the diagonal quadrats, while full circles represent points within. Color can be added through \code{label}.}
#' }
#' @references
#' Cate RB, Nelson LA. 1971. A simple statistical procedure for partitioning soil test correlation data into two classes. Soil. Sci. Soc. Amer. Proc. 35: 658-660.
#'
#' @export
#' @examples
#' #Generate data
#' n = 30
#' x <- rnorm(n-2)
#' x <- c(x, rep(min(x), 2))
#' y <- x + rnorm(n)
#' label <- LETTERS[(seq_len(n)-1)%%4+1] #Alternative for black and white: label = NULL
#'
#' #Call the function
#' CN <- cate_nelson(x, y, label = label, n_group = 3, trend = "positive")
#'
#' #Investigate the output
#' CN$x_partition
#' CN$y_partition
#' CN$model
#' CN$group
#' CN$graph
#'
cate_nelson <- function(x, y, label = NULL,
                        n_group, crit_x_index = 1, crit_y_index = 1, trend = "positive",
                        min_group_x = 2, min_group_y = 1,
                        details = TRUE, details_prop = 1,
                        x_lab = "X", y_lab = "Y", legend = "bottom"){
  #Check for the maximum number of group (10)
  if(n_group > 10){
    stop("n_group should be less or equal to 10")
  }

  #Partition in x
  ##Compute R2 for each partition in x
  cn_x <- cate_nelson_x(x, y, n_group = n_group, min_group = min_group_x)

  ##Selecting the best (or chosen) partition according to x (maximise R2)
  df_x <- cn_x$df[crit_x_index,]
  group_x <- cn_x$group[,crit_x_index, drop = FALSE]

  #Partition in y
  ##Given the partition in x, compute the number of points in each quadrat, defined by y partition
  cn_y <- cate_nelson_y(y, group_x, min_group = min_group_y, trend = trend)

  #Selecting the best (or chosen) partition in y (maximise the number of point in the predicted quadrats)
  df_y <- cn_y$df[crit_y_index,]
  group_y <- cn_y$group[,crit_y_index, drop = FALSE]
  Q <- cn_y$Q[crit_y_index,, drop = FALSE]

  #For the chosen partition in x and y
  ##Define which in which quadrat  are the points and which quadrats are pred or err
  quadrat_label <- paste0(group_x, group_y)
  quadrat_name_pred <- quadrat_name_pred(n_group, trend)
  quadrat_name_err <- quadrat_name_err(n_group, trend)

  ##Define group association for exportation
  df_group <- data.frame(x = x, y = y, group_x = group_x, group_y = group_y, stringsAsFactors = FALSE)

  ##Produce a graph
  graph <- cate_nelson_graph(x, y, df_x, df_y, quadrat_label, quadrat_name_pred, quadrat_name_err, x_lab = x_lab, y_lab = y_lab, label = label, legend = legend)

  #Exporting object
  object <- list()
  ##Details
  if(details){
    index <- seq_len(round(details_prop*nrow(cn_x$df)))
    object$x_partition <- cn_x$df[index,]
    object$y_partition <- cn_y$df[index,]
  }else{
    object$x_partition <- NULL
    object$y_partition <- NULL
  }
  ##Final model and graph
  object$model <- cbind(df_x, df_y)
  rownames(object$model) <- NULL
  object$group <- df_group
  object$graph <- graph

  #Note: more than one

  return(object)
}

