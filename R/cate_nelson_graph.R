#' @title cate_nelson_graph
#' @description Produce a graph associated to the Cate-Nelson analysis.
#' @param x \code{numeric} vector of a predictor variable (e.g. nutrient concentration).
#' @param y \code{numeric} vector of the associated response variable (e.g. yield, relative yield, ...).
#' @param df_x \code{data.frame} corresponding to the \code{df} element of an object produced by \code{cate_nelson_x} selected for a single partition in \code{x}.
#' @param df_y \code{data.frame} corresponding to the \code{df} element of an object produced by \code{cate_nelson_y} selected for a single partition in \code{y}.
#' @param quadrat \code{character}, vector of the same length as \code{x} and \code{y} defining the quadrats in which points belong.
#' @param quadrat_name_pred \code{character}, name of quadrats that correspond to the diagonal specified by \code{trend}.
#' @param quadrat_name_err \code{character}, name of quadrats that does not correspond to the diagonal specified by \code{trend}.
#' @param label \code{character} characterizing the point (e.g. the site where the sample was collected). It serves to automatically set color of points on the produced graph; if \code{label = NULL} (by default), black and white are used.
#' @param x_lab,y_lab,legend \code{character} specifying graphical options, repectively the name of the x-axis, the name of the y-axis and the position of the legend (\code{none} to remove, look into \code{ggplot2} options).
#' @return Return a \code{ggplot2} object representing points and the quadrats, delimited by the critical values of the \code{model}. Empty circles correspond to point falling outside the diagonal quadrats, while full circles represent point within. Color can be added through \code{label}.
#' @import ggplot2
#' @export
cate_nelson_graph <- function(x, y, df_x, df_y, quadrat, quadrat_name_pred, quadrat_name_err, x_lab = "X", y_lab = "Y", label = NULL, legend = 'bottom'){
  #Quadrats
  ##Quadrats lines
  CLX <- as.numeric(df_x[,grepl("crit_x", colnames(df_x))])
  CLY <- as.numeric(df_y[,grepl("crit_y", colnames(df_y))])

  ##Quadrats names
  pos_x <- seq_mean(c(min(x), CLX, max(x)))
  pos_y <- seq_mean(c(min(y), CLY, max(y)))
  quadrat_label <- expand.grid(pos_y = pos_y, pos_x = pos_x)
  quadrat_label$name <- quadrat_name(length(CLX)+1)

  ##Quadrat factor
  logical_pred <- quadrat %in% quadrat_name_pred
  logical_err <- quadrat %in% quadrat_name_err

  status <- vector("character", length = length(x))
  status[logical_pred] <- "pred"
  status[logical_err] <- "err"
  status = factor(status, levels = c("err","pred"))

  #Labels and status relatated aesthetics
  if(is.null(label)){
    label = as.factor(rep(1,length(x)))
    point_aes <- geom_point(aes(shape = status))

  }else{
    label = as.factor(label)
    point_aes <-geom_point(aes(shape = status, color = label))
  }

  #Dataset
  dataset <- data.frame(x = x, y = y, status = status, label = label)

  #Generate the graph
  gg = ggplot(dataset, aes(x = x, y = y, group = status)) +
    point_aes +
    scale_shape_manual(drop = FALSE, values=c(1, 19))+
    xlab(x_lab) +
    ylab(y_lab) +
    geom_hline(yintercept = as.numeric(CLY)) +
    geom_vline(xintercept = as.numeric(CLX)) +
    annotate("text", x = quadrat_label$pos_x, y = quadrat_label$pos_y, label = quadrat_label$name)+
    theme(panel.background = element_rect(fill = "white" , colour = "grey"))+
    theme(legend.position = legend)

  return(gg)
}
#--------------------------------------------------------------------------------
##Return a vector of length n-1 made of averages between adjacent sorted unique values of x
seq_mean=function(x){
  x=unique(sort(x))
  Lx=length(x)
  y=1/2*(x[1:Lx-1]+x[2:Lx])
  return(y)
}
#--------------------------------------------------------------------------------
