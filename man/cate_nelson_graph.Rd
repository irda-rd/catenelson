% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cate_nelson_graph.R
\name{cate_nelson_graph}
\alias{cate_nelson_graph}
\title{cate_nelson_graph}
\usage{
cate_nelson_graph(x, y, df_x, df_y, quadrat, quadrat_name_pred,
  quadrat_name_err, x_lab = "X", y_lab = "Y", label = NULL,
  legend = "bottom")
}
\arguments{
\item{x}{\code{numeric} vector of a predictor variable (e.g. nutrient concentration).}

\item{y}{\code{numeric} vector of the associated response variable (e.g. yield, relative yield, ...).}

\item{df_x}{\code{data.frame} corresponding to the \code{df} element of an object produced by \code{cate_nelson_x} selected for a single partition in \code{x}.}

\item{df_y}{\code{data.frame} corresponding to the \code{df} element of an object produced by \code{cate_nelson_y} selected for a single partition in \code{y}.}

\item{quadrat}{\code{character}, vector of the same length as \code{x} and \code{y} defining the quadrats in which points belong.}

\item{quadrat_name_pred}{\code{character}, name of quadrats that correspond to the diagonal specified by \code{trend}.}

\item{quadrat_name_err}{\code{character}, name of quadrats that does not correspond to the diagonal specified by \code{trend}.}

\item{x_lab, y_lab, legend}{\code{character} specifying graphical options, repectively the name of the x-axis, the name of the y-axis and the position of the legend (\code{none} to remove, look into \code{ggplot2} options).}

\item{label}{\code{character} characterizing the point (e.g. the site where the sample was collected). It serves to automatically set color of points on the produced graph; if \code{label = NULL} (by default), black and white are used.}
}
\value{
Return a \code{ggplot2} object representing points and the quadrats, delimited by the critical values of the \code{model}. Empty circles correspond to point falling outside the diagonal quadrats, while full circles represent point within. Color can be added through \code{label}.
}
\description{
Produce a graph associated to the Cate-Nelson analysis.
}
