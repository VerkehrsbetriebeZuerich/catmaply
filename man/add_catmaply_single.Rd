% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trace.R
\name{add_catmaply_single}
\alias{add_catmaply_single}
\title{Add single Catmaply traces}
\usage{
add_catmaply_single(
  fig,
  df,
  hover_hide,
  color_palette,
  categorical_color_range,
  legend_items,
  legend,
  visible = 1,
  colorbar_y = NA,
  xgap,
  ygap
)
}
\arguments{
\item{fig}{plotly object}

\item{df}{data.frame or tibble holding the data.}

\item{color_palette}{a color palette vector.}

\item{legend_items}{distinct/unique items of ordered legend items}

\item{legend}{boolean indicating if legend should be displayed or not; (default: TRUE).}

\item{colorbar_y}{y position of colorbar; (default: NA).}

\item{xgap}{Sets the horizontal gap (in pixels) between bricks.}

\item{ygap}{Sets the vertical gap (in pixels) between bricks.}
}
\value{
plot_ly object
}
\description{
Function to produce a single catmaply trace without interactive legend.
}
\keyword{internal}
