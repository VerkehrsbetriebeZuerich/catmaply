% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trace.R
\name{add_catmaply_traces}
\alias{add_catmaply_traces}
\title{Add Catmaply traces}
\usage{
add_catmaply_traces(
  fig,
  df,
  hover_hide,
  color_palette,
  categorical_color_range,
  category_items,
  legend_items,
  xgap,
  ygap
)
}
\arguments{
\item{fig}{plotly object}

\item{df}{data.frame or tibble holding the data.}

\item{hover_hide}{boolean indicating if the hover label should be hidden or not; (default: FALSE).}

\item{color_palette}{a color palette vector.}

\item{categorical_color_range}{if the resulting heatmap holds categorical field values or continuous values that belong to a category; (default: FALSE).}

\item{category_items}{distinct/unique items of ordered category items}

\item{legend_items}{distinct/unique items of ordered legend items}

\item{xgap}{Sets the horizontal gap (in pixels) between bricks.}

\item{ygap}{Sets the vertical gap (in pixels) between bricks.}
}
\value{
plot_ly object
}
\description{
Function to produce catmaply traces.
}
\keyword{internal}
