% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_dlf.R
\name{plot_dlf}
\alias{plot_dlf}
\title{Plot one or more variables from one or more Dlfs.
Each variable is plotted in a separate subplot.}
\usage{
plot_dlf(dlfs, x_var, y_vars, type, title_suffix = "")
}
\arguments{
\item{dlfs}{Either a list of Dlf or a single Dlf. If a list with of
components, the names are  used for the legend.}

\item{x_var}{Name of variable for x axis}

\item{y_vars}{Either a list of variables to plot, or a single variable}

\item{type}{Either a string defining the type of plot to produce OR a
single parameter function returning a ggplot2 object.

If a string, the the following types are supported
* "bar"
* "points"
* "lines"
Types cannot be abbreviated, but they can be combined as in
type="barpoints"
The types can be separated by a non-type string for readability as in
type="bar + lines + points"

If a function, then it is passed a ggplot2 object and it should add a geom to
the object and return it. For example,
type=function(gg) { gg + geom_point() }}

\item{title_suffix}{A string this is appended to the title of all subplots}
}
\value{
A ggplot2 object. If y_vars is a single variable then the plot can be
themed and further data added. If y_vars is a list of more than on
variable, it is not possible to add further data to it.
}
\description{
Plot one or more variables from one or more Dlfs.
Each variable is plotted in a separate subplot.
}
\examples{
data_dir <- system.file("extdata", package="daisyrVis")
path <- file.path(data_dir, "annual/Annual-FN/HourlyP-Annual-FN-2-2b.dlf")
dlf <- read_dlf(path)
# Plot using a string for type
plot_dlf(dlf, "year", "Crop", "bar")
# Same plot with a function for type
geom <- function(gg) { gg + ggplot2::geom_col(position="dodge") }
plot_dlf(dlf, "year", "Crop", geom)
}
