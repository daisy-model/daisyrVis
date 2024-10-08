% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_daily.R
\name{plot_daily}
\alias{plot_daily}
\title{Convenience function for plotting daily logged variables in a scatter plot}
\usage{
plot_daily(dlfs, variables, hour = 0, title_suffix = "", plot_line = FALSE)
}
\arguments{
\item{dlfs}{Either a list of Dlf or a single Dlf. If a list with of
components, the names are used for the legend.}

\item{variables}{Either a list of variables to plot, or a single variable}

\item{hour}{Which hour to plot}

\item{title_suffix}{A string that is appended to the title of all subplots}

\item{plot_line}{If TRUE plot both points and line otherwise only plot points}
}
\value{
ggplot2
}
\description{
Convenience function for plotting daily logged variables in a scatter plot
}
\examples{
## Load dlf data with read_dlf
data_dir <- system.file("extdata", package="daisyrVis")
path <- file.path(data_dir, "hourly/P2D-Daily-Soil_Chemical_110cm.dlf")
dlf <- read_dlf(path)

## Be careful with variable names. R does not like - so it is replaced with _
vars <- c("Leak_Matrix", "Transform")

## Plot a single dlf
hour <- 0
plot_daily(dlf, vars, hour, paste0(" - Soil chemical @ hour ", hour), TRUE)

## Plot multiple dlfs together
dlf2 <- dlf
dlf2[["Leak_Matrix"]] <-  dlf2[["Leak_Matrix"]] + 0.001
dlf$Transform <-  dlf2$Transform + 0.05
dlfs <- list(original=dlf, derived=dlf2)
hour <- hour + 2
plot_daily(dlfs, vars, hour, paste0(" - Soil chemical @ hour ", hour))
}
