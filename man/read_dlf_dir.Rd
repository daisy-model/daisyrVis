% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_dlf_dir.R
\name{read_dlf_dir}
\alias{read_dlf_dir}
\title{Walk a directory tree and read all dlf files in it.}
\usage{
read_dlf_dir(directory, pattern = ".*\\\\.dlf")
}
\arguments{
\item{directory}{Path to directory}

\item{pattern}{Regex pattern of files to include}
}
\value{
A list of S4 objects of class Dlf. Each dlf is named with the
relative path to it
}
\description{
Walk a directory tree and read all dlf files in it.
}
\examples{
data_dir <- system.file("extdata", package="daisyrVis")
dlfs <- read_dlf_dir(file.path(data_dir, "annual"))
print(names(dlfs))

dlfs <- drop_dir_from_names(dlfs)
print(names(dlfs))

dlfs <- strip_common_prefix_from_names(dlfs)
print(names(dlfs))
}
