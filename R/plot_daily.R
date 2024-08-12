#' Make a scatter plot of one or more variables from one or more dlfs
#' @param dlfs  Either a list of Dlf or a single Dlf. If a list with of
#'              components, the names are used for the legend.
#' @param variables  Either a list of variables to plot, or a single variable
#' @param hour Which hour to plot
#' @param title_suffix  A string that is appended to the title of all subplots
#' @param plot_line If TRUE plot both points and line otherwise only plot points
#' @return ggplot2
#' @export
#' @examples
#' ## Load dlf data with read_dlf
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "hourly/P2D-Daily-Soil_Chemical_110cm.dlf")
#' dlf <- read_dlf(path)
#'
#' ## Be careful with variable names. R does not like - so it is replaced with _
#' vars <- c("Leak_Matrix", "Transform")
#'
#' ## Plot a single dlf
#' hour <- 0
#' plot_daily(dlf, vars, hour, paste0(" - Soil chemical @ hour ", hour), TRUE)
#'
#' ## Plot multiple dlfs together
#' dlf2 <- dlf
#' dlf2@body[["Leak_Matrix"]] <-  dlf2@body[["Leak_Matrix"]] + 0.001
#' dlf2@body[["Transform"]] <-  dlf2@body[["Transform"]] + 0.05
#' dlfs <- list(original=dlf, derived=dlf2)
#' hour <- hour + 2
#' plot_daily(dlfs, vars, hour, paste0(" - Soil chemical @ hour ", hour))
plot_daily <- function(dlfs, variables, hour=0, title_suffix="",
                       plot_line=FALSE) {
    prepare_dlf <- function(dlf) {
        body <- dlf@body[dlf@body$hour == hour, ]
        dlf <- new("Dlf", header=dlf@header, units=dlf@units, body=body)
        daisyrVis::daisy_time_to_timestamp(dlf)
    }
    if (methods::is(dlfs, 'Dlf')) {
        dlfs <- prepare_dlf(dlfs)
    } else {
        dlfs <- lapply(dlfs, prepare_dlf)
    }
    daisyrVis::points_and_lines(dlfs, "time", variables, title_suffix,
                                plot_line)
}
