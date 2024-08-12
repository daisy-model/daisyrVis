#' Plot one or more variables from one or more dlfs using points and lines
#' @param dlfs  Either a list of Dlf or a single Dlf. If a list, the names are
#'              used for the legend.
#' @param x_var  Name of variable for x axis
#' @param y_vars  Either a list of variables to plot, or a single variable
#' @param title_suffix  A string that is appended to the title of all subplots
#' @param plot_line If TRUE plot both points and line otherwise only plot points
#' @return ggplot2
#' @export
#' @examples
#' ## Load dlf data with read_dlf
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir,
#'                   "annual/Annual-Tracer/HourlyP-Annual-Tracer-2-2b.dlf")
#' dlf <- read_dlf(path)
#' dlf <- daisy_time_to_timestamp(dlf, "date")
#' points_and_lines(dlf, "date", "Soil", "Soil tracer", FALSE)
#' points_and_lines(dlf, "date", "Soil", "Soil tracer", TRUE)
points_and_lines <- function(dlfs, x_var, y_vars, title_suffix="",
                             plot_line=FALSE) {
    geom <- function(gg) {
        gg <- gg + ggplot2::geom_point()
        if (plot_line) {
            gg <- gg + ggplot2::geom_line()
        }
        gg + ggplot2::guides(fill="none")
    }
    daisyrVis::plot_many(dlfs, x_var, y_vars, geom, title_suffix)
}
