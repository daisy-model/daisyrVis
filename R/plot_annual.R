#' Make a bar plot of one or more variables from one or more dlfs
#' @param dlfs  Either a list of Dlf or a single Dlf. If a list with of
#'              components, the names are used for the legend.
#' @param variables  Either a list of variables to plot, or a single variable
#' @param title_suffix  A string that is appended to the title of all subplots
#' @return ggplot2
#' @export
#' @examples
#' # Load dlf data with read_dlf
#' data_dir <- file.path(system.file("extdata", package="daisyrVis"),
#'                       "annual/Annual-FN")
#' scenarios <- list("2-2b"="HourlyP-Annual-FN-2-2b.dlf",
#'                   "2-3b"="HourlyP-Annual-FN-2-3b.dlf",
#'                   "2-4b"="HourlyP-Annual-FN-2-4b.dlf",
#'                   "2-5b"="HourlyP-Annual-FN-2-5b.dlf")
#' dlfs <- lapply(scenarios, function(fname) {
#'     read_dlf(file.path(data_dir, fname))
#' })
#' # Be careful with variable names. R does not like - so it is replaced with _
#' vars <- c("Matrix_Leaching", "Crop_Uptake", "Soil_Drain", "Surface_Loss")
#' plot_annual(dlfs, vars, " - Annual Field Nitrogen")
plot_annual <- function(dlfs, variables, title_suffix="") {
    geom <- function() {
        ggplot2::geom_col(position="dodge")
    }
    daisyrVis::plot_many(dlfs, "year", variables, geom, title_suffix)
}
