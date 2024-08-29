#' Convenience function for plotting annualy logged variables in a bar plot
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
#' dlfs <- read_dlf_dir(data_dir)
#' # Be careful with variable names. R does not like - so it is replaced with _
#' vars <- c("Matrix_Leaching", "Crop_Uptake", "Soil_Drain", "Surface_Loss")
#' plot_annual(dlfs, vars, " - Annual Field Nitrogen")
plot_annual <- function(dlfs, variables, title_suffix="") {
    daisyrVis::plot_dlf(dlfs, "year", variables, "bar", title_suffix)
}
