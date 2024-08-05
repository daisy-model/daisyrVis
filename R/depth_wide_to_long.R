#' Transform wide format time series to a long format time series
#' It is assumed that dlf@body only contains `time_name` columns and depth
#' columns with the format
#'   <var_name>_<depth-below-surface>
#' e.g. "q_100"
#'
#' @param dlf An S4 object of class Dlf
#' @param var_name Name of variable in columns
#' @param time_name Name of time column
#' @param depth_name Name to use for new depth column
#' @return An S4 object of class Dlf
#'
#' The body of rhe returned Dlf object contains one row for each time/depth
#' combination.
#'
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "daily/DailyP/DailyP-Daily-WaterFlux.dlf")
#' dlf <- read_dlf(path)
#' dlf@body[1,]
#' dlf <- daisy_time_to_timestamp(dlf)
#' dlf <- depth_wide_to_long(dlf, "q")
#' head(dlf@body)
depth_wide_to_long <- function(dlf, var_name, time_name="time",
                               depth_name="z") {
    names <- colnames(dlf@body)
    varying <- names[startsWith(names, var_name)]
    body <- reshape(
        dlf@body, varying, direction="long", sep="_", timevar=depth_name,
        new.row.names=NULL
    )[c(depth_name, "time", var_name)]
    if (time_name != "time") {
        colnames(body)[2] <- time_name
    }
    rownames(body) <- seq(1, nrow(body))
    if (time_name %in% dlf@units) {
        time_unit <- dlf@units[time_name]
    } else {
        time_unit <- ""
    }
    units <- data.frame(z="unknown-depth-unit",
                        time=time_unit,
                        q=dlf@units[varying[1]])
    colnames(units) <- colnames(body)
    new("Dlf", header=dlf@header, units=units, body=body)
}
