#' Transform wide format time series to a long format time series
#' It is assumed that dlf@data only contains `time_name` columns and depth
#' columns with the format
#'   <var_name>_<depth-below-surface>
#' e.g. "q_100"
#'
#' @param dlf An S4 object of class Dlf or a list of Dlf objects
#' @param var_name Name of variable in columns
#' @param time_name Name of time column
#' @param depth_name Name to use for new depth column
#' @param depth_unit Unit of depth
#' @return An S4 object of class Dlf
#'
#' The data of rhe returned Dlf object contains one row for each time/depth
#' combination.
#'
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "daily/DailyP/DailyP-Daily-WaterFlux.dlf")
#' dlf <- read_dlf(path, convert_depth=FALSE)
#' dlf@data[1,]
#' dlf <- depth_wide_to_long(dlf, "q")
#' head(dlf@data)
depth_wide_to_long <- function(dlf, var_name, time_name="time",
                               depth_name="z", depth_unit="unknown") {
    if (is.list(dlf)) {
        lapply(dlf, function(dlf_) {
            depth_wide_to_long(dlf_, var_name, time_name, depth_name)
        })
    } else {
        names <- colnames(dlf@data)
        varying <- names[startsWith(names, var_name)]
        data <- reshape(
            dlf@data, varying, direction="long", sep="..AT..",
            timevar=depth_name, new.row.names=NULL
        )[c(depth_name, "time", var_name)]
        data[, depth_name] <- as.numeric(gsub("_", "-", data[, depth_name]))
        if (time_name != "time") {
            colnames(data)[2] <- time_name
        }
        rownames(data) <- seq(1, nrow(data))
        if (time_name %in% dlf@units) {
            time_unit <- dlf@units[time_name]
        } else {
            time_unit <- ""
        }
        units <- data.frame(z=depth_unit,
                            time=time_unit,
                            q=dlf@units[varying[1]])
        colnames(units) <- colnames(data)
        new("Dlf", header=dlf@header, units=units, data=data)
    }
}
