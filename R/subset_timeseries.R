#' Select the subset of measurements in a specific time range
#' @param dlfs  Either a list of Dlf or a single Dlf
#' @param date_from Start date (included). ISO 8601 format (yyyy-mm-dd),
#' e.g 1999-01-31
#' @param date_to End date (included). ISO 8601 format (yyyy-mm-dd),
#' e.g 1999-01-31
#' @param time_from Start time (included). ISO 8601 format (hh:mm:ss),
#' e.g. 07:54:20
#' @param time_to End time (included). ISO 8601 format (hh:mm:ss),
#' e.g. 07:54:20
#' @param time_zone Name of time zone.
#' @param time_col Name of time column in dlfs.
#' @return Dlfs with data slot containing the selected rows
#' @export
#' @examples
#' # Load dlf data with read_dlf
#' data_dir <- file.path(system.file("extdata", package="daisyrVis"),
#'                       "annual/Annual-FN")
#' dlfs <- read_dlf_dir(data_dir)
#' dlfs <- daisy_time_to_timestamp(dlfs)
#' data.frame(lapply(dlfs, function(dlf) { dlf$time}))
#' dlfs <- subset_timeseries(dlfs, "1995-04-01", "1999-04-01", time_zone="CEST")
#' data.frame(lapply(dlfs, function(dlf) { dlf$time}))
subset_timeseries <- function(dlfs, date_from, date_to,
                              time_from="00:00:00", time_to="23:59:59",
                              time_zone="UTC", time_col="time") {

    from <- as.POSIXct(paste(date_from, time_from), tz=time_zone,
                       format="%Y-%m-%d %H:%M:%OS")
    to <- as.POSIXct(paste(date_to, time_to), tz=time_zone,
                     format="%Y-%m-%d %H:%M:%OS")
    subset_ <- function(dlf) {
        dlf@data <- dlf@data[from <= dlf[[time_col]] & dlf[[time_col]] <= to, ]
        dlf
    }
    if (is.list(dlfs)) {
        lapply(dlfs, subset_)
    } else {
        subset_(dlfs)
    }
}
