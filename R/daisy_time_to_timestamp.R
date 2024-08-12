#' Transform daisy time columns to a single timestamp
#'
#' @param dlf An S4 object of class Dlf or a list of Dlf objects
#' @param time_col_name The name of the new timestamp column
#' @return An S4 object of class Dlf
#'
#' The returned object is identical to the dlf, except that the Columns 'year',
#' 'month', 'mday' and 'hour' are dropped and replaced with a column containing
#' the corresponding POSIXct timestamp
#'
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, 'daily/DailyP/DailyP-Daily-WaterFlux.dlf')
#' dlf <- read_dlf(path)
#' dlf@body[1,]
#' dlf <- daisy_time_to_timestamp(dlf)
#' dlf@body[1,]
daisy_time_to_timestamp <- function(dlf, time_col_name='time') {
    if (is.list(dlf)) {
        lapply(dlf, function(dlf_) {
            daisy_time_to_timestamp(dlf_, time_col_name)
        })
    } else {
        body <- dlf@body
        raw_ts <- paste(body$year, body$month, body$mday, body$hour)
        body[[time_col_name]] <- as.POSIXct(raw_ts, format="%Y %m %d %H")
        time_cols <- c('year', 'month', 'mday', 'hour')
        body <- body[!(colnames(body) %in% time_cols)]
        body <- body[order(body[[time_col_name]]), , drop=FALSE]
        units <- dlf@units[!(colnames(dlf@units) %in% time_cols)]
        units[time_col_name] <- ''
        new('Dlf', header=dlf@header, units=units, body=body)
    }
}
