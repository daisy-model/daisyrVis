#' Transform daisy time columns to a single timestamp
#'
#' @param dlf An S4 object of class Dlf
#' @param time_col_name The name of the new timestamp column
#' @return An S4 object of class Dlf
#'
#' The returned object is identical to the dlf, except that the Columns 'year',
#' 'month', 'mday' and 'hour' are dropped and replaced with a column containing
#' the corresponding POSIXlt timestamp
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
    body <- dlf@body
    raw_ts <- paste(body$year, body$month, body$mday, body$hour)
    body[[time_col_name]] <- strptime(raw_ts, format="%Y %m %d %H")
    time_cols <- c('year', 'month', 'mday', 'hour')
    body <- body[!(colnames(body) %in% time_cols)]
    body <- body[order(body[[time_col_name]]), , drop=FALSE]
    units <- dlf@units[!(colnames(dlf@units) %in% time_cols)]
    units['time'] <- ''
    new('Dlf', header=dlf@header, units=units, body=body)
}
