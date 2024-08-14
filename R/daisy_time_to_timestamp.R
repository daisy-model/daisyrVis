#' Transform daisy time columns to a single timestamp
#'
#' @param dlf An S4 object of class Dlf or a list of Dlf objects
#' @param time_col_name The name of the new timestamp column
#' @param year_col Name of year column, set to NULL if no year column
#' @param month_col Name of month column, set to NULL if no month column
#' @param day_col Name of day column, set to NULL if no day column
#' @param hour_col Name of hour column, set to NULL if no hour column
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
daisy_time_to_timestamp <- function(dlf, time_col_name='time', year_col="year",
                                    month_col="month", day_col="mday",
                                    hour_col="hour") {
    if (is.list(dlf)) {
        lapply(dlf, function(dlf_) {
            daisy_time_to_timestamp(dlf_, time_col_name, year_col, month_col,
                                    day_col, hour_col)
        })
    } else {
        body <- dlf@body
        time_cols <- c(year_col, month_col, day_col, hour_col)
        format_string <- c()
        if (!is.null(year_col)) {
            format_string <- c(format_string, "%Y")
        }
        if (!is.null(month_col)) {
            format_string <- c(format_string, "%m")
        }
        if (!is.null(day_col)) {
            format_string <- c(format_string, "%d")
        }
        if (!is.null(hour_col)) {
            format_string <- c(format_string, "%H")
        }
        format_string <- paste(format_string, collapse=" ")
        if (!all(time_cols %in% colnames(body))) {
            stop(paste("Missing time columns.",
                       time_cols[!(time_cols %in% colnames(body))],
                       "not in", colnames(body)), sep=" ")
        }
        raw_ts <- apply(body[, time_cols], 1, function(row) {
            paste(row, collapse=" ")
        })
        body[[time_col_name]] <- as.POSIXct(raw_ts, format=format_string)
        body <- body[!(colnames(body) %in% time_cols)]
        body <- body[order(body[[time_col_name]]), , drop=FALSE]
        units <- dlf@units[!(colnames(dlf@units) %in% time_cols)]
        units[time_col_name] <- ''
        new('Dlf', header=dlf@header, units=units, body=body)
    }
}
