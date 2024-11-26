#' Transform daisy time columns to a single timestamp
#'
#' @param dlf An S4 object of class Dlf or a list of Dlf objects
#' @param time_col_name The name of the new timestamp column
#' @param year_col Name of year column, set to NULL if no year column
#' @param month_col Name of month column, set to NULL if no month column
#' @param day_col Name of day column, set to NULL if no day column
#' @param hour_col Name of hour column, set to NULL if no hour column
#' @param drop_daisy_time_cols If TRUE drop the year, month, day, hour columns
#' @return An S4 object of class Dlf
#'
#' If all of year_col, month_col, day_col and hour_col are NULL, then an attempt
#' is made to guess the relevant names.
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
#' dlf@data[1,]
#' dlf <- daisy_time_to_timestamp(dlf)
#' dlf@data[1,]
daisy_time_to_timestamp <- function(dlf, time_col_name='time', year_col=NULL,
                                    month_col=NULL, day_col=NULL, hour_col=NULL,
                                    drop_daisy_time_cols=FALSE) {
    if (is.list(dlf)) {
        lapply(dlf, function(dlf_) {
            daisy_time_to_timestamp(dlf_, time_col_name, year_col, month_col,
                                    day_col, hour_col, drop_daisy_time_cols)
        })
    } else {
        format_strings <- c(year="%Y", month="%m", day="%d", hour="%H")
        data <- dlf@data
        time_cols <- c(year=year_col, month=month_col, day=day_col,
                       hour=hour_col)
        if (is.null(time_cols)) {
            time_cols <- guess_time_cols(colnames(data))
        }
        format_string <- paste(format_strings[names(time_cols)], collapse=" ")
        if (!all(time_cols %in% colnames(data))) {
            stop(paste0("Missing time columns: '",
                        paste(time_cols[!(time_cols %in% colnames(data))],
                              collapse=" "),
                        "' not in '",
                        paste(colnames(data), collapse=" "), "'"))
        }
        raw_ts <- apply(data[, time_cols], 1, function(row) {
            paste(row, collapse=" ")
        })
        data[[time_col_name]] <- as.POSIXct(raw_ts, format=format_string)
        if (drop_daisy_time_cols) {
            data <- data[!(colnames(data) %in% time_cols)]
        }
        data <- data[order(data[[time_col_name]]), , drop=FALSE]
        units <- dlf@units[!(colnames(dlf@units) %in% time_cols)]
        units[time_col_name] <- ''
        new('Dlf', header=dlf@header, units=units, data=data)
    }
}

guess_time_cols <- function(column_names) {
    time_cols <- list()
    candidates <- list(year=c("year"),
                       month=c("month"),
                       day=c("mday", "day"),
                       hour=c("hour"))
    for (key in names(candidates)) {
        for (candidate in candidates[[key]]) {
            if (candidate %in% column_names) {
                time_cols[[key]] <- candidate
                break
            }
        }
    }
    unlist(time_cols)
}
