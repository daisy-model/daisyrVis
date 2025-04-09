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
            depth_wide_to_long(
                dlf_, var_name, time_name, depth_name, depth_unit
            )
        })
    } else {
        df <- dlf@data
        ## Find the depth columns. Then convert _ to - in column names so we can
        ## convert to depth values with as.double
        sep <- "..AT.."
        prefix <- paste0(var_name, sep)
        names <- colnames(df)
        varying_idx <- startsWith(names, prefix)
        ## We need the unit associated with the variable later
        var_unit <- dlf@units[names[varying_idx][1]]
        varying <- gsub('_', '-', names[varying_idx])
        colnames(df)[varying_idx] <- varying
        ## Reshape the data. Base reshape is slow and tidyr::pivot_longer does
        ## what we want out of the box
        data <- tidyr::pivot_longer(df, varying, names_to=depth_name,
                                    values_to=var_name, names_prefix=prefix,
                                    names_transform=as.double)
        ## The result from tidyr is a tibble. We should consider switching to
        ## tibble instead of data.frame, but for now we convert it to a
        ## data.frame
        data <- data.frame(data)

        ## Figure out the units
        col_names_to_keep <- names[!varying_idx]
        units <- data.frame(c(dlf@units[col_names_to_keep],
                              depth=depth_unit,
                              var=var_unit))
        colnames(units) <- colnames(data)
        new("Dlf", header=dlf@header, units=units, data=data)
    }
}
