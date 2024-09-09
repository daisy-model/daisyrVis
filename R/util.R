#' Filter a list of Dlfs
#'
#' @param dlfs List of Dlfs
#' @param FUN Function mapping dlf to a boolean of length 1
#' @return A list of Dlfs where FUN is TRUE
#' @export
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' dlfs <- read_dlf_dir(file.path(data_dir, "annual"))
#' names(dlfs)
#' ## We only want the dlfs that have a "Harvest" column
#' dlfs <- filter_dlfs(dlfs, function(dlf) {
#'     "Crop" %in% colnames(dlf@data)
#' })
#' names(dlfs)
## nolint start: object_name_linter
filter_dlfs <- function(dlfs, FUN) {
## nolint end
    dlfs[sapply(dlfs, function(dlf) {
        FUN(dlf)
    })]
}


#' Merge a list of Dlfs
#'
#' @param dlfs List of Dlfs
#' @param dlf_name_column Name of column to store name of dlf each row came from
#' @return A Dlf object with the data of the dlfs merged into a single dlf.
#' @export
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' dlfs <- read_dlf_dir(file.path(data_dir, "annual"))
#' dlfs <- dlfs[startsWith(names(dlfs), "Annual-FN")]
#' dlfs <- dir_names_to_columns(dlfs)
#' dlf <- merge_dlfs(dlfs)
#' head(dlf@data)
merge_dlfs <- function(dlfs, dlf_name_column="name") {
    if (!is.list(dlfs)) {
        dlfs
    } else {
        columns <- unique(lapply(dlfs, function(dlf) {
            colnames(dlf@data)
        }))
        if (length(columns) != 1) {
            stop("Cannot merge because dlfs do not have the same columns")
        }
        dlf_names <- names(dlfs)
        dlf <- dlfs[[1]]
        if (!is.null(dlf_name_column)) {
            dlf$name <- dlf_names[1]
        }
        for (i in seq.int(2, length(dlfs))) {
            df <- dlfs[[i]]@data
            if (!is.null(dlf_name_column)) {
                df[[dlf_name_column]] <- dlf_names[i]
            }
            dlf@data <- rbind(dlf@data, df)
        }
        dlf
    }
}
