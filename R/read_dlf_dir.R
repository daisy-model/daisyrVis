#' Walk a directory tree and read all dlf files in it.
#'
#' @param directory Path to directory
#' @param pattern Regex pattern of files to include
#' @return A list of S4 objects of class Dlf. Each dlf is named with the
#'         relative path to it
#'
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' dlfs <- read_dlf_dir(file.path(data_dir, "annual"))
#' print(names(dlfs))
#'
#' dlfs <- drop_dir_from_names(dlfs)
#' print(names(dlfs))
#'
#' dlfs <- strip_common_prefix_from_names(dlfs)
#' print(names(dlfs))
read_dlf_dir <- function(directory, pattern=".*\\.dlf") {
    dlfs <- list()
    for (path in list.files(directory, pattern=pattern, recursive=TRUE)) {
        name <- substr(path, 1, nchar(path) - 4)
        dlfs[[name]] <- daisyrVis::read_dlf(file.path(directory, path))
    }
    dlfs
}

#' Remove directory part from dlf names
#'
#' @param dlfs List of Dlf objects
#' @export
#' @return List of S4 objects of class Dlf with dir part stripped from names
drop_dir_from_names <- function(dlfs) {
    names(dlfs) <- basename(names(dlfs))
    dlfs
}

#' Strip common prefix from dlf names
#'
#' @param dlfs List of Dlf objects
#' @export
#' @return List of Dlf objects with common prefix stripped from names
strip_common_prefix_from_names <- function(dlfs) {
    strings <- names(dlfs)
    for (i in 1:(min(nchar(strings)) - 1)) {
        if (length(unique(substr(strings, i, i))) > 1) {
            if (i > 1) {
                strings <- substring(strings, i)
            }
            break
        }
    }
    names(dlfs) <- strings
    dlfs
}

#' Remove directory part of dlf name and store it in the data slot
#'
#' @param dlfs  List of dlfs
#' @param column_name_prefix Prefix to use for storing directory structure
#' @return A list of S4 objects of class Dlf. Each dlf is named with the
#'         basename of its original name
#' @export

#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' dlfs <- read_dlf_dir(file.path(data_dir, "annual"))
#' names(dlfs)
#' dlfs <- dir_names_to_columns(dlfs)
#' lapply(dlfs, function(dlf) {
#'     dlf@data[1, "dir"]
#' })
dir_names_to_columns <- function(dlfs, column_name_prefix=NULL) {
    if (!is.list(dlfs)) {
        stop("Single dlf is unnamed")
    }
    files <- basename(names(dlfs))
    dirs <- dirname(names(dlfs))
    if (is.null(column_name_prefix)) {
        column_name_prefix <- "dir"
    }
    dlfs <- lapply(seq_len(length(dirs)), function(idx) {
        sub_dirs <- strsplit(dirs[[idx]], "/", fixed=TRUE)
        num_sub_dirs <- length(sub_dirs)
        if (num_sub_dirs > 1) {
            names(sub_dirs) <- paste0(column_name_prefix, seq_len(num_sub_dirs))
        } else {
            names(sub_dirs) <- column_name_prefix
        }
        unit_cols <- data.frame(rep("", length(sub_dirs)))
        names(unit_cols) <- names(sub_dirs)
        dlf <- dlfs[[idx]]
        dlf@data <- cbind(dlf@data, sub_dirs)
        dlf@units <- cbind(dlf@units, unit_cols)
        dlf
    })
    names(dlfs) <- files
    dlfs
}

#' Walk a directory tree generated by daisy spawn, read all dlf files in it,
#' and merge similar log files into one dlf
#' #'
#' @param directory Path to directory
#' @param pattern Regex pattern of files to include
#' @return A list of S4 objects of class Dlf. Each dlf is named with the
#'         log type
#' @export
read_daisy_spawn_output <- function(directory, pattern=".*\\.dlf") {
    dlfs <- read_dlf_dir(directory, pattern)
    dlf_names <- basename(names(dlfs))
    lapply(unique(dlf_names), function(log_name) {
        daisyrVis::merge_dlfs(dir_names_to_columns(dlfs[log_name == dlf_names],
                                                   "run"), NULL)
    })
}
