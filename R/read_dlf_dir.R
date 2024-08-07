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
        dlfs[[name]] <- read_dlf(file.path(directory, path))
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
