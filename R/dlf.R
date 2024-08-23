## nolint start: object_name_linter
#' Class encapsulating Daisy log file (dlf) data
#' @slot header  A list with named components containing meta information
#' @slot units  A data.frame with the units of the values in data
#' @slot data  A data.frame with the values from the dlf file
#' @export Dlf
Dlf <- setClass("Dlf", slots=list(header="list",
                                  units="data.frame",
                                  data="data.frame"))
## nolint end

#' $ indexing on the data part of the dlf
#' @param x The dlf object
#' @param name Name of column in data part
#' @export
setMethod("$", "Dlf", function(x, name) {
    x@data[[name, exact=FALSE]]
})

#' $ assignment on the data part of the dlf
#' @param x The dlf object
#' @param name Name of column in data part
#' @param value New value to assign
#' @export
setMethod("$<-", "Dlf", function(x, name, value) {
    x@data[[name]] <- value
    x
})

#' [[ indexing on the data part of the dlf
#' @param x The dlf object
#' @param i Name or index of row OR if j is missing, name or index of column
#' @param j Name or index of column
#' @export
setMethod("[[", "Dlf", function(x, i, j) {
    if (missing(j)) {
        x@data[[i]]
    } else {
        x@data[[i, j]]
    }
})

#' [[ assignemnt on the data part of the dlf
#' @param x The dlf object
#' @param i Name or index of row OR if j is missing, name or index of column
#' @param j Name or index of column
#' @param value New value to assign
#' @export
setMethod("[[<-", "Dlf", function(x, i, j, value) {
    if (missing(j)) {
        x@data[[i]] <- value
    } else {
        x@data[[i, j]] <- value
    }
    x
})
