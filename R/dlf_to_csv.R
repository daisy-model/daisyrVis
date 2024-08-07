#' Write a dlf object as a csv file
#'
#' @param dlf Dlf object to write
#' @param path Output path to write to
#' @param sep Separator to use in csv file
#' @param include_dlf_header If TRUE write the Dlf to the begining of the csv
#'                           file
#'
#' @export
dlf_to_csv <- function(dlf, path, sep=",", include_dlf_header=FALSE) {
    if (include_dlf_header) {
        header <- data.frame(unname(unlist(dlf@header)))
        rownames(header) <- names(dlf@header)
        utils::write.table(header, path, sep=sep, row.names=TRUE,
                           col.names=FALSE, fileEncoding="UTF-8")
    }
    utils::write.table(dlf@units, path, append=include_dlf_header, sep=sep,
                       row.names=FALSE, fileEncoding="UTF-8")
    utils::write.table(dlf@body, path, append=TRUE, sep=sep, row.names=FALSE,
                       col.names=FALSE, fileEncoding="UTF-8")
}
