#' Class encapsulating Daisy log file (dlf) data
#' @slot header  A list with named components containing meta information
#' @slot units  A data.frame with the units of the values in body
#' @slot body  A data.frame with the values from the dlf file
#' @export Dlf
Dlf <- setClass("Dlf", slots=list(header='list', units='data.frame', body='data.frame'))

#' Read a dlf file (Daisy log file)
#'
#' @param path Path to dlf file
#'
#' @return An S4 object of class Dlf with three slots: header, units, body
#'
#' header is list with everything in the dlf file before the data. key: value mappings
#' from the header are stored as named components of the list.
#' units is a data.framecontaining the units of the values in body.
#' body is a data.frame containing the logged values
#'
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, 'annual/Annual-FN/HourlyP-Annual-FN-2-2b.dlf')
#' dlf <- read_dlf(path)
#' slotNames(dlf)
#' names(dlf@header)
#' dlf@units$Crop
#' dlf@body['Crop']
read_dlf <- function(path) {
    header_body_sep <- '--------------------'
    dlf_file <- file(path, open='rt', encoding='UTF-8')
    header <- list()
    tryCatch({
        while (TRUE) {
            line <- readLines(dlf_file, n=1, encoding='UTF-8')
            if (length(line) == 0 || startsWith(line, header_body_sep)) {
                break
            }
            line <- trimws(line)
            if (nchar(line) > 0) {
                if (startsWith(line, 'dlf')) {
                    header[['info']] = line
                }
                else {
                    kv_pair <- strsplit(line, ':', fixed=TRUE)[[1]]
                    k <- trimws(kv_pair[1])
                    v <- trimws(kv_pair[2])
                    if (is.na(v)) {
                        v <- ""
                    }
                    if (k %in% names(header)) {
                        header[k] <- paste(header[k], v, sep='\n')
                    }
                    else {
                        header[k] = v
                    }
                }
            }
        }
        csv_header <- readLines(dlf_file, n=1, encoding='UTF-8')[[1]]
        csv_header <- strsplit(trimws(csv_header, whitespace='\n'), '\t')[[1]]
        units <- readLines(dlf_file, n=1, encoding='UTF-8')[[1]]
        units <- strsplit(trimws(units, whitespace='\n'), '\t')[[1]]
        names(units) <- csv_header
        units <- as.data.frame(as.list(units)) # We make it a data.frame so names match with body
        
        body <- utils::read.table(dlf_file, sep='\t', col.names=csv_header)
        new('Dlf', header=header, units=units, body=body)
    },
    finally = {
        cat('Closing connection\n')
        close(dlf_file)
    })
}
