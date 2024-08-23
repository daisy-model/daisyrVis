#' Read a dlf file (Daisy log file)
#'
#' @param path Path to dlf file
#'
#' @return An S4 object of class Dlf with three slots: header, units, data
#'
#' header is list with everything in the dlf file before the data. key: value
#' pairs from the header are stored as named components of the list.
#' units is a data.framecontaining the units of the values in data.
#' data is a data.frame containing the logged values
#'
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "annual/Annual-FN/HourlyP-Annual-FN-2-2b.dlf")
#' dlf <- read_dlf(path)
#' slotNames(dlf)
#' names(dlf@header)
#' dlf@units$Crop
#' dlf$Crop # equivalent to dlf@data$Crop
#' dlf[["Crop"]]
read_dlf <- function(path) {
    header_data_sep <- "--------------------"
    dlf_file <- file(path, open="rt", encoding="UTF-8")
    header <- list()
    tryCatch({
        while (TRUE) {
            line <- readLines(dlf_file, n=1, encoding="UTF-8")
            if (length(line) == 0 || startsWith(line, header_data_sep)) {
                break
            }
            line <- trimws(line)
            if (nchar(line) > 0) {
                if (startsWith(line, "dlf")) {
                    header[["info"]] <- line
                } else {
                    kv_pair <- strsplit(line, ":", fixed=TRUE)[[1]]
                    k <- trimws(kv_pair[1])
                    v <- trimws(kv_pair[2])
                    if (is.na(v)) {
                        v <- ""
                    }
                    if (k %in% names(header)) {
                        header[k] <- paste(header[k], v, sep="\n")
                    } else {
                        header[k] <- v
                    }
                }
            }
        }
        csv_header <- readLines(dlf_file, n=1, encoding="UTF-8")[[1]]
        csv_header <- strsplit(trimws(csv_header, whitespace="\n"), "\t")[[1]]
        csv_header <- gsub("-", "_", gsub(" @ ", "..AT..", csv_header))
        units <- readLines(dlf_file, n=1, encoding="UTF-8")[[1]]
        units <- strsplit(units, "\t")[[1]]
        missing_units <- length(csv_header) - length(units)
        if (missing_units > 0) {
            units <- c(units, rep("", missing_units))
        }
        names(units) <- csv_header
        # Make units a data.frame so names match data (e.g. - removed)
        units <- as.data.frame(as.list(units))

        data <- utils::read.table(dlf_file, sep="\t", col.names=csv_header)
        new("Dlf", header=header, units=units, data=data)
    },
    finally = {
        close(dlf_file)
    })
}
