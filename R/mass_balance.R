#' Calculate mass balance for a set of specified variables
#' @param dlfs  Either a list of Dlf or a single Dlf
#' @param input  Name(s) of variable(s) containing mass input
#' @param output  Name(s) of variable(s) containing mass output
#' @param content  Name(s) of variable(s) containing mass content
#' @return A list of Dlf or a single Dlf. Four variables are added to each Dlf,
#'         inout_sum, outout_sum, content_sum, and balance, which hold the sum
#'         and balance of the inout/output/content variables calculated for each
#'         time point.
#' @export
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "hourly/P2D-Daily-Soil_Chemical_110cm.dlf")
#' dlf <- read_dlf(path)
#' input <- c("In_Matrix", "In_Biopores", "External", "Transform", "Tillage")
#' output <- c("Decompose", "Leak_Matrix", "Leak_Biopores", "Drain_Soil",
#'             "Drain_Biopores", "Uptake")
#' content <- c("Content", "Biopores")
#' dlf <- mass_balance(dlf, input, output, content)
#' dlf <- daisy_time_to_timestamp(dlf)
#' points_and_lines(dlf, "time",
#'                  c("input_sum", "output_sum", "content_sum", "balance"))
mass_balance <- function(dlfs, input, output, content) {
    if (is.list(dlfs)) {
        lapply(dlfs, function(dlf) {
            mass_balance(dlf, input, output, content)
        })
    } else {
        unit <- unique(unlist(dlfs@units[, c(input, output, content)]))
        if (length(unit) > 1) {
            stop(paste("Unit mismatch:", unit))
        }
        body <- dlfs@body
        input_sum <- cumsum(row_sum(dlfs@body[, input]))
        output_sum <- cumsum(row_sum(dlfs@body[, output]))
        content_sum <- row_sum(dlfs@body[, content])
        balance <- content_sum - (input_sum - output_sum)
        body <- cbind(body, data.frame(input_sum=input_sum,
                                       output_sum=output_sum,
                                       content_sum=content_sum,
                                       balance=balance))
        units <- cbind(dlfs@units, data.frame(input_sum=unit,
                                              output_sum=unit,
                                              content_sum=unit,
                                              balance=unit))
        new("Dlf", header=dlfs@header, units=units, body=body)
    }
}

#' @export
mass_balance_table <- function(dlfs, input, output, content) {
    if (is.list(dlfs)) {
        lapply(dlfs, function(dlf) {
            mass_balance(dlf, input, output, content)
        })
    } else {
        unit <- unique(unlist(dlfs@units[, c(input, output, content)]))
        if (length(unit) > 1) {
            stop(paste("Unit mismatch:", unit))
        }
        result <- list()
        input_sum <- col_sum(dlfs@body[, input])
        input_total <- sum(input_sum)
        result$Inputs <- c(input_sum, Total=input_total)
        output_sum <- col_sum(dlfs@body[, output])
        output_total <- sum(output_sum)
        result$Outputs <- c(output_sum, Total=output_total)
        in_out_delta <- output_total - input_total
        initial_content <- dlfs@body[1, content]
        initial_content_total <- sum(initial_content)
        final_content <- dlfs@body[nrow(dlfs@body), content]
        final_content_total <- sum(final_content)
        content_delta <- final_content_total - initial_content_total
        result$InitialContent <- as.data.frame(c(initial_content,
                                                 Total=initial_content_total))
        result$FinalContent <- as.data.frame(c(final_content,
                                               Total=final_content_total))
        result$Balance <- data.frame(Input=input_total,
                                     Output=output_total,
                                     InOutChange=in_out_delta,
                                     InitialContent=initial_content_total,
                                     FinalContent=final_content_total,
                                     ContentChange=content_delta,
                                     Balance=content_delta + in_out_delta)
        result
    }
}

row_sum <- function(df) {
    if (ncol(df) < 2) {
        df
    } else {
        rowSums(df)
    }
}

col_sum <- function(df) {
    if (ncol(df) < 2) {
        sum(df)
    } else {
        colSums(df)
    }
}
