#' Calculate mass balance
#' @param dlfs  Either a list of Dlf or a single Dlf
#' @param input  Name(s) of variable(s) containing mass input
#' @param output  Name(s) of variable(s) containing mass output
#' @param content  Name(s) of variable(s) containing mass content
#' @param use_initial_content_as_reference If TRUE subtract the initial content
#' from the content sum before calculating balance.
#' @return A list of Dlf or a single Dlf. Four variables are added to each Dlf,
#'         input_sum, output_sum, content_sum, and balance, which hold the sum
#'         and balance of the input/output/content variables calculated for each
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
# nolint start
mass_balance <- function(dlfs, input, output, content,
                         use_initial_content_as_reference=TRUE) {
# nolint end
    if (is.list(dlfs)) {
        lapply(dlfs, function(dlf) {
            mass_balance(dlf, input, output, content)
        })
    } else {
        unit <- unique(unlist(dlfs@units[, c(input, output, content)]))
        if (length(unit) > 1) {
            stop(paste("Unit mismatch:", unit))
        }
        data <- dlfs@data
        input_sum <- cumsum(row_sum(data[, input]))
        output_sum <- cumsum(row_sum(data[, output]))
        content_sum <- row_sum(data[, content])
        if (use_initial_content_as_reference) {
            content_sum <- content_sum - sum(data[1, content])
        }
        balance <- content_sum + output_sum - input_sum
        data <- cbind(data, data.frame(input_sum=input_sum,
                                       output_sum=output_sum,
                                       content_sum=content_sum,
                                       balance=balance))
        units <- cbind(dlfs@units, data.frame(input_sum=unit,
                                              output_sum=unit,
                                              content_sum=unit,
                                              balance=unit))
        new("Dlf", header=dlfs@header, units=units, data=data)
    }
}

#' Plot Dlf object with mass balance calculation in a control chart
#' @param dlfs  Either a list of Dlf or a single Dlf. If a list of Dlfs each
#' Dlf is plotted in a separate subplot.
#' @param x_var  Name of variable for x axis
#' @param title_suffix A string that is appended to the title of all subplots
#' @return A ggplot2 object.
#'
#' @export
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "hourly/P2D-Daily-Soil_Chemical_110cm.dlf")
#' dlf <- read_dlf(path)
#' input <- c("In_Matrix", "In_Biopores", "External", "Transform", "Tillage")
#' output <- c("Decompose", "Leak_Matrix", "Leak_Biopores", "Drain_Soil",
#'             "Drain_Biopores", "Uptake")
#' content <- c("Content", "Biopores")
#' mb <- mass_balance(dlf, input, output, content)
#' mb <- daisy_time_to_timestamp(mb)
#' plot_mass_balance(mb, "time", " - Soil chemical @ 110cm")
plot_mass_balance <- function(dlfs, x_var, title_suffix="") {
    if (methods::is(dlfs, "Dlf")) {
        dlfs <- list(dlfs)
    }
    plotlist <- lapply(dlfs, function(dlf) {
        df <- dlf@data[, c(x_var, 'balance')]
        sample_mean <- mean(df$balance)
        sample_sd <- stats::sd(df$balance)
        cl2 <- sample_mean + 2 * c(-sample_sd, sample_sd)
        cl3 <- sample_mean + 3 * c(-sample_sd, sample_sd)
        x_var_sym <- rlang::sym(x_var) # This is for aes
        gg <- ggplot2::ggplot(df,
                              ggplot2::aes(x=!!x_var_sym, y=get("balance"))) +
            ggplot2::geom_point(shape=3, alpha=0.5) +
            ggplot2::geom_hline(yintercept=sample_mean, linetype='dashed') +
            ggplot2::geom_hline(yintercept=cl2, colour='orange',
                                linetype='dashed') +
            ggplot2::geom_hline(yintercept=cl3, colour='red',
                                linetype='dashed') +
            ggplot2::labs(y="Balance",
                          fill="Dlf",
                          colour="Dlf",
                          shape="Dlf",
                          title=paste0("Mass balance", title_suffix))
        gg
    })
    if (length(plotlist) > 1) {
        cowplot::plot_grid(plotlist=plotlist, labels="AUTO")
    } else {
        plotlist[[1]]
    }
}

#' Mass balance summary for a set of specified variables
#' @param dlfs  Either a list of Dlf or a single Dlf
#' @param input  Name(s) of variable(s) containing mass input
#' @param output  Name(s) of variable(s) containing mass output
#' @param content  Name(s) of variable(s) containing mass content
#' @return Either a single list or a list of lists, with the nested lists having
#'         five elements: Inputs, Outputs, InitialContent, FinalContent, and
#'         Balance.
#' @export
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "hourly/P2D-Daily-Soil_Chemical_110cm.dlf")
#' dlf <- read_dlf(path)
#' input <- c("In_Matrix", "In_Biopores", "External", "Transform", "Tillage")
#' output <- c("Decompose", "Leak_Matrix", "Leak_Biopores", "Drain_Soil",
#'             "Drain_Biopores", "Uptake")
#' content <- c("Content", "Biopores")
#' mass_balance_summary(dlf, input, output, content)
mass_balance_summary <- function(dlfs, input, output, content) {
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
        input_sum <- col_sum(dlfs@data[, input])
        input_total <- sum(input_sum)
        result$Inputs <- c(input_sum, Total=input_total)
        output_sum <- col_sum(dlfs@data[, output])
        output_total <- sum(output_sum)
        result$Outputs <- c(output_sum, Total=output_total)
        in_out_delta <- output_total - input_total
        initial_content <- dlfs@data[1, content]
        initial_content_total <- sum(initial_content)
        final_content <- dlfs@data[nrow(dlfs@data), content]
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
