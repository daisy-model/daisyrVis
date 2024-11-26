#' Plot one or more timepoints from one or more dlfs containing depth dependent
#' timeseries
#'
#' @param dlfs An S4 object of class Dlf or a list of Dlf objects
#' @param time_points List or vector of time points to plot. All dlfs must have
#' data for all time points
#' @param x_var Name of variable on x axis
#' @param y_var Name of variable on y axis
#' @param time_var Name of time variable
#' @param x_label Label for x axis (unit will be appended)
#' @param y_label Label for y axis (unit will be appended)
#' @param legend_name Name to use for legend
#' @param title Plot title (time point will be appended)
#' @return ggplot2 object
#'
#' @export
plot_dlf_timeseries <- function(dlfs, time_points, x_var, y_var="z",
                                time_var='time', x_label="",
                                y_label="", legend_name="dlf",
                                title="") {
    unlist_result <- FALSE
    if (methods::is(dlfs, "Dlf")) {
        dlfs <- list(dlfs)
        unlist_result <- TRUE
    }
    if (!is.list(time_points)) {
        time_points <- as.list(time_points)
    }
    x_var_sym <- rlang::sym(x_var)
    y_var_sym <- rlang::sym(y_var)
    legend_name_sym <- rlang::sym(legend_name)
    plotlist <- lapply(time_points, function(time_point) {
        dfs <- lapply(names(dlfs), function(name) {
            dlf <- dlfs[[name]]
            df <- dlf[dlf$time == time_point, ]
            df[legend_name] <- name
            df
        })
        df <- do.call("rbind", dfs)
        ggplot2::ggplot(df, ggplot2::aes(x=!!x_var_sym,
                                         y=!!y_var_sym,
                                         colour=!!legend_name_sym)) +
            ggplot2::geom_point() +
            ggplot2::labs(y=paste0(y_label, " [", dlfs[[1]]@units[y_var], "]"),
                          x=paste0(x_label, " [", dlfs[[1]]@units[x_var], "]"),
                          title=paste(title, time_point))
    })
    if (length(plotlist) > 1) {
        cowplot::plot_grid(plotlist=plotlist, labels="AUTO")
    } else {
        if (unlist_result) {
            plotlist[[1]]
        } else {
            plotlist
        }
    }
}
