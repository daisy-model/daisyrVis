#' Plot one or more variables from one or more Dlfs with a user supplied geom.
#' Each variable is plotted in a separate subplot.
#' @param dlfs  Either a list of Dlf or a single Dlf. If a list with of
#'              components, the names are  used for the legend.
#' @param x_var  Name of variable for x axis
#' @param y_vars  Either a list of variables to plot, or a single variable
#' @param geom  A single parameter function calling a ggplot2 geom_* function.
#'              It is passed the plot object as the only parameter.
#' @param title_suffix  A string this is appended to the title of all subplots
#' @param x_var_is_posix_time Set to TRUE if x_var is POSIXct class, e.g if
#'                            you have used daisy_time_to_timestamp
#' @return ggplot2
#' @export
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "annual/Annual-FN/HourlyP-Annual-FN-2-2b.dlf")
#' dlf <- read_dlf(path)
#' geom <- function(gg) { gg + ggplot2::geom_col(position="dodge") }
#' plot_many(dlf, "year", "Crop", geom)
plot_many <- function(dlfs, x_var, y_vars, geom, title_suffix="",
                      x_var_is_posix_time=FALSE) {
    if (methods::is(dlfs, "Dlf")) {
        dlfs <- list(dlfs)
    }
    group_col <- "group.name"
    groups <- names(dlfs)
    if (is.null(groups)) {
        groups <- seq(1, length(dlfs))
    }
    if (x_var_is_posix_time) {
        ## If x_var is of class POSIXct/POSIXlt then we need to convert it to
        ## character or integer before using reshape. When we then plot it we
        ## get overlapping tick labels, but if we convert it back to POSIXct
        ## then the labels are handled nicely by ggplot2.
        time_to_character <- function(dlf) {
            dlf@body[[x_var]] <- as.character(dlf@body[[x_var]])
            dlf
        }
        dlfs <- lapply(dlfs, time_to_character)
    }
    x_var_sym <- rlang::sym(x_var) # This is for aes
    plotlist <- lapply(y_vars, function(y_var) {
        y_dfs <- lapply(groups, function(group) {
            df <- dlfs[[group]]@body[, c(x_var, y_var)]
            df <- stats::reshape(df,
                                 direction="long",
                                 varying=y_var,
                                 times=y_var,
                                 v.names="value",
                                 timevar="var",
                                 idvar=x_var)
            df[group_col] <- group
            df
        })
        df <- do.call("rbind", y_dfs)
        if (x_var_is_posix_time) {
            ## Convert back to POSIXct
            df[[x_var]] <- as.POSIXct(df[[x_var]])
        }
        df[[group_col]] <- factor(df[[group_col]])
        ## Use `get` in aes to avoid ckeck() note about "no visible binding"
        gg <- ggplot2::ggplot(df,
                              ggplot2::aes(x=!!x_var_sym,
                                           y=get("value"),
                                           group=get("group.name"),
                                           fill=get("group.name"),
                                           shape=get("group.name"),
                                           colour=get("group.name")))
        geom(gg) + ggplot2::labs(y = dlfs[[1]]@units[y_var],
                                 fill="Dlf",
                                 colour="Dlf",
                                 shape="Dlf",
                                 title=paste0(y_var, title_suffix))
    })
    cowplot::plot_grid(plotlist=plotlist, labels="AUTO")
}
