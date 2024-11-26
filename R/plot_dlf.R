#' Plot one or more variables from one or more Dlfs.
#' Each variable is plotted in a separate subplot.
#' @param dlfs  Either a list of Dlf or a single Dlf. If a list with of
#'              components, the names are  used for the legend.
#' @param x_var  Name of variable for x axis
#' @param y_vars  Either a list of variables to plot, or a single variable
#' @param type  Either a string defining the type of plot to produce OR a
#' single parameter function returning a ggplot2 object.
#'
#' If a string, the the following types are supported
#'      * "bar"
#'      * "points"
#'      * "lines"
#' Types cannot be abbreviated, but they can be combined as in
#'     type="barpoints"
#' The types can be separated by a non-type string for readability as in
#'     type="bar + lines + points"
#'
#' If a function, then it is passed a ggplot2 object and it should add a geom to
#' the object and return it. For example,
#'     type=function(gg) { gg + geom_point() }
#' @param title_suffix  A string that is appended to the title of all subplots
#' @param group_col Name of column to use for grouping. If not NULL then Dlfs
#' must be a single Dlf object, not a list
#' @return A ggplot2 object. If y_vars is a single variable then the plot can be
#'         themed and further data added. If y_vars is a list of more than on
#'         variable, it is not possible to add further data to it.
#' @export
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "annual/Annual-FN/HourlyP-Annual-FN-2-2b.dlf")
#' dlf <- read_dlf(path)
#' # Plot using a string for type
#' plot_dlf(dlf, "year", "Crop", "bar")
#' # Same plot with a function for type
#' geom <- function(gg) { gg + ggplot2::geom_col(position="dodge") }
#' plot_dlf(dlf, "year", "Crop", geom)
plot_dlf <- function(dlfs, x_var, y_vars, type, title_suffix="",
                     group_col=NULL) {
    if (is.character(type)) {
        geom <- get_geom_from_type(type)
    }
    if (!is.null(group_col) && is.list(dlfs)) {
        stop("Cannot specify `group_col` when passing a list of dlfs")
    }

    ## Get the data that we are goint to plot
    if (is.null(group_col)) {
        if (!is.list(dlfs)) {
            dlfs <- list(dlfs)
        }
        group_name <- "Dlf"
        group_col <- "group.name"
        groups <- names(dlfs)
        if (is.null(groups)) {
            groups <- seq(1, length(dlfs))
        }
        df <- do.call("rbind", lapply(groups, function(group) {
            df_ <- dlfs[[group]]@data[, c(x_var, y_vars)]
            df_[group_col] <- group
            df_
        }))
        df[[group_col]] <- factor(df[[group_col]])
        units <- dlfs[[1]]@units
    } else {
        group_name <- group_col
        df <- dlfs@data[, c(x_var, y_vars, group_col)]
        units <- dlfs@units
    }
    group_col_sym <- rlang::sym(group_col)
    x_var_sym <- rlang::sym(x_var) # This is for aes
    plotlist <- lapply(y_vars, function(y_var) {
        y_var_sym <- rlang::sym(y_var) # This is for aes
        gg <- ggplot2::ggplot(df,
                              ggplot2::aes(x=!!x_var_sym,
                                           y=!!y_var_sym,
                                           group=!!group_col_sym,
                                           fill=!!group_col_sym,
                                           shape=!!group_col_sym,
                                           colour=!!group_col_sym))
        geom(gg) + ggplot2::labs(y=paste0(y_var, format_unit(units[y_var])),
                                 x=paste0(x_var, format_unit(units[x_var])),
                                 fill=group_name,
                                 colour=group_name,
                                 shape=group_name,
                                 title=paste0(y_var, title_suffix))
    })
    if (length(plotlist) > 1) {
        cowplot::plot_grid(plotlist=plotlist, labels="AUTO")
    } else {
        plotlist
    }
}

format_unit <- function(unit) {
    if (unit != "") {
        paste0(" [", unit, "]")
    }
}

get_geom_from_type <- function(type) {
    geoms <- list()
    type <- tolower(type)
    if (grepl("bar", type, fixed=TRUE)) {
        geoms <- append(geoms, function(gg) {
            gg + ggplot2::geom_col(position="dodge") +
                ggplot2::guides(colour="none")
        })
    }
    if (grepl("points", type, fixed=TRUE)) {
        geoms <- append(geoms, function(gg) {
            gg + ggplot2::geom_point() +
                ggplot2::guides(fill="none")
        })
    }
    if (grepl("lines", type, fixed=TRUE)) {
        geoms <- append(geoms, function(gg) {
            gg + ggplot2::geom_line() +
                ggplot2::guides(fill="none")
        })
    }
    if (length(geoms) == 0) {
        stop("Unknown plot type", type)
    }
    function(gg) {
        for (f in geoms) {
            gg <- f(gg)
        }
        gg
    }
}
