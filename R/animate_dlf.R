#' Animate a depth dependent timeseries from a Dlf file
#'
#' @param dlf An S4 object of class Dlf
#' @param x_var Name of variable on x axis. If NULL assume it is the column that
#' is not y_var or time_var
#' @param y_var Name of variable on y axis
#' @param time_var Name of time variable
#' @return A plotly figure
#'
#' @importFrom plotly %>%
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "daily/DailyP/DailyP-Daily-WaterFlux.dlf")
#' dlf <- read_dlf(path)
#' animate_dlf(dlf, "q")
animate_dlf <- function(dlf, x_var=NULL, y_var="z", time_var="time") {
    if (is.null(x_var)) {
        columns <- colnames(dlf@data)
        x_var <- columns[!columns %in% c(y_var, time_var)][1]
    }
    dlf@data %>%
        plotly::plot_ly(x=as.formula(paste0("~", x_var)),
                        y=as.formula(paste0("~", y_var)),
                        frame=as.formula(
                            paste0("~as.character(", time_var, ")")
                        ),
                        type="scatter",
                        mode="markers") %>%
        plotly::animation_slider(currentvalue=list(prefix="Time"), y=1.1,
                                 yanchor="bottom") %>%
        plotly::animation_button(y=1, yanchor="top")
}
