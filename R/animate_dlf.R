#' Animate a depth dependent timeseries from a Dlf file
#'
#' @param dlf An S4 object of class Dlf
#' @param var_name Name of variable in columns
#' @return A plotly figure
#'
#' @importFrom plotly %>%
#' @export
#'
#' @examples
#' data_dir <- system.file("extdata", package="daisyrVis")
#' path <- file.path(data_dir, "daily/DailyP/DailyP-Daily-WaterFlux.dlf")
#' dlf <- read_dlf(path)
#' animate_dlf(dlf, "q")
animate_dlf <- function(dlf, var_name) {
    dlf <- daisyrVis::daisy_time_to_timestamp(dlf)
    dlf <- daisyrVis::depth_wide_to_long(dlf, var_name)
    dlf@data %>%
        plotly::plot_ly(x=~q,
                        y=~z,
                        frame=~as.character(time),
                        type="scatter",
                        mode="markers") %>%
        plotly::animation_slider(currentvalue=list(prefix="Time"), y=1.1,
                                 yanchor="bottom") %>%
        plotly::animation_button(y=1, yanchor="top")
}
