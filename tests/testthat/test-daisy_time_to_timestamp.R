test_that("daisy_time_to_timestamp", {
    header <- list()
    units <- data.frame(year=c(''), month=c(''), mday=c(''), hour=c(''))
    data <- data.frame(year=c(2012), month=c(3), mday=c(4), hour=c(5))
    dlf <- new('Dlf', header=header, units=units, data=data)
    dlf2 <- daisy_time_to_timestamp(dlf, 'time', drop_daisy_time_cols=TRUE)
    expect_equal(dlf@header, dlf2@header)
    expect_equal(colnames(dlf2@data), c('time'))
    expect_equal(colnames(dlf2@units), c('time'))
    expect_equal(nrow(dlf2@data), 1)
    expect_equal(strftime(dlf2$time[1], "%Y%m%d%H%M%S"), '20120304050000')

    dlf3 <- daisy_time_to_timestamp(dlf, 'time')
    expect_equal(colnames(dlf3@data),
                 c('year', 'month', 'mday', 'hour', 'time'))
})
