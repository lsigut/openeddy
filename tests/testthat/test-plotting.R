#' #' Base R plots create graphical output as a side effect, therefore they cannot
#' #' be easily tested. Visual inspection is still possible and reproducible
#' #' examples are valuable to catch issues. Typically, problems with plotting do
#' #' not mean error is produced, rather unexpected visualization is obtained. The
#' #' whole file is commented out so it does not interfere with the package
#' #' building (unnecessarily extending build time) but it is stored where it is
#' #' expected given its purpose and it will travel with the package.
#' #'
#' #' Solved issue:
#' #' - plot_eddy() at weekly interval used day1 variable from monthly scale
#' #' instead of more suitable date[1]: it is sensible to enforce full month extent
#' #' but weeks can start from arbitrary date and previous approach led to an edge
#' #' case when 2 weeks of data were separated to different plots if data started
#' #' one week after first day of month (day1)
#'
#' library(openeddy)
#'
#' # monthly plots should start at the first day of given month and leave space for
#' # 4 plot panels to conserve width to height ratio
#'
#' # 4 months shifted by 2 weeks correctly take 5 panels and 2 plots
#' t <- seq(ISOdate(2020, 3, 15, 0, 15), ISOdate(2020, 7, 14, 23, 45), "30 mins")
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' a <- data.frame(timestamp = t, my_var = my_var)
#' plot_eddy(a, "my_var", skip = "weekly")
#'
#' # weekly plots should start at the first available day (no need to assure start
#' # of week) with two weeks of data per plot
#'
#' # start of month (2 weeks of data)
#' t <- seq(ISOdate(2020, 7, 1, 0, 15), ISOdate(2020, 7, 14, 23, 45), "30 mins")
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' a <- data.frame(timestamp = t, my_var = my_var)
#' plot_eddy(a, "my_var", skip = "monthly")
#'
#' # half week later
#' t <- seq(ISOdate(2020, 7, 4, 0, 15), ISOdate(2020, 7, 17, 23, 45), "30 mins")
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' a <- data.frame(timestamp = t, my_var = my_var)
#' plot_eddy(a, "my_var", skip = "monthly")
#'
#' # week later - issue with day1 variable specification (at monthly scale)
#' t <- seq(ISOdate(2020, 7, 8, 0, 15), ISOdate(2020, 7, 21, 23, 45), "30 mins")
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' a <- data.frame(timestamp = t, my_var = my_var)
#' plot_eddy(a, "my_var", skip = "monthly")
#'
#' # 1 and half week later
#' t <- seq(ISOdate(2020, 7, 12, 0, 15), ISOdate(2020, 7, 25, 23, 45), "30 mins")
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' a <- data.frame(timestamp = t, my_var = my_var)
#' plot_eddy(a, "my_var", skip = "monthly")
#'
#' # 2 weeks later
#' t <- seq(ISOdate(2020, 7, 15, 0, 15), ISOdate(2020, 7, 28, 23, 45), "30 mins")
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' a <- data.frame(timestamp = t, my_var = my_var)
#' plot_eddy(a, "my_var", skip = "monthly")
#'
#' # 3 weeks later
#' t <- seq(ISOdate(2020, 7, 22, 0, 15), ISOdate(2020, 7, 30, 23, 45), "30 mins")
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' a <- data.frame(timestamp = t, my_var = my_var)
#' plot_eddy(a, "my_var", skip = "monthly")
#'
#' # example with whole year of data
#' library(REddyProc)
#' DETha98 <- fConvertTimeToPosix(Example_DETha98, 'YDH',
#'                                Year = 'Year', Day = 'DoY', Hour = 'Hour')
#' DETha98$timestamp <- DETha98$DateTime - 900
#' names(DETha98)[names(DETha98) == "Rg"] <- "GR"
#' plot_eddy(DETha98, "NEE")
#'
#' trace(plot_eddy, browser)
#' untrace(plot_eddy)
#'
