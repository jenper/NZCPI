#' Plots NZ CPI
#'
#' @description
#' Plots NZ CPI against time.
#' Optional arguments to the function are start and end dates that limit the displayed CPI to within the date range.
#' If inputted start and end dates are not listed in the 'Date' column of `data` then the function picks the nearest date in 'Date' to use.
#' Gives warnings/errors if given dates are inappropriate.
#'
#' @details
#' Requires ggplot2 and scales.
#'
#' @param start,end takes chr or date type of format'YYYY-MM-DD', `start` (defaults to first date in `data`) and `end` (defaults to last date in `data`) gives the date range of the CPI to plot
#' @param data NZ CPI dataset, use `create_cpi_df` function or defaults to `nz_cpi_data` dataset
#'
#' @examples
#' plot_cpi()
#' plot_cpi(data = create_cpi_df('consumers-price-index-june-quarter-2022-index-numbers.csv'))
#' plot_cpi('1997-01-01','2000-06-01')
#' plot_cpi(start='1900-01-12',end='2020-08-10')
#' plot_cpi(end='2000-06-01')
#' plot_cpi(start='1950-01-01',end='2020-09-01', data = create_cpi_df('consumers-price-index-june-quarter-2022-index-numbers.csv'))
#'
#' \dontrun{
#' plot_cpi('01-01-2000','02-10-2000')
#' plot_cpi(start='2000-01-01',end='1999-01-01')
#' plot_cpi(start=c('2000-01-01', '2000-03-01'),end=c('1999-01-01','2000-01-01'))
#' }
#'
#' @keywords consumer price index, plot, CPI, NZ

plot_cpi <- function(start = data[1,1], end = data[nrow(data),1], data = nz_cpi_data){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop(
      "Package \"scales\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (is.na(lubridate::ymd(start)) | is.na(lubridate::ymd(end))){
    stop("Invalid input - check input is in date format 'YYYY-MM-DD'")
  }
  if (start > end){
    warning("End date is earlier than start date.")
  }
  if (start < data[1,1]| end > data[nrow(data),1]){
    warning("Out of data date range.")
  }
  df = data[data$Date >= start & data$Date <= end,]
  ggplot(df, aes(x=Date, y=CPI_Index)) +
    geom_line() +
    scale_x_date(breaks = scales::breaks_pretty(10),expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::breaks_pretty(10)) +
    labs(title = "NZ CPI") +
    theme_minimal()
}

