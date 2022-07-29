#' Calculates percentage change of NZ CPI between two dates
#'
#' Calculates percentage change of NZ CPI between two given dates using the formula (b-a)/a*100 and
#' returns sentence describing how CPI has changed.
#' If inputted start and end dates are not listed in the 'Date' column of `data` then the function picks
#' the nearest date in 'Date' to use.
#' Gives warnings/errors if inputted dates are inappropriate.
#'
#' @param start,end only accepts a single value chr or date type of format'YYYY-MM-DD', `start` (defaults to first date in `data`) and `end` (defaults to last date in `data`) gives
#' the start and end dates for the period over which you want to calculate how the CPI has changed
#' @param data NZ CPI dataset, use `create_cpi_df` function or defaults to `nz_cpi_data` dataset
#'
#' @returns string describing CPI change
#'
#' @examples
#' perc_change_cpi()
#' perc_change_cpi('2000-01-01','1999-01-01')
#' perc_change_cpi('1999-01-01','2000-01-01')
#' perc_change_cpi(start='2000-06-01')
#' perc_change_cpi(start='1900-01-12',end='2020-08-10')
#' perc_change_cpi(start='1950-01-01',end='2020-09-01', data = create_cpi_df('consumers-price-index-june-quarter-2022-index-numbers.csv'))
#'
#' \dontrun{
#' perc_change_cpi('01-01-2000','01-10-2000')
#' perc_change_cpi(start=c('2000-01-01', '2000-03-01'),end=c('2000-03-01','2000-06-01'))
#' }
#'
#' @keywords consumer price index, percentage, change, CPI, NZ

perc_change_cpi <- function(start = data[1,1], end = data[nrow(data),1], data = nz_cpi_data){
  ifelse((length(start) > 1 | length(end) > 1), stop('Error multiple values. Please only input one start and one end value'), c(start,end))
  if (is.na(lubridate::ymd(start)) | is.na(lubridate::ymd(end))){
    stop("Invalid input - check input is in date format 'YYYY-MM-DD'")
  }
  if (start < data[1,1]| end > data[nrow(data),1]){
    warning("Out of data date range.")
  }
  start = as.Date(start)
  end = as.Date(end)
  v1 = data[which.min(abs(start-data$Date)), 'CPI_Index']
  v2 = data[which.min(abs(end-data$Date)), 'CPI_Index']
  if (v1 == 0) {
    cpi_change = paste('Unable to calculate percentage change as unable to divide by zero (CPI value is 0 in', start, ')')
  }else{
    change = ((v2-v1)/v1)*100
    cpi_change = paste('The NZ CPI Index changed by', round(change,2), 'percent between', start, 'and', end)
  }
  return(cpi_change)
}
