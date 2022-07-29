#' Use CPI to adjust for inflation
#'
#' Given a value and start and end dates applies the CPI to the value to give purchasing power of one date in terms of another date
#' e.g Dollar Amount multiplied by Start CPI divided by End CPI.
#' When given a single value the function returns a sentence describing how the value has changed unless the `full` argument is changed to 'FALSE'.
#' When given a vector containing multiple values will return a vector of CPI adjusted `amount`.
#' If given start and end dates are not listed in the 'Date' column of the data then the function picks
#' the nearest date in 'Date' to use.
#' If the CPI associated with the `start` date is 0 then it sets the `start` CPI to 0.1 so that it can return a numeric value.
#' The function checks that values are numeric and dates are in appropriate format and gives warnings/errors if inputs are inappropriate.
#'
#' @param amount single element or vector of numeric values, the amount of dollars to be adjusted by CPI
#' @param start,end takes chr or date type of format'YYYY-MM-DD', `start` (defaults to first date in `data`) is date of value to be converted and `end` (defaults to last date in `data`) is date of inflation adjusted value
#' @param data NZ CPI dataset, use `create_cpi_df` function or defaults to `nz_cpi_data` dataset
#' @param full boolean value to determine whether to return a string or vector
#'
#' @returns a string if `full`=TRUE and length of amount,start and end is 1 otherwise returns a vector of adjusted `amount` values
#'
#' @examples
#' apply_cpi()
#' apply_cpi(250)
#' apply_cpi('250')
#' apply_cpi('2000-01-01','1999-01-01')
#' apply_cpi('1999-01-01','2000-01-01')
#' apply_cpi(start='2000-06-11')
#' apply_cpi(full='False')
#' apply_cpi(start='1900-01-12',end='2020-08-10')
#' apply_cpi(200, start='1950-01-01',end='2020-09-01', data = create_cpi_df('consumers-price-index-june-quarter-2022-index-numbers.csv'))
#' apply_cpi(200, start='1950-01-01',end='2020-09-01', data = create_cpi_df('consumers-price-index-june-quarter-2022-index-numbers.csv'), full='FALSE')
#' apply_cpi(c(100,-200,'100.11'))
#' apply_cpi(c(100,'200',-300.5), start=c('1950-01-01','1951-01-01', '1960-08-30'),end=c('2020-09-01', '1989-12-12', '1950-03-21'))
#'
#' \dontrun{
#' apply_cpi('01-01-2000','02-10-2000')
#' apply_cpi(c('100s', '$200', 'one hundred'))
#' apply_cpi(start=c('2021-01-01','2022-01-01'), end=c('2022-01-01','2022-03-01','2022-06-01'))
#' }
#'
#' @keywords consumer price index, inflation, CPI, NZ

apply_cpi <- function(amount=100, start = data[1,1], end = data[nrow(data),1], data = nz_cpi_data, full = TRUE){
  ifelse((is.na(lubridate::ymd(start)) | is.na(lubridate::ymd(end))), stop("Invalid input - check input is in date format 'YYYY-MM-DD'"), c(start,end))
  ifelse((start < data[1,1] | end > data[nrow(data),1]), warning("Out of data date range."), c(start, end))
  ifelse(is.na(as.numeric(amount)), stop("Check value to convert is numeric type"), amount)
  amount = as.numeric(amount)
  start = as.Date(start)
  end = as.Date(end)
  past = sapply(start, function(x) data[which.min(abs(x-data$Date)), 'CPI_Index'])
  current = sapply(end, function(x) data[which.min(abs(x-data$Date)), 'CPI_Index'])
  ifelse(past == 0, past <- 0.1, past)
  worth = round(amount*current/past,2)
  if (length(amount) <= 1 & length(start) <= 1 & length(end) <= 1 & full == TRUE){
    return(paste(amount, 'dollars in', start, 'would have the same purchasing power as', worth, 'dollars in', end))
  }else{
    return(worth)
  }
}
