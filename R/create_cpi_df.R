#' Creates dataframe of NZ CPI from StatsNZ csv file
#'
#' @description
#' Using consumer price index data from StatsNZ the function reformats the date column, fills in missing data and selects the 'all group' CPI
#' (which is an average of all the CPI categories) to return a dataframe of the NZ CPI and matching quarterly dates.
#' This function can be used in the `data` argument in the other functions in this package.
#'
#' @details
#' From the Stats NZ website e.g 'https://www.stats.govt.nz/large-datasets/csv-files-for-download/'
#' locate the NZ CPI file which should be labelled something like 'consumers-price-index-quarter here-index-numbers.csv'
#' and download the csv file for input.
#'
#' @param cpi path to downloaded CPI csv file from StatsNZ
#'
#' @returns a data.frame with column 'Date' giving quarterly dates, and column 'CPI_Index' giving NZ consumer price index.
#'
#' @examples
#' create_cpi_df(cpi='consumers-price-index-june-quarter-2022-index-numbers.csv')
#' create_cpi_df('consumers-price-index-june-quarter-2022-index-numbers.csv')
#' create_cpi_df('~/Downloads/consumers-price-index-june-quarter-2022-index-numbers.csv')
#'
#' \dontrun{
#' create_cpi_df('a_random_csv.csv')
#' }
#'
#' @keywords consumer price index, CPI, NZ, StatsNZ

create_cpi_df <- function(cpi){
  if(!grepl("consumers\\-price\\-index.*index\\-numbers\\.csv$", basename(cpi))){ #check right file
    stop('Incorrect file. See help on what file to use.')
  }
  df = read.csv(cpi)
  df = df[df$Group == 'CPI All Groups for New Zealand',]
  df['Date'] = gsub("\\.", "-", df$Period)
  df['Date'] = gsub("$", "-01", df$Date)
  df['Date'] = as.Date(df$Date, "%Y-%m-%d")
  df$Data_value = zoo::na.locf(with(df, replace(Data_value,Data_value==0,NA)), fromLast = TRUE) #replace 0 CPI values with previous quarters value
  colnames(df)[colnames(df)  == 'Data_value'] = 'CPI_Index'
  rownames(df) = 1:nrow(df)
  df = df[c('Date','CPI_Index')]
  return(df)
}

