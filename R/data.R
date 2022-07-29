#' NZ CPI from 1914-06-01 to 2022-06-01
#'
#' A dataset of the times series of NZ CPI taken from Stats NZ giving the NZ
#' Consumer Price Index from 1914-06-01 to 2022-06-01.
#' The resulting index series was based on average annual aggregate expenditures
#' for the four centres for the period 1909 to 1913 and calculated using the
#' Laspeyres formulae.
#'
#' @format A data frame with 433 rows and 2 variables:
#' \describe{
#'   \item{Date}{date, quarterly in 'YYYY-MM-DD' format}
#'   \item{CPI_Index}{NZ Consumer Price Index}
#' }
#' @source \url{https://www.stats.govt.nz/large-datasets/csv-files-for-download/}
"nz_cpi_data"

