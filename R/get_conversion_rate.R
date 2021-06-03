#' Returns the current exchange rate from USD to HUF
#' @param retried count the number of tries
#' @return number
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom checkmate assert_number
#' @importFrom logger log_error log_info
#' @examples
#' get_usdhuf(retried = 2)


get_usdhuf <- function(retried = 0) {
  tryCatch({
    conversion_rate <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(conversion_rate, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhuf( retried = retried + 1)
  })
  log_info('1 USD={conversion_rate} HUF')
  conversion_rate
}


#' Returns the exchange rates from one currency to another from a start to an end-date
#' @param start_date date
#' @param end_date date
#' @param fromCUR base currency that you want to convert
#' @param toCUR final currency
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom checkmate assert_numeric
#' @importFrom logger log_error
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_exchange_rates('EUR', 'USD', start_date = '2020-05-12', end_date = '2020-05-13')
#' get_exchange_rates('EUR', 'HUF', start_date = '2020-05-12', end_date = '2020-05-13')


get_exchange_rates <- function(fromCUR, toCUR ,start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0) {
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = fromCUR,
        symbols = toCUR
      )
    )
    #stop_for_status(response)
    exchange_rates <- content(response)$rates
    convrate_table <- data.table(
      date = as.Date(names(exchange_rates)),
      convrate = as.numeric(unlist(exchange_rates)))
    assert_numeric(convrate_table$convrate)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_exchange_rates(retried = retried + 1)
  })
  convrate_table
}

