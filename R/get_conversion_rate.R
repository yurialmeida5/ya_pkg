#' Format numbers as Hungarian forints
#' @param retried count the number of tries
#' @return number
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom checkmate assert_number
#' @importFrom logger log_error log_info
#' @examples
#' get_conversion_rate(retried = 2)


get_conversion_rate <- function(retried = 0) {
  tryCatch({
    conversion_rate <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(conversion_rate, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_conversion_rate( retried = retried + 1)
  })
  log_info('1 USD={conversion_rate} HUF')
  conversion_rate
}
