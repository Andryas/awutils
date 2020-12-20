#' Format a number
#'
#' Let numbers readable for humans.
#'
#' @param x a numeric value
#' @param symbol a symbol like R$
#' @param digits digits to considering (Default: 1)
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(cars, aes(x = dist, y = speed)) +
#'  geom_point() +
#'  scale_y_continuous(label = formatNum)
#'
#' formatNum(seq(100000, 1000000, 100000))
#'
#' @export
formatNum <- function(x = NULL, symbol = "", digits = 1) {
  humanity <- function(y) {
    if (!is.na(y)) {

      tn <- round(abs(y) / 1e12, digits)
      b <- round(abs(y) / 1e9, digits)
      m <- round(abs(y) / 1e6, digits)
      k <- round(abs(y) / 1e3, digits)

      if (y >= 0) {
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }

      if (k < 1) {
        paste0(y_is_positive, symbol, round(abs(y), digits))
      } else if (m < 1) {
        paste0(y_is_positive, symbol,  k , "k")
      } else if (b < 1) {
        paste0(y_is_positive, symbol, m , "m")
      } else if (tn < 1) {
        paste0(y_is_positive, symbol, b , "bn")
      } else {
        paste0(y_is_positive, symbol,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)) {
      "-"
    }
  }

  sapply(x, humanity)
}
