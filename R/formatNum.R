#' Format a number
#'
#' Let numbers readble for humans.
#'
#' @param x a numeric value
#' @param smbl a symbol like R$
#' @param signif digits to considering (Default: 1)
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(cars, aes(x = dist, y = speed)) +
#'  geom_point() +
#'  scale_y_continuous(label = gFormatNum)
#'
#' formatNum(seq(100000, 1000000, 100000))
#'
#' @export
formatNum <- function(x = NULL, smbl = "", signif = 1) {
  humanity <- function(y) {
    if (!is.na(y)) {
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)

      if (y >= 0) {
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }

      if (k < 1) {
        paste0(y_is_positive, smbl, round(abs(y), signif))
      } else if (m < 1) {
        paste0(y_is_positive, smbl,  k , "k")
      } else if (b < 1) {
        paste0(y_is_positive, smbl, m , "m")
      } else if (tn < 1) {
        paste0(y_is_positive, smbl, b , "bn")
      } else {
        paste0(y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)) {
      "-"
    }
  }

  sapply(x, humanity)
}
