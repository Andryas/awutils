#' Split Group Equal
#'
#' This function is useful to split a data into
#' k equals groups.
#'
#' @param n total number of observations
#' @param k number of groups
#'
#' @return a numbered vector of k groups.
#'
#' uKGroup
#'
#' uKGroup(100, 2)
#'
#' uKGroup(100, 3)
#'
#' @export
splitGroupEqual <- function(n, k) {
  return(ceiling((1:n)/(n/k)))
}
