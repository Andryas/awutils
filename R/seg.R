#' Split in Equal Groups
#'
#' Split a vector of size n into k groups
#'
#' @param n size of vector
#' @param k number of groups
#'
#' @return a numbered vector of k groups.
#'
#' @examples
#'
#' x <- c(1:100)
#' sge(length(x), 3)
#' split(x, sge(length(x), 3))
#'
#' @export
seg <- function(n, k) {
  return(ceiling((1:n)/(n/k)))
}