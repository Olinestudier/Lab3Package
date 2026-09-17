#' Euclidean Algorithm
#'
#' Calculates the greatest common divisor of two integers using the Euclidean algorithm.
#'
#' @param a A numeric scalar or integer.
#' @param b A numeric scalar or integer.
#'
#' @return The greatest common divisor as a numeric scalar or integer.
#' @export
#'
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
euclidean <- function(a, b) {

  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)

  a <- abs(a)
  b <- abs(b)

  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }

  return(a)
}
