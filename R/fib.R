#' Generate fibonacci numbers.
#'
#' @param n A number.
#' @return \code{n}'th Fibonacci number
#' @examples
#' fib(5)
#' fib(20)
fib <- function(n) {
  if (is.na(n)) {
    stop("Input parameter must not be 'NA'")
  } else if (length(n) > 1) {
    warning("only 1st vector element is used")
  } else if (n != round(n)) {
    stop("Integer value for parameter 'n' expected")
  } else if (n < 0) {
    stop("Negative numbers not supported")
  }
  # Base cases
  if (n == 0) {
    return(0)
  }
  if (n == 1) {
    return(1)
  }
  # Everything else
  return(fib(n - 1) + fib(n - 2))
}
