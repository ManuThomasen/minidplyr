#' Title
#'
#' More description
#' 
#' More details
#'
#' @param a a number
#' @param b a number
#'
#' @returns the sum of a and b 'a + b' 
#' @export
#' @import dplyr
#'
#' @examples
#' add(1,2)
add <- function(a, b) {
  # make sure that it's two floats
  a <- as.numeric(a)
  b <- as.numeric(b)
  
  if (is.na(a) || is.na(b)) {
    stop("Both inputs must be numeric values.")
  }
  
  a + b
}