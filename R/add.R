#' Title
#'
#' @param a 
#' @param b 
#'
#' @returns
#' @export
#'
#' @examples
add <- function(a, b) {
  # make sure that it's two floats
  a <- as.numeric(a)
  b <- as.numeric(b)
  
  if (is.na(a) || is.na(b)) {
    stop("Both inputs must be numeric values.")
  }
  
  a + b
}