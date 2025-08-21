#' Filter rows of a data frame
#'
#' A minimal base R helper to filter rows of a data frame using a logical
#' condition evaluated in the data's environment. Rows where the condition
#' is \code{NA} are dropped.
#'
#' @param data A data frame.
#' @param condition A logical expression or logical vector indicating rows to keep.
#'   If an expression, it is evaluated in the context of \code{data}.
#'   The result must be length 1 or \code{nrow(data)}. A scalar \code{TRUE}/\code{FALSE}
#'   is recycled to \code{nrow(data)}.
#'
#' @return A data frame containing only rows where \code{condition} is \code{TRUE}.
#'   Rows with \code{NA} in \code{condition} are removed.
#'
#' @examples
#' df <- data.frame(x = 1:5, y = c(2, 2, 3, NA, 5))
#'
#' # Expression evaluated in data context
#' filter2(df, x > 2)
#'
#' # Combine conditions
#' filter2(df, x > 2 & y == 5)
#'
#' # Logical vector (must be length 1 or nrow(df))
#' keep <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#' filter2(df, keep)
#'
#' # Scalar logical is recycled
#' filter2(df, TRUE)
#'
#' @export
filter2 <- function(data, condition) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (missing(condition)) {
    stop("`condition` must be provided.")
  }
  
  # Evaluate user condition in data context
  cond <- eval(substitute(condition), data, parent.frame())
  
  if (!is.logical(cond)) {
    stop("`condition` must evaluate to a logical vector.")
  }
  
  n <- nrow(data)
  
  # Allow scalar recycling (TRUE/FALSE) to n rows
  if (length(cond) == 1L) {
    cond <- rep(cond, n)
  }
  
  if (length(cond) != n) {
    stop("`condition` must have length 1 or nrow(data).")
  }
  
  # Drop NA rows; keep only TRUE
  keep <- !is.na(cond) & cond
  data[keep, , drop = FALSE]
}
