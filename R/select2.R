#' Select Variables from a Data Frame
#'
#' This function selects variables (columns) from a data frame based on 
#' either a character vector of variable names or an integer vector of 
#' variable positions. It is a simple wrapper around the standard 
#' subsetting operator `[` for data frames.
#'
#' @param data A data frame from which variables will be selected.
#' @param vars A character vector of variable names or an integer vector 
#'   of column positions.
#'
#' @return A data frame containing only the selected variables.
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
#' 
#' # Select by name
#' select2(df, c("a", "c"))
#'
#' # Select by position
#' select2(df, c(1, 3))
#'
#' @export
select2 <- function(data, vars) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  if (!is.character(vars) && !is.numeric(vars)) {
    stop("`vars` must be a character vector (names) or numeric vector (positions).")
  }
  
  # Prevent empty selections
  if (length(vars) == 0) {
    stop("`vars` must specify at least one column.")
  }
  
  # If numeric: check bounds
  if (is.numeric(vars)) {
    if (any(vars < 1 | vars > ncol(data))) {
      stop("subscript out of bounds|undefined columns selected.")
    }
  }
  
  # If character: check existence
  if (is.character(vars)) {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop("undefined columns selected")
    }
  }
  
  data[, vars, drop = FALSE]
}

