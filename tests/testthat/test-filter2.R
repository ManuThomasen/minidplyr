test_that("filters by expression in data context", {
  df <- data.frame(x = 1:5, y = c(2, 2, 3, NA, 5))
  out <- filter2(df, x > 2)
  expect_identical(out, df[df$x > 2, , drop = FALSE])
})

test_that("drops NA in condition (keeps only TRUE)", {
  df <- data.frame(x = 1:5, y = c(2, 2, 3, NA, 5))
  cond <- df$y == 2  # c(TRUE, TRUE, FALSE, NA, FALSE)
  out <- filter2(df, cond)
  expect_identical(out, df[!is.na(cond) & cond, , drop = FALSE])
})

test_that("accepts scalar logical and recycles", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_identical(filter2(df, TRUE), df)
  expect_identical(filter2(df, FALSE), df[FALSE, , drop = FALSE])
})

test_that("accepts logical vector of length nrow(data)", {
  df <- data.frame(x = 1:4)
  keep <- c(TRUE, FALSE, TRUE, FALSE)
  expect_identical(filter2(df, keep), df[keep, , drop = FALSE])
})

test_that("errors if data is not a data.frame", {
  mat <- matrix(1:4, nrow = 2)
  expect_error(filter2(mat, TRUE), "`data` must be a data frame\\.")
})

test_that("errors if condition missing", {
  df <- data.frame(x = 1:3)
  expect_error(filter2(df), "`condition` must be provided\\.")
})

test_that("errors if condition not logical", {
  df <- data.frame(x = 1:3)
  expect_error(filter2(df, x + 1), "`condition` must evaluate to a logical vector\\.")
})

test_that("errors on incompatible length", {
  df <- data.frame(x = 1:3)
  expect_error(filter2(df, c(TRUE, FALSE)), "`condition` must have length 1 or nrow\\(data\\)\\.")
})

