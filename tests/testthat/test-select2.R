# tests/testthat/test-select.R

test_that("selects by character names", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  out <- select2(df, c("a", "c"))
  expect_identical(out, df[, c("a", "c"), drop = FALSE])
})

test_that("selects by integer positions", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  out <- select2(df, c(1L, 3L))
  expect_identical(out, df[, c(1L, 3L), drop = FALSE])
})

test_that("single column still returns a data.frame (drop = FALSE)", {
  df <- data.frame(a = 1:3, b = 4:6)
  out <- select2(df, "a")
  expect_true(is.data.frame(out))
  expect_identical(names(out), "a")
})

test_that("errors if `data` is not a data.frame", {
  mat <- matrix(1:4, nrow = 2)
  expect_error(select2(mat, 1), "`data` must be a data frame\\.")
})

test_that("errors if `vars` is not character or numeric", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(select2(df, TRUE), "`vars` must be a character vector \\(names\\) or numeric vector \\(positions\\)\\.")
  expect_error(select2(df, list("a")), "`vars` must be a character vector \\(names\\) or numeric vector \\(positions\\)\\.")
})

test_that("errors on invalid names or positions (base R semantics)", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  # invalid name
  expect_error(select2(df, "z"), "undefined columns selected")
  
  # invalid position (0 or > ncol)
  expect_error(select2(df, 0), "subscript out of bounds|undefined columns selected")
  expect_error(select2(df, 3), "subscript out of bounds|undefined columns selected")
})
