test_that("Retry works", {
  res <- wrp_connect() %>%
    set_script("ws:REV") %>%
    wrp_exec()
  expect_error(res, NA)
  dont_work <- function() {
    wrp_connect() %>%
      set_script("ws:REVI") %>%
      wrp_exec(retry = 1)
  }
  start <- Sys.time()
  expect_error(dont_work())
  end <- Sys.time()
  testthat::expect_gte(as.numeric(end - start, units = "secs"), 3)
})

test_that("Unsanitize object", {
  res <- wrp_connect() %>%
    set_script("2 {}", add = c("numeric", "map")) %>%
    wrp_exec()
  expect_equal(res, list(structure(list(), .Names = character(0)), 2))
})