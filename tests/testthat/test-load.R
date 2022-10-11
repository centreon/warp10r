test_that("load a file", {
  file <- tempfile()
  writeLines(c("1", "[ 2 $c ] "), file)
  expect_equal(
    wrp_connect() %>%
      load_file(file, c = 3, return_object = c("numeric", "list")) %>%
      wrp_exec(),
    list(c(2, 3), 1)
  )
})

test_that("load an empty file", {
  file <- tempfile()
  writeLines("", file)
  expect_equal(
    wrp_connect() %>%
      load_file(file) %>%
      wrp_exec(),
    list()
  )
})