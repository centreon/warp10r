test_that("load a file", {
  file <- tempfile()
  writeLines("1 [ 2 $c ]", file)
  expect_equal(
    wrp_connect() %>%
      load_file(file, c = 3, return_object = c("numeric", "list")) %>%
      wrp_exec(),
    list(c(2, 3), 1)
  )
})