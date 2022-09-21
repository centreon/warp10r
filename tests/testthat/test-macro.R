test_that("macro works", {
  res <- wrp_connect() %>%
    set_script("ws:<% %>") %>%
    wrp_store("macro") %>%
    run_macro("macro", "map", .eval = TRUE, x = 1) %>%
    wrp_exec()
  expect_equal(res, c(x = 1))

  res <- wrp_connect() %>%
    set_script(1) %>%
    set_script("ws:<% DROP 1 %>") %>%
    wrp_store("macro") %>%
    run_macro("macro", list(numeric = "numeric"), .eval = TRUE) %>%
    wrp_exec()
  expect_equal(res, 1)


  expect_error({
    wrp_connect() %>%
      set_script(1) %>%
      set_script("ws:<% DROP 1 %>") %>%
      wrp_store("macro") %>%
      run_macro("macro", "numeric", .eval = TRUE) %>%
      wrp_exec()
  })
})
