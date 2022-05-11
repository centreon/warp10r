test_that("assert labels", {
  expect_identical(assert_labels(NULL), NULL)
  expect_identical(assert_labels(list(foo = "bar")), list(foo = "bar"))
  expect_identical(assert_labels(list(foo = 1)), list(foo = "1"))
  expect_identical(assert_labels(c(foo = "bar")), list(foo = "bar"))
  expect_error(assert_labels(list("foo", "bar")))
  expect_error(assert_labels(list(foo = "bar", "bar")))
})