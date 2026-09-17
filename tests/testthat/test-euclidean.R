test_that("euclidean calculates correct GCD values", {
  expect_equal(euclidean(100, 1000), 100)
  expect_equal(euclidean(123612, 13892347912), 4)
  expect_equal(euclidean(-10, 5), 5)
})

test_that("euclidean handles invalid inputs correctly", {
  expect_error(euclidean("a", 5))
  expect_error(euclidean(c(10, 20), 5))
})
