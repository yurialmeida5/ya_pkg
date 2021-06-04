context('forrint')

library(ya)
library(stats)

test_that("converts number to string and add the Ft", {
  expect_equal( forint(42), "42 Ft")
})
#> Test passed 🥇

test_that("converts integer to string and add the Ft", {
  expect_equal( forint(42.5), "42.50 Ft")
})
#> Test passed 🥇

test_that("converts integer to string and add the Ft", {
  expect_equal( forint(42.59), "42.59 Ft")
})
#> Test passed 🥇

