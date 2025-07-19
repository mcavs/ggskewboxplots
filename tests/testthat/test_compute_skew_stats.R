library(testthat)
library(ggskewboxplots)

test_that("compute_skew_stats hesaplama doğru çalışıyor", {
  library(waldo)
  x <- c(1, 2, 3, 4, 5, 100)  # Aykırı değerli örnek

  res <- compute_skew_stats(x, method = "tukey", k = 1.5)

  expect_equal(res$middle, median(x))
  expect_true(res$ymin <= min(x))
  expect_true(res$ymax <= max(x))
  expect_true(all(res$outliers > res$upper | res$outliers < res$lower))
})

test_that("compute_skew_stats bilinmeyen method hata veriyor", {
  expect_error(compute_skew_stats(c(1,2,3), method = "bilinmeyen"))
})
