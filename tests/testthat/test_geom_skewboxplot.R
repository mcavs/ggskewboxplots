library(testthat)
library(ggplot2)
library(ggskewboxplots)

test_that("geom_skewboxplot temel grafik oluşturuyor", {
  p <- ggplot(mpg, aes(x = class, y = hwy)) +
    geom_skewboxplot(method = "walker")

  expect_s3_class(p, "ggplot")

  # Geometrilerin içinde GeomBoxplot var mı kontrol et
  geoms <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomBoxplot" %in% geoms)
})


test_that("geom_skewboxplot uyarı vermiyor", {
  expect_warning(
    print(
      ggplot(mpg, aes(x = class, y = hwy)) +
        geom_skewboxplot(method = "walker")
    ),
    NA
  )
})

