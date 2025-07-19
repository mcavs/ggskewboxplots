#' Statistical Transformation for Skew Boxplots
#'
#' `StatSkewBoxplot` is a ggproto object that computes statistics necessary
#' to create skew boxplots, including quantiles, skewness measures, and whiskers.
#'
#' @format A ggproto object
#' @usage NULL
#' @keywords internal
#' @export
StatSkewBoxplot <- ggplot2::ggproto("StatSkewBoxplot", ggplot2::Stat,
                                    required_aes = c("x", "y"),

                                    compute_group = function(data, scales, method, k) {


                                      stats <- compute_skew_stats(data$y, method = method, k = k)

                                      data.frame(
                                        x = data$x[1],
                                        ymin = stats$ymin,
                                        lower = stats$lower,
                                        middle = stats$middle,
                                        upper = stats$upper,
                                        ymax = stats$ymax,
                                        y = stats$middle,
                                        outliers = I(list(stats$outliers))
                                      )
                                    }
)
#' Skewness-Aware Boxplot (ggplot2 layer)
#'
#' Draws boxplots using alternative methods for skewness adjustment.
#'
#' @inheritParams ggplot2::geom_boxplot
#' @param stat The statistical transformation to use on the data for this layer. Defaults to "skewboxplot".
#' @param method Skew boxplot method (e.g. "tukey", "hubert", etc.)
#' @param k Tuning parameter (default = 1.5)
#' @export
geom_skewboxplot <- function(mapping = NULL, data = NULL,
                             stat = StatSkewBoxplot, position = "dodge",
                             ..., method = "tukey", k = 1.5,
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE) {
  ggplot2::layer(
    stat = stat,              # StatSkewBoxplot ggproto objesi doğrudan
    geom = ggplot2::GeomBoxplot,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method = method, k = k, na.rm = na.rm, ...)
  )
}
