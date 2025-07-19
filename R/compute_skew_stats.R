  #' Compute alternative boxplot statistics
  #'
  #' @param x A numeric vector
  #' @param method Method name ("tukey", "kimber", "hubert", etc.)
  #' @param k Tuning parameter (default 1.5)
  #'
  #' @return A list of boxplot stats: ymin, lower, middle, upper, ymax, outliers
  #' @export
  compute_skew_stats <- function(x, method = "tukey", k = 1.5) {
    x <- stats::na.omit(x)
    q1 <- stats::quantile(x, 0.25)
    q2 <- stats::median(x)
    q3 <- stats::quantile(x, 0.75)
    iqr <- q3 - q1

    mc <- {
      above <- x[x > q2]
      below <- x[x < q2]
      if (length(above) == 0 || length(below) == 0) 0 else
        (stats::median(above) - stats::median(below)) / iqr
    }

    sk <- {
      n <- length(x)
      m3 <- mean((x - mean(x))^3)
      m2 <- mean((x - mean(x))^2)
      s <- if (m2 == 0) 0 else m3 / (m2^(3/2))
      max(min(s, 3.5), -3.5)
    }

    BC <- (q3 + q1 - 2 * q2) / (q3 - q1)

    switch(tolower(method),
           tukey = {
             lower <- q1 - k * iqr
             upper <- q3 + k * iqr
           },
           kimber = {
             lower <- q1 - 2 * k * (q2 - q1)
             upper <- q3 + 2 * k * (q3 - q2)
           },
           hubert = {
             if(mc > 0) {
               factor_l <- exp(-4 * mc)
               factor_u <- exp(3 * mc)
               lower <- q1 - k * factor_l * iqr
               upper <- q3 + k * factor_u * iqr  
               }
             else {
               factor_l <- exp(-3 * mc)
               factor_u <- exp(4 * mc)
               lower <- q1 - k * factor_l * iqr
               upper <- q3 + k * factor_u * iqr  
             }
           },
           adil = {
             factor_l <- exp(-sk * abs(mc))
             factor_u <- exp(sk * abs(mc))
             lower <- q1 - k * iqr * factor_l
             upper <- q3 + k * iqr * factor_u
           },
           babura = {
             factor <- exp(6 * BC)
             lower <- q1 - k * factor * iqr
             upper <- q3 + k * factor * iqr
           },
           walker = {
             factor_l <- (1 - BC) / (1 + BC)
             factor_u <- (1 + BC) / (1 - BC)
             lower <- q1 - k * iqr * factor_l
             upper <- q3 + k * iqr * factor_u
           },
           junsawang = {
             factor_l <- exp(BC * (q2 - q1) / (q3 - q2))
             factor_u <- exp(BC * (q3 - q2) / (q2 - q1))
             lower <- q1 - k * factor_l * iqr
             upper <- q3 + k * factor_u * iqr
           },
           stop("Unknown method")
    )

    lower <- max(lower, min(x, na.rm = TRUE))
    #upper <- min(upper, max(x, na.rm = TRUE))

    list(
      #ymin = if (any(x >= lower)) min(x[x >= lower], na.rm = TRUE) else min(x, na.rm = TRUE),
      ymin = lower,
      lower = q1,
      middle = q2,
      upper = q3,
      #ymax = if (any(x <= upper)) max(x[x <= upper], na.rm = TRUE) else max(x, na.rm = TRUE),
      ymax = upper,
      outliers = x[x < lower | x > upper]
    )
  }
