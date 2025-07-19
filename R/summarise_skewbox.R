#' Summarise skewness-aware boxplot statistics
#' @name summarise_skewbox
#' @title Summarise Skew-Aware Boxplot Stats
#'
utils::globalVariables("stats")
#'
#' @param .data A data frame (preferably grouped)
#' @param var Unquoted numeric column name
#' @param method Method name (e.g. "adil", "hubert", etc.)
#' @param k Tuning parameter (default 1.5)
#'
#' @return A tibble with boxplot stats
#' @export
summarise_skewbox <- function(.data, var, method = "tukey", k = 1.5) {
  var <- rlang::enquo(var)

  .data |>
    dplyr::summarise(stats = list(compute_skew_stats(!!var, method = method, k = k)), .groups = "drop") |>
    tidyr::unnest_wider(stats)
}
