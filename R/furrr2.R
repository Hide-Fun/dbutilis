#' Suppress message of furrr::future_map()
#'
#' @param .x list.
#' @param .f function.
#' @param ... path through `future_map()`
#' @export
supmap <- function(.x, .f, ...) {
  suppressMessages(suppressWarnings(furrr::future_map(.x = .x, .f = .f, ..., .options = furrr::furrr_options(seed = 1L))))
}

supmap_dfr <- function(.x, .f, ...) {
  suppressMessages(suppressWarnings(furrr::future_map_dfr(.x = .x, .f = .f, ..., .options = furrr::furrr_options(seed = 1L))))
}

supmap_dfc <- function(.x, .f, ...) {
  suppressMessages(suppressWarnings(furrr::future_map_dfc(.x = .x, .f = .f, ..., .options = furrr::furrr_options(seed = 1L))))
}
