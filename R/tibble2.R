#' tibble2
#'
#' @param ... path through `tibble::tibble()`
#' @export
tibble2 <- purrr::possibly(.f = tibble::tibble, otherwise = tidyr::crossing())
