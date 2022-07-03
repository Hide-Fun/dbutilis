#' map2_chr() with progress bar.
#'
#' heavily inspired with https://github.com/tidyverse/purrr/issues/149#issuecomment-1167456412
#' @param .x variable 1.
#' @param .y variable 2.
#' @param .f function.
#' @param ... augments.
#' @export
promap2_chr <- function(.x, .y, .f, ...) {
  progressr::with_progress({
    .f <- purrr::as_mapper(.f, ...)
    p <- progressr::progressor(steps = length(.x))
    everyN = 1
    if(length(.x) > 5000){
      everyN = 50
    }
    new.f <- function(...){
      ret = .f(...)
      # Hacky, but for large maps, only do a progressbar every so often.
      # Probably has minimal impact and maybe `runif` is more expensive
      if(everyN == 1){
        p()
      } else if(ceiling(runif(1, 0, everyN)) == 1){
        p(amount=everyN)
      }
      ret
    }
    .Call(purrr:::map2_impl, environment(), ".x", ".y", "new.f", "character")
  })
}
