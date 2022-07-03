#' curl_download2
#'
#' @param ... path through `curl::curl_download()`
#' @param .sleep sleep time.
#' @export
curl_download2 <- function(..., .sleep) {
  # capture side effect.
  res <- safe_curl_download(...)
  Sys.sleep(.sleep)
  if(is.null(res$result[[1]])) {
    result <- "fault"
  } else {
    result <- "succeed"
  }
  return(result)
}

safe_curl_download <- purrr::safely(curl::curl_download)

# curl_download2 <- function(.params, ..., .sleep) {
#   # capture side effect.
#   res <- safe_curl_download(.params[[1]], .params[[2]], ...)
#   Sys.sleep(.sleep)
#   if(is.null(res$result[[1]])) {
#     result <- "fault"
#   } else {
#     result <- "succeed"
#   }
#   return(result)
# }
