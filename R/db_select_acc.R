#' Select accession ids.
#'
#' @param ... path through `curl::curl_download()`
#' @param .sleep sleep time.
#' @export
db_select_acc <- function(.acc, .type = "Direct submissions") {
  # get acc_table.
  ref <- acc_table
  # get pattern.
  pat <- ref %>%
    dplyr::filter(Type == .type) %>%
    dplyr::pull(Prefix)
  pat <- paste0(pat, collapse = "|")
  cat(cli::col_br_blue(cli::style_bold(glue::glue("raw data: {len} .\n",  len = length(.acc)))))
  # filter by condition.
  acc_used <- .acc %>%
    tibble::enframe() %>%
    dplyr::filter(stringr::str_detect(value, pat)) %>%
    dplyr::pull(value)
  cat(cli::col_br_blue(cli::style_bold(glue::glue("filtered data: {len} .\n",  len = length(acc_used)))))
  return(acc_used)
}
