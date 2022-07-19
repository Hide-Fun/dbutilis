#' Classify accession ids.
#'
#' @param .acc accession ids.
#' @export
db_classify_acc <- function(.acc) {
  type <- unique(acc_table$Type)
  classify <- purrr::map(type, db_divide_acc, .acc = acc_raw)
  classify_len <- purrr::map_dbl(classify, length)
  unclassify <- .acc[!.acc %in% unlist(classify)]
  unclassify_len <- length(unclassify)
  type <- c(type, "others")
  len <- c(classify_len, unclassify_len)
  return(tibble::tibble(len = len, type = type))
}
