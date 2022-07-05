#' Convert table to fasta.
#'
#' @param .data data.frame
#' @param .otu otu column name.
#' @param .seq sequence column name.
#' @export
table_to_fasta = function(.data, .otu, .seq) {
  otu <- rlang::enquo(.otu)
  seq <- rlang::enquo(.seq)
  # select
  n <- nrow(.data)
  otu_col <- .data %>%
    dplyr::select(!!otu) %>%
    dplyr::rename(col = !!otu) %>%
    dplyr::mutate(col = stringr::str_c(">", col)) %>%
    tibble::add_column(num = seq(1, 2 * n, by = 2))
  seq_col <- .data %>%
    dplyr::select(!!seq) %>%
    dplyr::rename(col = !!seq) %>%
    tibble::add_column(num = seq(1, 2 * n, by = 2) + 1)
  # bind
  fasta <- dplyr::bind_rows(otu_col, seq_col) %>%
    dplyr::arrange(num) %>%
    dplyr::select(col)
  return(fasta)
}
