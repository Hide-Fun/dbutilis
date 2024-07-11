#' Classify accession ids.
#'
#' @param .acc accession ids.
#' @export
db_classify_acc <- function(.acc) {
  # remove RefSeq
  filtered_ids <- .acc[!grepl("^[A-Z]+_\\d+$", .acc)]
  cat(cli::col_br_blue(cli::style_bold("Removing RefSeq IDs.")))
  cat(cli::col_br_blue(cli::style_bold(glue::glue("filtered data: {len} .\n",  len = length(filtered_ids)))))
  dbutilis::acc_table |>
    dplyr::filter(!is.na(accession_format)) |>
    dplyr::mutate(
      query = purrr::map_int(accession_pat, ~sum(stringr::str_detect(filtered_ids, .x)))
    )
}
