#' Convert taxid to accession ids.
#'
#' @param .taxid taxids.
#' @param .db_path database path.
#' @export
db_convert_taxid2acc <- function(.taxid, .db_path) {
  cat(cli::col_br_blue(cli::style_bold("convert taxid to accession id.\n")))
  # load data.
  taxid2acc <- arrow::read_parquet(here::here(.db_path))
  # get accession ids.
  acc <- taxid2acc %>%
    dplyr::filter(taxid %in% .taxid) %>%
    dplyr::pull(accession)
  return(acc)
}
