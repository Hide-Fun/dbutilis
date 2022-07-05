#' Convert taxon name to taxid.
#'
#' @param .taxon taxon name.
#' @param .target_rank taxon rank.
#' @param .db_path database path.
#' @param .check_kingdom kingdom of taxon.
#' @param .lower_taxon get taxid in lower taxon.
#' @export
db_convert_taxon2taxid <- function(.taxon,
                                   .target_rank,
                                   .db_path,
                                   .check_kingdom = "Fungi",
                                   .lower_taxon = TRUE) {
  cat(cli::col_br_blue(cli::style_bold("convert taxon name to taxid.\n")))
  # load data.
  rankedlineage <- arrow::read_parquet(here::here(.db_path))
  # get taxid(s).
  if(.lower_taxon == TRUE) {
    taxid_query <- rankedlineage %>%
      dplyr::filter({{ .target_rank }} == .taxon & kingdom == .check_kingdom) %>%
      dplyr::pull(tax_id)
  } else {
    taxid_query <- rankedlineage %>%
      dplyr::filter(tax_name == .taxon & kingdom == .check_kingdom) %>%
      dplyr::pull(tax_id)
  }
  return(taxid_query)
}
