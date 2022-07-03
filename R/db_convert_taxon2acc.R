#' Convert taxon name to accession ids.
#'
#' @param .taxon taxon name.
#' @param .target_rank taxon rank.
#' @param .db_path_tax taxonomy database path.
#' @param .db_path_acc accession database path.
#' @param .check_kingdom kingdom of taxon.
#' @param .lower_taxon get taxid in lower taxon.
#' @export
db_convert_taxon2acc <- function(.taxon,
                                 .target_rank,
                                 .db_path_tax,
                                 .db_path_acc,
                                 .check_kingdom = "Fungi",
                                 .lower_taxon = TRUE) {
  cli::col_br_blue(cli::style_bold("Exclude WGS sequences."))
  # convert taxon name to taxid.
  taxids <- db_convert_taxon2taxid(
    .taxon = taxon,
    .target_rank = {{ .target_rank }},
    .db_path = .db_path_tax,
    .check_kingdom = .check_kingdom,
    .lower_taxon = .lower_taxon
  )
  # convert taxid to accession id.
  acc <- db_convert_taxid2acc(
    .taxid = taxids,
    .db_path = .db_path_acc
  )
  return(acc)
}
