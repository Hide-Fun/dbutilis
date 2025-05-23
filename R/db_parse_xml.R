#' Parse genbank xml files.
#'
#' @param .target_dir target directory that contains xml fiels.
#' @param .gbseq parse option.
#' @param .workers see `?multisession`.
#' @export
db_parse_gbxml <- function(.target_dir, .gbseq, .workers) {
  # set multi session.
  future::plan(future::multisession, workers = .workers)
  # get xml path.
  cat(cli::col_br_blue(cli::style_bold("parse XML to csv.\n")))
  xml_path <- fs::dir_ls(here::here(.target_dir), glob = "*.xml")
  cat(cli::col_br_blue(cli::style_bold(glue::glue("parse {len} xml files.\n",  len = length(xml_path)))))
  # load  & parse xml.
  parsed <- suppressWarnings(promap(
      xml_path,
      parse_gbxml2,
      .gbseq = .gbseq
  ))
  return(parsed)
}

parse_gbxml2 <- function(.path, .gbseq) {
  xml <- xml2::read_xml(.path)
  parsed <- parse_gbxml(.xml = xml, .gbseq = .gbseq)
  return(parsed)
}

parse_gbxml <- function(.xml, .gbseq = c("all", "non_seq", "manual")) {
  # get xml size.
  size <- xml2::xml_length(.xml)
  # get children.
  list <- xml2::as_list(.xml)
  # split xml.
  # each GBSeq stored in list.
  gbseq_list <- supmap(1:size, ~SplitGBset(.list = list, .num = .x))
  feature_table_list <- supmap(gbseq_list, purrr::pluck, "GBSeq_feature-table", "GBFeature", "GBFeature_quals")
  other_seqids_list <- supmap(gbseq_list, purrr::pluck, "GBSeq_other-seqids")
  references_list <- supmap(gbseq_list, purrr::pluck, "GBSeq_references", "GBReference")

  # parse gbseq_list.
  if(.gbseq == "all") {
    lev_gbeq <- list("GBSeq_locus",
                     "GBSeq_length",
                     "GBSeq_strandedness",
                     "GBSeq_moltype",
                     "GBSeq_topology",
                     "GBSeq_division",
                     "GBSeq_update-date",
                     "GBSeq_create-date",
                     "GBSeq_definition",
                     "GBSeq_primary-accession",
                     "GBSeq_accession-version",
                     "GBSeq_source",
                     "GBSeq_organism",
                     "GBSeq_taxonomy",
                     "GBSeq_sequence")
  } else if(.gbseq == "non_seq") {
    lev_gbeq <- list("GBSeq_locus",
                     "GBSeq_length",
                     "GBSeq_strandedness",
                     "GBSeq_moltype",
                     "GBSeq_topology",
                     "GBSeq_division",
                     "GBSeq_update-date",
                     "GBSeq_create-date",
                     "GBSeq_definition",
                     "GBSeq_primary-accession",
                     "GBSeq_accession-version",
                     "GBSeq_source",
                     "GBSeq_organism",
                     "GBSeq_taxonomy")
  } else if(.gbseq == "manual") {
    stop("not implemented now")
  }
  # get lev_gbseq.
  repl_gbseq <- PatColnames(.x = lev_gbeq)
  gbseq <- supmap_dfr(gbseq_list, MapGetValue, .names = lev_gbeq) %>%
    dplyr::rename_with(stringr::str_replace_all,
                       .cols = dplyr::everything(),
                       pattern = repl_gbseq)

  # parse references_list.
  lev_ref <- list("GBReference_title",
                  "GBReference_journal")
  repl_ref <- PatColnames(.x = lev_ref)
  references <- supmap_dfr(references_list, MapGetValue, .names = lev_ref) %>%
    dplyr::rename_with(stringr::str_replace_all,
                       .cols = dplyr::everything(),
                       pattern = repl_ref)

  # parse feature_table_list.
  feature_table <- feature_table_list %>%
    tibble::enframe(name = "id") %>%
    tidyr::unnest(value) %>%
    dplyr::mutate(name = names(value)) %>%
    dplyr::mutate(name2 = supmap(value, names),
                  value2 = supmap(value, unlist)) %>%
    tidyr::unnest(c(name2, value2)) %>%
    tidyr::pivot_wider(names_from = "name2", values_from = "value2") %>%
    dplyr::mutate(GBQualifier_value = dplyr::if_else(
      is.na(GBQualifier_value), GBQualifier_name, GBQualifier_value
    )) %>%
    dplyr::select(id, GBQualifier_name, GBQualifier_value) %>%
    tidyr::pivot_wider(names_from = "GBQualifier_name", values_from = "GBQualifier_value")

  # parse other_seqids_list.
  other_seqids <- other_seqids_list %>%
    tibble::enframe(name = "id") %>%
    dplyr::mutate(value2 = supmap(value, unlist),
                  value2 = supmap(value2, paste0, collapse = "_")) %>%
    tibble::add_column(name2 = "GBSeqid") %>%
    tidyr::unnest(c(name2, value2)) %>%
    dplyr::select(id, name2, value2) %>%
    tidyr::pivot_wider(names_from = "name2", values_from = "value2") %>%
    dplyr::select(-id)

  # check row number.
  if(nrow(gbseq) != size | nrow(references) != size | nrow(feature_table) != size | nrow(other_seqids) != size) {
    stop("row number is not correct")
  }
  # bind all data.
  rlt <- dplyr::bind_cols(
    gbseq, other_seqids, references, feature_table
  ) %>%
    dplyr::mutate(
      dplyr::across(where(is.list), as.character),
      dplyr::across(where(is.character), stringr::str_replace, "NULL", NA_character_)
    )
  return(rlt)
}

# split by GBSet.
SplitGBset <- function(.list, .num) {
  rlt <- .list$GBSet[.num]
  rlt <- purrr::flatten(rlt)
  return(rlt)
}

# get xml text value.
GetValue <- function(.list, .name) {
  rlt <- unlist(.list[[.name]])
  if(is.null(rlt)) {
    rlt <- NA
  }
  return(rlt)
}

# multiple name.
MapGetValue <- function( .list, .names, .f = GetValue) {
  supmap_dfc(.names, GetValue, .list = .list)
}

# make colname pattern.
PatColnames <- function(.x) {
  len <- length(.x)
  colname <- paste0("...", rev(1:len))
  rlang::set_names(nm = colname,
                   x = rev(unlist(.x)))
}

