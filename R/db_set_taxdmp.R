#' Donwload taxdmp.
#'
#' @param .db_dir directory.
#' @param .override override old one.
#' @param .save_to_parquet convert to parquet format.
#' @export
db_set_taxdmp <- function(.db_dir = "db",
                          .override = TRUE,
                          .save_to_parquet = TRUE) {
  target_dir <- here::here(glue::glue("{.db_dir}"))
  if(fs::dir_exists(target_dir) && .override) {
    fs::file_delete(fs::dir_ls(target_dir))
  }
  fs::dir_create(path = target_dir)
  cat(cli::col_br_blue(cli::style_bold("getting taxdump_readme.txt\n")))
  path <- here::here(glue::glue("{.db_dir}/taxdump_readme.txt"))
  # download database.
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/new_taxdump/taxdump_readme.txt",
    httr::write_disk(path, overwrite = .override),
    httr::progress("down")
  )
  cat(cli::col_br_blue(cli::style_bold("getting new_taxdump.tar.gz\n")))
  path_taxdmp <- here::here(glue::glue("{.db_dir}/new_taxdump.tar.gz"))
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/new_taxdump/new_taxdump.tar.gz",
    httr::write_disk(path_taxdmp, overwrite = .override),
    httr::progress("down")
  )
  cat(cli::col_br_blue(cli::style_bold("getting new_taxdump.tar.gz.md5")))
  path_taxdmp_md5sum <- here::here(glue::glue("{.db_dir}/new_taxdump.tar.gz.md5"))
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/new_taxdump/new_taxdump.tar.gz.md5",
    httr::write_disk(path_taxdmp_md5sum, overwrite = .override),
    httr::progress("down")
  )
  # check md5sum.
  if(!tools::md5sum(path_taxdmp) == stringr::str_remove(readr::read_lines(path_taxdmp_md5sum), "  new_taxdump\\.tar\\.gz$")) {
    stop(cat(cli::col_br_red(cli::style_bold("md5sum check fault.\n"))))
  } else {
    cat(cli::col_br_blue(cli::style_bold("md5sum successfully checked.\n")))
  }
  # decompress.
  untar(path_taxdmp, exdir = target_dir)
  # convert to parquet.
  if(.save_to_parquet == TRUE) {
    day <- Sys.Date()
    type <- c("names", "nodes", "rankedlineage", "host")
    path <- paste0(target_dir, "/", type, ".dmp")
    datas <- purrr::map2(path, type, read_taxdmp2)
    purrr::walk2(datas, stringr::str_replace(path, "\\.dmp", glue::glue("_{day}.parquet")), arrow::write_parquet)
    fs::file_delete(fs::dir_ls(target_dir, glob = "*.dmp"))
  }
  return(cli::col_br_blue("taxdmp successfully downloaded and formatted.\n"))
}

read_taxdmp2 <- function(.path, .type) {
  # load data,
  data <- readr::read_delim(.path, delim = "\t|\t", col_names = F) %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_remove, "\\\t\\|"))
  if(.type == "names") {
    # set column names.
    col_name_list <- c("tax_id", "name_txt", "unique_name", "name_class")
    col_name_pat <- purrr::set_names(
      nm = sprintf("X%d", 1:ncol(data)),
      x = col_name_list
    ) %>% rev()
    d <- data %>%
      dplyr::rename_with(.fn = stringr::str_replace_all, .cols = dplyr::everything(), col_name_pat)
  } else if(.type == "nodes") {
    # set column names.
    col_name_list <- c("tax_id", "parent_tax_id", "rank", "embl_code", "division_id", "inherited_div_flag", "genetic_code_id", "inherited_GC_flag", "mitochondrial_genetic_code_id", "inherited_MGC_flag", "GenBank_hidden_flag", "hidden_subtree_root_flag", "comments", "plastid_genetic_code_id", "inherited_PGC_flag", "specified_species", "hydrogenosome_genetic_code_id", "inherited_HGC_flag")
    col_name_pat <- purrr::set_names(
      nm = sprintf("X%d", 1:ncol(data)),
      x = col_name_list
    ) %>% rev()
    d <- data %>%
      dplyr::rename_with(.fn = stringr::str_replace_all, .cols = dplyr::everything(), col_name_pat) %>%
      dplyr::mutate(inherited_HGC_flag = as.numeric(inherited_HGC_flag))
  } else if(.type == "rankedlineage") {
    # set column names.
    col_name_list <- c("tax_id", "tax_name", "species", "genus", "family", "order", "class", "phylum", "kingdom", "superkingdom")
    col_name_pat <- purrr::set_names(
      nm = sprintf("X%d", 1:ncol(data)),
      x = col_name_list
    ) %>% rev()
    d <- data %>%
      dplyr::rename_with(.fn = stringr::str_replace_all, .cols = dplyr::everything(), col_name_pat)
  } else if(.type == "host") {
    # set column names.
    col_name_list <- c("tax_id", "potential_hosts")
    col_name_pat <- purrr::set_names(
      nm = sprintf("X%d", 1:ncol(data)),
      x = col_name_list
    ) %>% rev()
    d <- data %>%
      dplyr::rename_with(.fn = stringr::str_replace_all, .cols = dplyr::everything(), col_name_pat)
  }
  return(d)
}
