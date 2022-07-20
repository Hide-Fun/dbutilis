#' Donwload accession2taxid_
#'
#' @param .db_dir directory.
#' @param .override override old one.
#' @param .save_to_parquet convert to parquet format.
#' @export
db_set_acc2taxid_ <- function(.db_dir,
                             .override = TRUE,
                             .save_to_parquet = TRUE) {
  target_dir <- here::here(glue::glue("{.db_dir}"))
  if(fs::dir_exists(target_dir) && .override) {
    fs::file_delete(fs::dir_ls(target_dir))
  }
  fs::dir_create(path = target_dir)
  # load database.
  cat(cli::col_br_blue(cli::style_bold("getting readme.txt\n")))
  path <- here::here(glue::glue("{.db_dir}/readme.txt"))
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/accession2taxid/README",
    httr::write_disk(path, overwrite = .override),
    httr::progress("down")
  )
  cat(cli::col_br_blue(cli::style_bold("getting nucl_gb.accession2taxid.gz\n")))
  path_gb <- here::here(glue::glue("{.db_dir}/nucl_gb.accession2taxid.gz"))
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/accession2taxid/nucl_gb.accession2taxid.gz",
    httr::write_disk(path_gb, overwrite = .override),
    httr::progress("down")
  )
  cat(cli::col_br_blue(cli::style_bold("getting nucl_gb.accession2taxid.gz.md5\n")))
  path_gb_md5sum <- here::here(glue::glue("{.db_dir}/nucl_gb.accession2taxid.gz.md5"))
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/accession2taxid/nucl_gb.accession2taxid.gz.md5",
    httr::write_disk(path_gb_md5sum, overwrite = .override),
    httr::progress("down")
  )
  cat(cli::col_br_blue(cli::style_bold("getting nucl_wgs.accession2taxid.gz\n")))
  path_wgs <- here::here(glue::glue("{.db_dir}/nucl_wgs.accession2taxid.gz"))
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/accession2taxid/nucl_wgs.accession2taxid.gz",
    httr::write_disk(path_gb, overwrite = .override),
    httr::progress("down")
  )
  cat(cli::col_br_blue(cli::style_bold("getting nucl_wgs.accession2taxid.gz.md5\n")))
  path_wgs_md5sum <- here::here(glue::glue("{.db_dir}/nucl_wgs.accession2taxid.gz.md5"))
  httr::GET(
    url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/accession2taxid/nucl_wgs.accession2taxid.gz.md5",
    httr::write_disk(path_gb_md5sum, overwrite = .override),
    httr::progress("down")
  )
  # check md5sum.
  if(!tools::md5sum(path_gb) == stringr::str_remove(readr::read_lines(path_gb_md5sum), "  nucl_gb\\.accession2taxid\\.gz$")) {
    stop(cat(cli::col_br_red(cli::style_bold("md5sum check fault.\n"))))
  } else {
    cat(cli::col_br_blue(cli::style_bold("md5sum successfully checked.\n")))
  }
  if(!tools::md5sum(path_wgs) == stringr::str_remove(readr::read_lines(path_wgs_md5sum), "  nucl_wgs\\.accession2taxid\\.gz$")) {
    stop(cat(cli::col_br_red(cli::style_bold("md5sum check fault.\n"))))
  } else {
    cat(cli::col_br_blue(cli::style_bold("md5sum successfully checked.\n")))
  }
  # decompress.
  R.utils::decompressFile(
    filename = path_gb,
    destname = stringr::str_replace(path_gb, "\\.gz", ".tsv"),
    ext = "gz",
    FUN = gzfile
  )
  R.utils::decompressFile(
    filename = path_wgs,
    destname = stringr::str_replace(path_wgs, "\\.gz", ".tsv"),
    ext = "gz",
    FUN = gzfile
  )
  # convert to parquet.
  if(.save_to_parquet == TRUE) {
    day <- Sys.Date()
    acc2taxid_gb <- arrow::read_tsv_arrow(file = stringr::str_replace(path_gb, "\\.gz", ".tsv"))
    acc2taxid_wgs <- arrow::read_tsv_arrow(file = stringr::str_replace(path_wgs, "\\.gz", ".tsv"))
    arrow::write_parquet(acc2taxid_gb, here::here(glue::glue("{.db_dir}/acc2taxid_gb_{day}.parquet")))
    arrow::write_parquet(acc2taxid_wgs, here::here(glue::glue("{.db_dir}/acc2taxid_wgs_{day}.parquet")))
    fs::file_delete(stringr::str_replace(path_wgs, "\\.gz", ".tsv"))
  }
  return(cat(cli::col_br_blue(cli::style_bold("acc2taxid successfully downloaded and formatted.\n"))))
}

