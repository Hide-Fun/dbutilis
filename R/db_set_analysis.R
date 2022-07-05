#' Set Analysis
#'
#' @param .target_dir target directory.
#' @export
db_set_analysis <- function(.target_dir) {
  cat(cli::col_br_blue(cli::style_bold("set directory for analysis.\n")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/accession")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/blastdb")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/db")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/metadata")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/ITSx")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/formatted")))
  cat(cli::col_br_blue(cli::style_bold(glue::glue("{dir}.\n", dir = fs::dir_ls(.target_dir, type = "directory")))))
}
