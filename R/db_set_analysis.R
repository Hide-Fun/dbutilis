#' Set Analysis
#'
#' @param .target_dir target directory.
#' @export
db_set_analysis <- function(.target_dir) {
  fs::dir_create(here::here(glue::glue("{.target_dir}/accession")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/blastdb")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/metadata")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/ITSx")))
  fs::dir_create(here::here(glue::glue("{.target_dir}/formatted")))
}
