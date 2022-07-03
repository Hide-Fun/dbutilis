#' Set Analysis
#'
#' @param ... `...`
#' @export
db_set_analysis <- function(...) {
  fs::dir_create(here::here("accession"))
  fs::dir_create(here::here("blastdb"))
  fs::dir_create(here::here("metadata"))
  fs::dir_create(here::here("ITSx"))
  fs::dir_create(here::here("formatted"))
}
