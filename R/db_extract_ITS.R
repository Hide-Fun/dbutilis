#' Extract ITS region by using ITSx
#'
#' @param .raw_fasta path of fasta files.
#' @param .marker_region marker region (ITS1, ITS2, 5.8S, ITS, SSU, LSU).
#' @param .save_dir save directory.
#' @param .type return marker region only or full length sequences.
#' @param .nthread number of thread
#' @export
db_extract_ITS <- function(.raw_fasta,
                           .marker_region,
                           .save_dir = "ITSx",
                           .type = c("full", "marker"),
                           .nthread = 10) {
  # make ITSx command.
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\nextract {.marker_region} region by ITSx.\n"))))
  raw_fasta <- here::here(.raw_fasta)
  save_dir <- here::here(.save_dir)
  cmd <- "cd {save_dir};ITSx -i {raw_fasta} --date T --reset T -t F --multi_thread T --detailed_results T --cpu {.nthread} --save_regions all"
  cmd <- glue::glue(cmd)
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  # load fasta.
  if(.marker_region == "5.8S") {
    .marker_region <- "5_8S"
    path <- fs::dir_ls(here::here(glue::glue("{.save_dir}")), regexp = glue::glue(".{.marker_region}."))
  } else if(.marker_region == "ITS") {
    .marker_region <- "full"
    path <- fs::dir_ls(here::here(glue::glue("{.save_dir}")), regexp = glue::glue(".{.marker_region}."))
  } else {
    path <- fs::dir_ls(here::here(glue::glue("{.save_dir}")), regexp = glue::glue(".{.marker_region}."))
  }
  fas <- readr::read_tsv(path, col_names = FALSE) %>%
    fasta_to_table() %>%
    dplyr::mutate(otu = stringr::str_extract(otu, "^[^|]+"))
  #
  if(.type == "full") {
    # load raw fasta.
    raw_fasta <- readr::read_tsv(.raw_fasta, col_names = FALSE) %>%
      fasta_to_table()
    fas_cleaned <- dplyr::semi_join(raw_fasta, fas, by = "otu")
  } else {
    fas_cleaned <- fas
  }
  # readr::write_tsv(fasta_to_table(fas_cleaned, otu, sequence),
  #                  here::here(glue::glue("blastdb/{.db_name}")),
  #                  col_names = FALSE)
  fs::dir_ls(save_dir)
  return(fas_cleaned)
}
