#' Extract ITS region by using ITSx
#'
#' @param .raw_fasta path of fasta files.
#' @param .marker_region marker region (ITS1, ITS2, 5.8S, ITS, SSU, LSU, all).
#' @param .save_dir save directory.
#' @param .db_name names for output fasta files.
#' @param .type return marker region only or full length sequences.
#' @param .nthread number of thread
#' @export
db_extract_ITS <- function(.raw_fasta,
                           .marker_region,
                           .save_dir,
                           .db_name,
                           .type = c("full", "marker"),
                           .nthread = 10) {
  # make ITSx command.
  wd <- here::here()
  cmd <- "cd ITSx;ITSx -i {.raw_fasta} --date T --reset T -t F --multi_thread T --detailed_results T --cpu {.nthread} --save_regions {.marker_region}"
  cmd <- glue::glue(cmd)
  cat(cmd)
  system(command = cmd, intern = TRUE)
  # load fasta.
  fas <- readr::read_tsv(path, col_names = FALSE) %>%
    fasta_to_table() %>%
    dplyr::mutate(otu = stringr::str_extract(otu, "^[^|]+"))
  #
  if(.type == "full") {
    # load raw fasta.
    raw_fasta <- readr::read_tsv(.raw_fasta, col_names = FALSE) %>%
      fasta_to_table()
    fas_cleaned <- dplyr::semi_join(raw_fasta, fas)
  }
  readr::write_tsv(fasta_to_table(fas_cleaned, otu, sequence),
                   here::here(glue::glue("blastdb/{.db_name}")),
                   col_names = FALSE)
  fs::dir_ls("ITSx")
  return(fas_cleaned)
}
