#' Extract ITS region by using ITSx
#'
#' @param .raw_fasta path of fasta files.
#' @param .marker_region marker region (ITS1, ITS2, 5.8S, ITS, SSU, LSU, all).
#' @param .save_dir save directory.
#' @param .db_name names for database.
#' @param .type return marker region only or full length sequences.
#' @param .nthread number of thread
#' @export
db_construct_blastdb <- function(.raw_fasta,
                                 .marker_region,
                                 .save_dir,
                                 .db_name,
                                 .type = c("full", "marker"),
                                 .nthread = 10) {
  # make blast command.
  cmd <- "cd blastdb;makeblastdb -dbtype nucl -parse_seqids -in {.db_name} -logfile make_blastdb.log -max_file_sz '4GB'"
  cmd <- glue::glue(cmd)
  cat(cmd)
  system(command = cmd, intern = TRUE)
  fs::dir_ls("blastdb")
}
