#' Construct BLASTDB.
#'
#' @param .seq_path path of fasta files.
#' @param .save_dir save directory.
#' @export
db_construct_blastdb <- function(.seq_path,
                                 .save_dir = "blastdb") {
  # make blast command.
  cat(cli::col_br_blue(cli::style_bold("\nconstruct blast database.\n")))
  seq_path <- here::here(.seq_path)
  save_dir <- here::here(.save_dir)
  cmd <- "cd {save_dir};makeblastdb -dbtype nucl -parse_seqids -in {seq_path} -logfile make_blastdb.log -max_file_sz '4GB'"
  cmd <- glue::glue(cmd)
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  fs::dir_ls(save_dir)
}
