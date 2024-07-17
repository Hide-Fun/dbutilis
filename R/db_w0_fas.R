#' Clean.
#'
#' @param .fas input fasta.
#'
#' @export
db_w0_fas <- function(.fas) {
  cmd <- glue::glue("seqkit seq -w 0 {.fas} > temp.fas && mv temp.fas {.fas}")
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\nseqkit seq -w 0.\n"))))
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
}
