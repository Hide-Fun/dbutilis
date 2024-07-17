#' Remove chimeric sequences.
#'
#' @param .save_dir directory for save.
#' @param .marker_region maker
#' @param .ref_db_path UCHIME reference db path
#'
#' @export
db_remove_chimera <- function(.save_dir = "phylogeny",
                              .marker_region = c("full", "ITS1", "ITS2", "LSU", "SSU"),
                              .ref_db_path) {
  # move ITSx result.
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\nprepare {.marker_region} sequences.\n"))))
  save_dir <- here::here(.save_dir)
  cmd <- "cat ITSx/ITSx_out.*.{.marker_region}.fasta > {.save_dir}/{.marker_region}.fas"
  cmd <- glue::glue(cmd)
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  # remove chimeras by UCHIME DB.
  cmd <- glue::glue("vsearch --uchime_ref {.save_dir}/{.marker_region}.fas --db {.ref_db_path} --nonchimeras {.save_dir}/ref_nonchimeras.fas --chimeras {.save_dir}/ref_chimeras.fas")
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\nremove chimeras by UCHIME DB.\n"))))
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  cmd <- glue::glue("vsearch --uchime_denovo {.save_dir}/ref_nonchimeras.fas --nonchimeras {.save_dir}/nonchimeras.fas --chimeras {.save_dir}/denovo_chimeras.fas")
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\nremove chimeras de novo.\n"))))
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  cmd <- glue::glue("seqkit seq -w 0 {.save_dir}/nonchimeras.fas > temp.fas && mv temp.fas {.save_dir}/nonchimeras.fas")
  system(command = cmd, intern = TRUE)
}
