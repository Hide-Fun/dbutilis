#' Remove outliner.
#'
#' @param .save_dir save directory.
#' @param .dont_use_python use python or not.
#' @param .nthread number of threads
#'
#' @export
db_remove_outlier <- function(
    .save_dir = "phylogeny",
    .dont_use_python = T,
    .nthread = 10)
{
  # create msa
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\ngenerate MSA.\n"))))
  cmd <- glue::glue("mafft-fftns --thread {.nthread} {.save_dir}/nonchimeras.fas >  {.save_dir}/nonchimeras_fftnsi.fas")
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  # infer phylogenetic tree.
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\ninfer phylogenetic tree.\n"))))
  cmd <- glue::glue("fasttree -nt -gtr {.save_dir}/nonchimeras_fftnsi.fas > {.save_dir}/nonchimeras_fftnsi.nwk")
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  cat("\n")
  if (!.dont_use_python) {
    # detect outlier.
    cat(cli::col_br_blue(cli::style_bold(glue::glue("\ndetect outlier.\n"))))
    cmd <- glue::glue("cd {.save_dir}; run_treeshrink.py -t fftnsi.nwk")
    cat("\n")
    cat(cmd)
    system(command = "mamba activate treeshrink", intern = TRUE)
    system(command = cmd, intern = TRUE)
    system(command = "mamba deactivate", intern = TRUE)

    # remove outlier.
    outliner <- read_lines(glue::glue("{.save_dir}/fftnsi_treeshrink/output.txt")) |>
      str_extract_all("[A-Z]{1,3}\\d{5,8}") |>
      unlist()
    cat(cli::col_br_blue(cli::style_bold(glue::glue("\n{length(outliner)} outlier: {paste0(outliner, collapse = ',')}\n"))))
    cleaned <- readr::read_tsv(glue::glue("{.save_dir}/{.marker_region}.fas"), col_names = F) |>
      fasta_to_table() |>
      filter(!str_detect(otu, paste0(outliner, collapse = "|")))
    fs::dir_ls(save_dir)
    return(cleaned)
  } else {
  }
}
