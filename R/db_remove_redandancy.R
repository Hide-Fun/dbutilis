#' Clustering.
#'
#' @param .work_dir working directory.
#' @param .marker_region maker
#' @param .ref_db_path UCHIME reference db path
#'
#' @export
db_remove_redundany <- function(.work_dir = "phylogeny",
                                .fas,
                                .centroid = "clustered.fas",
                                .consensus = "consensus.fas",
                                .save_dir = "cluster_dir",
                                .id = 0.98) {
  cmd <- glue::glue("cd {.work_dir}; vsearch --cluster_size {.fas} -id {.id} --centroids {.centroid} --consout {.consensus} --clusters clusters; mkdir -p {.save_dir}; mv clusters* {.save_dir}")
  cat(cli::col_br_blue(cli::style_bold(glue::glue("\nremove redundancy.\n"))))
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
  # remove \n.
  cmd <- glue::glue('for file in `ls {.work_dir}/{.save_dir}/`; do
    seqkit seq -w 0 {.work_dir}/{.save_dir}/"$file" > temp.fas && mv temp.fas {.work_dir}/{.save_dir}/"$file".fas
done')
  cat("\n")
  cat(cmd)
  cat("\n")
  system(command = cmd, intern = TRUE)
}
