#' Get metadata as XML format
#'
#' @param .acc accession ids.
#' @param .dest_dir save path.
#' @param .n_max maximum number of chunks.
#' @param .sleep sleep time.
#' @param .try_max number of trials.
#' @export
db_get_meta <- function(.acc, .dest_dir, .n_max, .sleep, .try_max = NULL) {
  acc <- .acc
  # if dose not exist, create save directory.
  if(fs::dir_exists(here::here(.dest_dir)) == FALSE) {
    fs::dir_create(here::here(.dest_dir))
  }
  # split accession ids.
  acc_list <- split(acc, ceiling(seq_along(acc)/.n_max))
  # accession id
  .ids <- purrr::map(acc_list, paste0, collapse = ",")
  # set query.
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id={.ids}&rettype=gb&retmode=xml"
  use_urls <- unlist(purrr::map(base_url, ~glue::glue(.x, .ids = .ids)))
  path_list <- unlist(purrr::map(1:length(use_urls), ~paste0(.dest_dir, "/", .x, ".xml")))
  # download.
  res <- promap2_chr(use_urls, path_list, ~curl_download2(.x, .y, .sleep = .sleep))
  # trials.
  if(!is.null(.try_max)) {
    # make empty list.
    res_list <- vector(mode = "list", length = .try_max + 1)
    res_list[[1]] <- res
    for (i in 1:.try_max) {
      if(length(which(res_list[[i]] == "fault")) != 0) {
        # update queries.
        use_urls <- use_urls[which(res_list[[i]] == "fault")]
        path_list <- path_list[which(res_list[[i]] == "fault")]
        res_list[[i + 1]] <- promap2_chr(use_urls, path_list, ~curl_download2(.x, .y, .sleep = .sleep))
        res <- res_list[[i + 1]]
      } else {
        res <- res_list[[i]]
        use_urls <- use_urls[which(res_list[[i]] == "fault")]
        path_list <- path_list[which(res_list[[i]] == "fault")]
        break
      }
    }
  }
  return(tibble2(result = res, use_urls = use_urls, path_list = path_list))
}
