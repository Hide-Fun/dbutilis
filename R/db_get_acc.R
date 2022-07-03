# library(tidyverse)
# library(arrow)
#
# db_get_acc <- function(.taxon,
#                        .db_dir,
#                        .update_db = TRUE) {
#   if(.update_db == TRUE) {
#     target_dir <- here::here(glue::glue("{.db_dir}"))
#     if(fs::dir_exists(target_dir) == TRUE) {
#       fs::dir_delete(target_dir)
#     }
#     fs::dir_create(path = target_dir)
#     path <- here::here(glue::glue("{.db_dir}/taxdump.tar.gz"))
#     # download database.
#     curl::curl_download(
#       url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/new_taxdump/new_taxdump.tar.gz",
#       destfile = path
#     )
#     # decompress.
#     untar(path, exdir = target_dir)
#     # remove.
#     fs::file_delete(path = path)
#     fs::dir_ls(target_dir)
#     # parse data & save arrow format.
#     type <- c("names", "nodes", "rankedlineage", "host")
#     path <- paste0(target_dir, "/", type, ".dmp")
#     datas <- map2(path, type, read_taxdmp2)
#     walk2(datas, str_replace(path, "\\.dmp", ".parquet"), arrow::write_parquet)
#
#     # accession2taxid.
#     path2 <- here::here(glue::glue("{.db_dir}/nucl_wgs.accession2taxid.gz"))
#     curl::curl_download(
#       url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/accession2taxid/nucl_wgs.accession2taxid.gz",
#       destfile = path2
#     )
#     path3 <- here::here(glue::glue("{.db_dir}/nucl_gb.accession2taxid.gz"))
#     curl::curl_download(
#       url = "ftp.ncbi.nlm.nih.gov/pub/taxonomy/accession2taxid/nucl_gb.accession2taxid.gz",
#       destfile = path3
#     )
#     # decompress.
#     R.utils::decompressFile(
#       filename = path2,
#       destname = stringr::str_replace(path2, "\\.gz", ".tsv"),
#       ext = "gz",
#       FUN = gzfile
#     )
#     R.utils::decompressFile(
#       filename = path3,
#       destname = stringr::str_replace(path3, "\\.gz", ".tsv"),
#       ext = "gz",
#       FUN = gzfile
#     )
#     # load & save as arrow format.
#     nucl_wgs_accession2taxid <- arrow::read_tsv_arrow(stringr::str_replace(path2, "\\.gz", ".tsv"))
#     nucl_gb_accession2taxid <- arrow::read_tsv_arrow(stringr::str_replace(path3, "\\.gz", ".tsv"))
#     arrow::write_parquet(nucl_wgs_accession2taxid, stringr::str_replace(path2, "\\.gz", ".parquet"))
#     arrow::write_parquet(nucl_gb_accession2taxid, stringr::str_replace(path3, "\\.gz", ".parquet"))
#     # remove files.
#     fs::file_delete(
#       c(stringr::str_replace(path2, "\\.gz", ".tsv"),
#         stringr::str_replace(path3, "\\.gz", ".tsv"))
#     )
#   }
# }
#
#
#
#
# names <- arrow::read_parquet("db/names.parquet")
# rankedlineage <- arrow::read_parquet("db/rankedlineage.parquet")
# nodes <- arrow::read_parquet("db/nodes.parquet")
#
# # filter.
# tax_id <- names %>%
#   filter(name_txt == "Drosophila") %>%
#   pull(tax_id)
#
# tax_id <- names %>%
#   filter(name_txt == "Ceratobasidiaceae") %>%
#   pull(tax_id)
# df2 <- df %>% filter(taxid == tax_id)
#
# nucl_wgs.accession2taxid.gz
# nucl_gb.accession2taxid.gz
# Accession
# Accession.version
# TaxId
# GI
#
# library(arrow)
# accession2taxid_wgs <- read_tsv_arrow("nucl_wgs.accession2taxid.tsv")
# accession2taxid_gb <- read_tsv_arrow("nucl_gb.accession2taxid.tsv")
# accession2taxid_gb %>% class()
#
# write_parquet(accession2taxid_gb, "nucl_gb.accession2taxid.parquet")
# accession2taxid_gb <- read_parquet("nucl_gb.accession2taxid.parquet")
#
#
# con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
# copy_to(con, mtcars)
