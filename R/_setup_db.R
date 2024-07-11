# library(tidyverse)
# library(googlesheets4)
#
# df <- read_sheet("https://docs.google.com/spreadsheets/d/1ch1UfmoOL_J4dohx0xrwi-Ta9RUQTIhsvsicQ4YdX0I/edit?gid=0#gid=0") |>
#   janitor::clean_names() |>
#   mutate(
#     alfabetical_prefix_num = case_when(
#       str_detect(accession_prefix, "-") ~ (str_length(accession_prefix) - 1)/2,
#       .default = str_length(accession_prefix)
#     ),
#     alfabetical_prefix_first_letter = str_extract(accession_prefix, "^[A-Z]"),
#     alfabetical_prefix_pat = case_when(
#       str_detect(accession_prefix, "-") ~ paste0(alfabetical_prefix_first_letter, "[A-Z]{", (alfabetical_prefix_num - 1), "}"),
#       .default = accession_prefix
#     ),
#     numeric_suffix_num = str_extract_all(accession_format, "\\+\\d"),
#     numeric_suffix_num = map(numeric_suffix_num, str_remove_all, "\\+"),
#     numeric_suffix_num = case_when(
#       str_detect(accession_format, "or more") ~ map(numeric_suffix_num, ~paste0("\\d{", .x, ",}")),
#       .default = map(numeric_suffix_num, ~paste0("\\d{", .x, "}"))
#     ),
#     numeric_suffix_pat = map_chr(numeric_suffix_num, paste0, collapse = "|"),
#     accession_pat = case_when(
#       str_detect(numeric_suffix_pat, "\\|") ~ paste0("^", alfabetical_prefix_pat, "(", numeric_suffix_pat, ")", "$"),
#       .default = paste0("^", alfabetical_prefix_pat, numeric_suffix_pat, "$"),
#     )
#   ) |>
#   select(accession_prefix, insdc_partner, sequence_type, accession_format, accession_pat)
#
# write_sheet(data = df, ss = gs4_create("accession_patterns"))
# acc_table <- df
# usethis::use_data(acc_table, overwrite = TRUE)
