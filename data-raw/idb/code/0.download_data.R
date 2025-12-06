library(httr)
library(tidyverse)

# download_data -----------------------------------------------------------

urls <- stringr::str_glue(
  "https://www.uscourts.gov/sites/default/files/data_tables/fjcs_c5_0331.{seq(2018,2024)}.xlsx"
)

purrr::walk(
  urls,
  function(u) {
    httr::GET(
      u,
      httr::write_disk(
        stringr::str_glue("data-raw/idb/raw_data/{basename(u)}"),
        overwrite = TRUE
      )
    )
  }
)

# process tables ----------------------------------------------------------

files <- fs::dir_ls("data-raw/idb/raw_data/", regexp = "fjcs")

uscourts_totals <- purrr::map(files, \(f) {
  nms <- c(
    "total_cases",
    "total_median",
    "nocourt_cases",
    "nocourt_median",
    "before_cases",
    "before_median",
    "duringafter_cases",
    "duringafter_median",
    "during_cases",
    "during_median"
  )
  readxl::read_excel(f, range = "B8:K8", col_names = nms) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
}) |>
  purrr::list_rbind(names_to = "file") |>
  dplyr::mutate(
    year = stringr::str_extract(file, "\\d{4}(?=\\.xlsx)"),
    .before = file
  )

usethis::use_data(uscourts_totals, overwrite = TRUE)
