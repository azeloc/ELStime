library(httr)
library(tidyverse)

# download_data -----------------------------------------------------------

urls <- stringr::str_glue("https://www.uscourts.gov/sites/default/files/data_tables/fjcs_c5_0331.{seq(2018,2024)}.xlsx")

purrr::walk(
  urls,
  function(u){
    httr::GET(
      u,
      httr::write_disk(
        stringr::str_glue("data-raw/idb/raw_data/{basename(u)}"),
        overwrite = TRUE
      ))
  }
)

# process tables ----------------------------------------------------------

files <- list.files("data-raw/idb/raw_data/", recursive = TRUE, full.names = TRUE, pattern = "fjcs")

uscourts_dataset <- purrr::map_dfr(files, function(f){readxl::read_excel(f)[6, c(2,3)] |> mutate(arquivo = f)}) |>
  purrr::set_names(c("cases_disposed", "median_time", "file")) |>
  mutate(
    cases_disposed = as.numeric(cases_disposed),
    median_time = as.numeric(median_time)
  )

usethis::use_data(uscourts_dataset, overwrite = TRUE)
