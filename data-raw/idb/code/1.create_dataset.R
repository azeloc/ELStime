# baixar daqui https://www.fjc.gov/sites/default/files/idb/textfiles/cv88on.zip
da_report <- readr::read_delim("data-raw/idb/raw_data/cv88on.txt") |>
  dplyr::mutate(
    disposition_date = lubridate::mdy(TERMDATE),
    filing_date = lubridate::mdy(FILEDATE),
    time = ifelse(
      STATUSCD == "L", as.numeric(disposition_date-filing_date),
      as.numeric(as.Date("2025-07-25")-filing_date)
    ),
    evento = STATUSCD == "L",
    court_action = !(PROCPROG %in% c(1)),
    evento_competing = factor(dplyr::case_when(
      PROCPROG %in% c(1) ~ "no court action",
      STATUSCD == "L" ~ "court action",
      TRUE ~ "censor"
      ), labels = c("censor", "no court action", "court action")),
    CIRCUIT = as.character(CIRCUIT),
    JURIS = (stringr::str_detect(PLT, "COMMONWEALTH|USA|UNITED +STATES|STATE +OF|FEDERAL TRADE COMMISSIO") | stringr::str_detect(DEF, "COMMONWEALTH|USA|UNITED +STATES|STATE +OF|FEDERAL TRADE COMMISSIO"))
  ) |>
  dplyr::filter(filing_date >= "2002-01-01") |>
  dplyr::select(disposition_date, filing_date, time, evento, evento_competing, PROCPROG, court_action)
