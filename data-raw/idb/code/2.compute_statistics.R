# complete case -----------------------------------------------------------

# esses microdados aqui estão tentando replicar o numero oficial. quase dá certo, mas erra um pouco

da_report |>
  dplyr::filter(disposition_date >= "2023-04-01", disposition_date <= "2024-03-31") |>
  dplyr::group_by(a = PROCPROG %in% c(1, 3)) |>
  dplyr::summarise(
    freq = dplyr::n(),
    time = median(time/30))

da_report |>
  dplyr::filter(disposition_date >= "2024-04-01", disposition_date <= "2025-03-31") |>
  dplyr::group_by(a = PROCPROG %in% c(1, 3)) |>
  dplyr::summarise(
    freq = dplyr::n(),
    time = median(time/30))

# numeros oficiais

uscourts_dataset |>
  dplyr::filter(stringr::str_detect(file, "2024|2025"))

# survival ----------------------------------------------------------------

modelo <- survival::survfit(survival::Surv(time, evento) ~ year, data = da_report |> dplyr::mutate(year = factor(lubridate::year(filing_date))))

modelo

# só os casos de 2020 estão demorndo e dando os 24 meses da conta de cima.
