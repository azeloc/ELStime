# complete case -----------------------------------------------------------

# esses microdados aqui estão tentando replicar o numero oficial. quase dá certo, mas erra um pouco

get_statistics <- function(year) {
  da_filter <- da_report |>
    dplyr::filter(
      disposition_date >= glue::glue("{year-1}-04-01"),
      disposition_date <= glue::glue("{year}-03-31")
    )

  # Exclude per Table C5 note: land condemnations, prisoner petitions,
  # deportation reviews, recovery of overpayments, and enforcement of judgments
  nos_exclude <- c(
    210, # Land Condemnation
    150, # Overpayments & Enforcement of Judgments
    151, # Overpayments under the Medicare Act
    152, # Recovery of Defaulted Student Loans
    153, # Recovery of Overpayments of Vet Benefits
    460, # Deportation
    #463, # Habeas Corpus - Alien Detainee (?)
    510, # Prisoner Petitions - Vacate Sentence
    530, # Prisoner Petitions - Habeas Corpus
    535, # Habeas Corpus - Death Penalty
    540, # Prisoner Petitions - Mandamus and Other
    550, # Prisoner - Civil Rights
    555 # Prisoner - Prison Condition
    # 560 # Civil Detainee (?)
  )
  da_filter |>
    dplyr::filter(!NOS %in% nos_exclude) |>
    dplyr::mutate(
      category = dplyr::case_when(
        PROCPROG %in% c(1, 3) ~ "No Court Action",
        PROCPROG %in% c(2, 4, 10, 11, 12) ~ "Before Pretrial",
        PROCPROG == 5 ~ "During or After Pretrial",
        PROCPROG %in% c(6, 7, 8, 9, 13) ~ "During Trial",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(median_total = median(time / (365.25 / 12))) |>
    dplyr::group_by(category) |>
    dplyr::summarise(
      freq = dplyr::n(),
      time = median(time / (365.25 / 12)),
      median_total = unique(median_total)
    ) |>
    dplyr::summarise(
      total_cases = sum(freq),
      total_median = unique(median_total),
      nocourt_cases = sum(freq[category == "No Court Action"]),
      nocourt_median = time[category == "No Court Action"],
      before_cases = sum(freq[category == "Before Pretrial"]),
      before_median = time[category == "Before Pretrial"],
      duringafter_cases = sum(freq[category == "During or After Pretrial"]),
      duringafter_median = time[category == "During or After Pretrial"],
      during_cases = sum(freq[category == "During Trial"]),
      during_median = time[category == "During Trial"]
    )
}

# Compute statistics for each year
results <- 2018:2025 |>
  purrr::set_names() |>
  purrr::map(get_statistics) |>
  purrr::list_rbind(names_to = "year")

# Compare with official numbers (totals)
comparison <- results |>
  dplyr::inner_join(
    uscourts_totals,
    by = "year",
    suffix = c("_calc", "_official")
  ) |>
  tidyr::pivot_longer(
    cols = -c(year, file),
    names_to = c("measure", "statistic", "source"),
    names_sep = "_",
    values_to = "value"
  ) |>
  dplyr::select(-file)

# plotting comparison ----------------------------------------------
comparison |>
  ggplot2::ggplot(ggplot2::aes(
    x = year,
    y = value,
    colour = source,
    group = source
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(measure ~ statistic, scales = "free_y", nrow = 5) +
  ggplot2::labs(
    title = "Comparison of Calculated vs Official US Courts Table C5 Statistics",
    x = "Year",
    y = "Value",
    colour = "Source"
  ) +
  ggplot2::theme_bw()


# numeros oficiais

uscourts_totals |>
  dplyr::filter(stringr::str_detect(file, "2024|2025"))

# survival ----------------------------------------------------------------

modelo <- survival::survfit(
  survival::Surv(time, evento) ~ year,
  data = da_report |>
    dplyr::mutate(year = factor(lubridate::year(filing_date)))
)

modelo

# só os casos de 2020 estão demorndo e dando os 24 meses da conta de cima.
