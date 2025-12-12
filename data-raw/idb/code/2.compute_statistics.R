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

library(ggrepel)

comparison |>
  filter(source == "official", measure == "total", statistic == "median") |>
  mutate(year = as.numeric(year), texto = round(value, 2)) |>
  ggplot(aes(x = year, y = value, label = texto)) +
  geom_col() +
  scale_x_continuous(breaks = seq(2018, 2025)) +
  geom_text(vjust = -1) +
  theme_minimal(20) +
  labs(x = "Year", y = "Median disposition time")

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

histogram_of_duration <- da_report |>
  mutate(
    year = as.numeric(lubridate::year(filing_date))
  ) |>
  filter(disposition_date <= "2025-03-31", disposition_date >= "2024-03-31") |>
  dplyr::filter(!NOS %in% nos_exclude) |>
  dplyr::mutate(
    category = dplyr::case_when(
      PROCPROG %in% c(1, 3) ~ "No Court Action",
      PROCPROG %in% c(2, 4, 10, 11, 12) ~ "Before Pretrial",
      PROCPROG == 5 ~ "During or After Pretrial",
      PROCPROG %in% c(6, 7, 8, 9, 13) ~ "During Trial",
      TRUE ~ NA_character_
    ),
    time_scaled = (time/365.25)*12
  ) |>
  filter(time_scaled <= 100) |>
  ggplot(aes(x = time_scaled)) +
  geom_histogram(color = 'white', fill = "black", alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  theme_minimal(20) +
  labs(x = "Disposition time of cases disposed between march 2024 and march 2025",
       y = "Number of cases")

ggsave(plot = histogram_of_duration, filename = "data-raw/idb/figures/figure4.png")

# só os casos de 2020 estão demorndo e dando os 24 meses da conta de cima.

da_report |>
  mutate(
    year = as.numeric(lubridate::year(filing_date))
  ) |>
  filter(disposition_date <= "2025-03-31", disposition_date >= "2024-03-31") |>
  dplyr::filter(!NOS %in% nos_exclude) |>
  dplyr::mutate(
    category = dplyr::case_when(
      PROCPROG %in% c(1, 3) ~ "No Court Action",
      PROCPROG %in% c(2, 4, 10, 11, 12) ~ "Before Pretrial",
      PROCPROG == 5 ~ "During or After Pretrial",
      PROCPROG %in% c(6, 7, 8, 9, 13) ~ "During Trial",
      TRUE ~ NA_character_
    ),
    time_scaled = (time/365.25)*12
  ) |>
  group_by(NOS == '365') |>
  summarise(
    tempo = median(time_scaled)
  )

  filter(time_scaled <= 100) |>
  ggplot(aes(x = time_scaled)) +
  geom_histogram(color = 'white', fill = "black", alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  theme_minimal(20) +
  labs(x = "Disposition time of cases disposed between march 2024 and march 2025",
       y = "Number of cases") +
  facet_wrap(~circuit, scales = 'free')

library(ggfortify)

survival_curve <- da_report |>
    mutate(
      year = as.numeric(lubridate::year(filing_date))
    ) |>
    filter(filing_date >= as.Date("2020-01-01")) |>
    dplyr::filter(!NOS %in% nos_exclude) |>
    dplyr::mutate(
      category = dplyr::case_when(
        PROCPROG %in% c(1, 3) ~ "No Court Action",
        PROCPROG %in% c(2, 4, 10, 11, 12) ~ "Before Pretrial",
        PROCPROG == 5 ~ "During or After Pretrial",
        PROCPROG %in% c(6, 7, 8, 9, 13) ~ "During Trial",
        TRUE ~ NA_character_
      ),
      time_scaled = (time/365.25)*12
    ) |>
    filter(NOS != '365') |>
    survival::survfit(
      survival::Surv(time_scaled, evento) ~ 1,
      data = _
    ) |>
    autoplot() +
    theme_minimal(20) +
    geom_vline(xintercept = 8.5, linetype = 2) +
    scale_x_continuous(breaks = c(0, 8.5, 20, 40, 60)) +
    labs(x = "Time after filing date (in months)", y = "Survival probability of a lawsuit")

ggsave(plot = survival_curve, filename = "data-raw/idb/figures/figure5.png")

survival_curve_years <- da_report |>
  mutate(
    year = as.factor(lubridate::year(filing_date))
  ) |>
  filter(filing_date >= as.Date("2020-01-01")) |>
  dplyr::filter(!NOS %in% nos_exclude) |>
  dplyr::mutate(
    category = dplyr::case_when(
      PROCPROG %in% c(1, 3) ~ "No Court Action",
      PROCPROG %in% c(2, 4, 10, 11, 12) ~ "Before Pretrial",
      PROCPROG == 5 ~ "During or After Pretrial",
      PROCPROG %in% c(6, 7, 8, 9, 13) ~ "During Trial",
      TRUE ~ NA_character_
    ),
    time_scaled = (time/365.25)*12
  ) |>
  filter(NOS != '365') |>
  survival::survfit(
    survival::Surv(time_scaled, evento) ~ year,
    data = _
  ) |>
  autoplot() +
  theme_minimal(20) +
  geom_vline(xintercept = 8.5, linetype = 2) +
  scale_x_continuous(breaks = c(0, 8.5, 20, 40, 60)) +
  labs(
    color = "Filing year", fill = "Filing year",
    x = "Time after filing date (in months)", y = "Survival probability of a lawsuit")

ggsave(plot = survival_curve_years, filename = "data-raw/idb/figures/figure6.png")


survival_curve_circuits <- da_report |>
  mutate(
    year = as.factor(lubridate::year(filing_date))
  ) |>
  filter(filing_date >= as.Date("2020-01-01")) |>
  dplyr::filter(!NOS %in% nos_exclude) |>
  dplyr::mutate(
    category = dplyr::case_when(
      PROCPROG %in% c(1, 3) ~ "No Court Action",
      PROCPROG %in% c(2, 4, 10, 11, 12) ~ "Before Pretrial",
      PROCPROG == 5 ~ "During or After Pretrial",
      PROCPROG %in% c(6, 7, 8, 9, 13) ~ "During Trial",
      TRUE ~ NA_character_
    ),
    time_scaled = (time/365.25)*12
  ) |>
  filter(NOS != '365') |>
  survival::survfit(
    survival::Surv(time_scaled, evento) ~ circuit,
    data = _
  ) |>
  autoplot() +
  theme_minimal(20) +
  geom_vline(xintercept = 8.5, linetype = 2) +
  scale_x_continuous(breaks = c(0, 8.5, 20, 40, 60)) +
  labs(
    color = "Filing year", fill = "Filing year",
    x = "Time after filing date (in months)", y = "Survival probability of a lawsuit")

ggsave(plot = survival_curve_circuits, filename = "data-raw/idb/figures/figure7.png")

# Cox

model_data <- da_report |>
  filter(filing_date >= as.Date("2020-01-01")) |>
  mutate(
    year = as.factor(lubridate::year(filing_date))
  ) |>
  dplyr::filter(!NOS %in% nos_exclude) |>
  dplyr::mutate(
    personal_injury = (NOS == '365'),
    category = dplyr::case_when(
      PROCPROG %in% c(1, 3) ~ "No Court Action",
      PROCPROG %in% c(2, 4, 10, 11, 12) ~ "Before Pretrial",
      PROCPROG == 5 ~ "During or After Pretrial",
      PROCPROG %in% c(6, 7, 8, 9, 13) ~ "During Trial",
      TRUE ~ NA_character_
    ),
    time_scaled = (time/365.25)*12
  )

regression_table <- model_data |>
  coxph(
    survival::Surv(time_scaled, evento) ~ circuit + personal_injury + year,
    data = _
  )

cox.zph(regression_table)

stargazer::stargazer(regression_table, single.row = TRUE)

stargazer(regression_table)
