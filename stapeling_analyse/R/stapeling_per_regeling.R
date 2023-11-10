#gemiddeld aantal regelingen
stapeling_per_regeling <- function(.data){
  .data %>%
    pivot_longer(
      cols = c(-starts_with('RIN'), -starts_with('huishoud'), -starts_with('totaal')),
      names_to = "Regeling",
      values_to = "Dummy"
    ) %>%
    group_by(Regeling, Dummy) %>%
    summarise(
      mean = mean(totaal),
      mean_excl_ts = mean(totaal_excl_toeslag),
      mean_excl_ts_kind = mean(totaal_excl_toeslag_kind),
      median = median(totaal),
      median_excl_ts = median(totaal_excl_toeslag),
      median_excl_ts_kind = median(totaal_excl_toeslag_kind),
      n = n()
    )
}
