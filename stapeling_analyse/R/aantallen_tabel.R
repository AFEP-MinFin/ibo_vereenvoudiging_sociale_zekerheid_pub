#Per regeling berekenen in hoeveel andere regelingen individuen zitten####
stapeling_elke_regeling <- function(.data) {
  .data %>%
    summarise(
      across(Bijstand:AKW, sum, na.rm = T),
      Totaal = n()
    )
}

get_aantallen_voor_regeling <- function(regeling) {
  df %>%
    filter(!!ensym(regeling) == 1) %>%
    stapeling_elke_regeling() %>% mutate(Regeling = regeling)
}

#tabel: hoeveel personen in regeling X zitten ook in regeling Y? ####
aantallen_tabel <- function(.data) {
  regelingen <- colnames(df)[4:23]

  aantallen <- bind_rows(lapply(regelingen, get_aantallen_voor_regeling))

  aantallen <- aantallen %>% select(Regeling, Totaal, everything())
}

