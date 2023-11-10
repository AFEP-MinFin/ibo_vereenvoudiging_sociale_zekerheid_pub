hoeveelheid_regelingen <- function(totaal_names){
  z <- df %>%
    group_by(!!ensym(totaal_names)) %>%
    summarise(n = n()) %>%
    mutate(n = ifelse(n<11, NA, n)) %>% 
    rename(
      'hoeveelheid' = totaal_names,
      totaal_names = "n"
    )
  # Remove cells with 10 or less observations for CBS privacy
}

hoeveelheid_regelingen_tabel <- function(.data) {
  totaal_names <- names(df)[23:25]
  
  aantallen <- bind_cols(lapply(totaal_names, hoeveelheid_regelingen))
}
