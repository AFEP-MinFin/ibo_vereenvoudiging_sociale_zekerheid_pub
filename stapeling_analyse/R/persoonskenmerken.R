#stap 1: koppel persoonskenmerken####
koppel_persoonskenmerken <- function(.data, kenmerken){
  persoonskenmerken <- read_sav('L:/211130 Stapelingsmonitor SZW 2019 V3_selectie9198.sav',
                                col_select = c(RINPERSOONS, RINPERSOON, kenmerken)) %>%
    filter(leeftijd_2019 > 17)

  df_pk <- .data %>% left_join(persoonskenmerken)
}
#stap 2: schrijf code voor elk kenmerk om het netjes te recoden ####
recode_pk_samen <- function(.data){
  .data %>% mutate(Geslacht = recode(as.character(geslacht_2019),
                                     "1" = "Man",
                                     "2" = "Vrouw"),
                   Opleiding = recode(as.character(hbopl_2019),
                                      "100" = "Laag ",
                                      "200" = "Middelbaar",
                                      "300" = 'Hoog',
                                      "---" = "Onbekend"),
                   Herkomst = recode(as.character(herkomst_2019),
                                     "0" = "Onbekend",
                                     "1" = "Nederland",
                                     "2" = "Turkije",
                                     "3" = "Marokko",
                                     "4" = "Suriname",
                                     "5" = "Nederlands Antillen en Aruba",
                                     "6" = "Poolse migratieachtergrond of MOE-lander",
                                     "7" = "Syrische migratieachtergrond",
                                     "8" = "Somalische migratieachtergrond",
                                     "9" = "Overige westerse migratieachtergrond",
                                     "10" = "Overige niet westerse migratieachtergrond"),
                   Stedelijkheid = recode(as.character(stedgem_2019),
                                          "1" = "Zeer sterk (>=2500 omgevingsadressen/km2)",
                                          "2" = "Sterk (1500 tot 2500 omgevingsadressen/km2)",
                                          "3" = "Matig (1000 tot 1500 omgevingsadressen/km2)",
                                          "4" = "Weinig (500 tot 1000 omgevingsadressen/km2)",
                                          "5" = "Niet (<500 omgevingsadressen/km2)",
                                          "9" = "Onbekend"),
                   Generatie = recode(as.character(generatie_2019),
                                      "-" = "Onbekend",
                                      "0" = "Persoon met een Nederlandse achtergrond",
                                      "1" = "Eerste generatie migratieachtergrond",
                                      "2" = "Tweede generatie migratieachtergrond"))  %>%
    select(-c(geslacht_2019, hbopl_2019, generatie_2019))
}

#stap 3: functie om samen te vatten####
samenvatting_kenmerk <- function(.data, groeperen, kenmerk){
  if (kenmerk == "Opleiding") {
    .data %>% group_by(!!ensym(groeperen), value, !!ensym(kenmerk)) %>%
      summarise(gew = sum(gewichtopl_2019, na.rm = T),
                n = n()) %>%
      filter(value == 1) %>%
      filter(Opleiding != "---") %>%
      group_by(!!ensym(groeperen)) %>%
      mutate(
        n = ifelse(n <11, NA, n),
        gew = ifelse(n <11, NA, gew),
        perc = gew/sum(gew, na.rm = T))
   #ggplot(aes(x = !!ensym(groeperen), y = perc, fill = Opleiding)) +
   #  geom_bar(position = "stack", stat = "identity")
  }
  else {
  .data %>% group_by(!!ensym(groeperen), value, !!ensym(kenmerk)) %>%
      summarise(n = n()) %>%
      filter(value == 1) %>%
      ungroup() %>%
      group_by(!!ensym(groeperen)) %>%
      mutate(n = ifelse(n <11, NA, n),
             perc = n/sum(n))
    #ggplot(aes(x = !!ensym(groeperen), y = perc, fill = !!ensym(kenmerk))) +
    #  geom_bar(position = "stack", stat = "identity")
  }
}
