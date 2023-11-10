meest_voorkomende_combinaties <- function(.data, eerste_var, laatste_var, toeslag) {
  if (toeslag == "Scope"){
    z <- .data %>% filter(totaal > 1 & totaal_excl_toeslag_kind > 0) %>%
      #mutate & select: om alle toeslagen onder 1 variabel 'toeslag' te zetten, als je ook naar de combinaties van toeslagen wilt kijken, kan je dit weghalen
      select(-contains("totaal"),
             -Huurtoeslag,
             -Zorgtoeslag,
             -Kindgebondenbudget,
             -Kinderopvangtoeslag) %>%
      #de 0en en 1en uit de dummytabel worden aan elkaar geplakt in colom 'combo' op basis van de input eerste_var en laatste_var
      unite(col = "combo", .data[[ eerste_var ]]:.data[[ laatste_var ]]) %>%
      group_by(combo) %>%
      summarise(n = n()) %>%
      mutate(n = ifelse(n<11, NA, n)) %>% filter(!is.na(n))

    vars = names(df)[c(4:18, 23)]
    z[,3:18] = str_split_fixed(z$combo, "_", 16)
    colnames(z)[3:18] <- vars
  }
  if (toeslag == "Ja"){
    z <- .data %>% filter(totaal > 2 & totaal_excl_toeslag_kind > 1) %>%
      #mutate & select: om alle toeslagen onder 1 variabel 'toeslag' te zetten, als je ook naar de combinaties van toeslagen wilt kijken, kan je dit weghalen
      mutate(toeslag = ifelse(
        Kinderopvangtoeslag == 1 |
          Kindgebondenbudget == 1 |
          Huurtoeslag == 1 |
          Zorgtoeslag == 1 |
          AKW == 1,
        1, 0)) %>%
      select(-contains("totaal"),
             -Huurtoeslag,
             -Zorgtoeslag,
             -Kindgebondenbudget,
             -Kinderopvangtoeslag,
             -AKW) %>%
      #de 0en en 1en uit de dummytabel worden aan elkaar geplakt in colom 'combo' op basis van de input eerste_var en laatste_var
      unite(col = "combo", .data[[ eerste_var ]]:.data[[ laatste_var ]]) %>%
      group_by(combo) %>%
      summarise(n = n()) %>%
      mutate(n = ifelse(n<11, NA, n)) %>% filter(!is.na(n))

    vars = names(df)[c(4:18)]
    vars[16] = "toeslag"
    z[,3:18] = str_split_fixed(z$combo, "_", 16)
    colnames(z)[3:18] <- vars
  }

  if (toeslag == "Nee"){
    z <- .data %>% filter(totaal > 2 & totaal_excl_toeslag_kind > 1) %>%
      #mutate & select: om alle toeslagen onder 1 variabel 'toeslag' te zetten, als je ook naar de combinaties van toeslagen wilt kijken, kan je dit weghalen
      select(-contains("totaal"),
             -Huurtoeslag,
             -Zorgtoeslag,
             -Kindgebondenbudget,
             -Kinderopvangtoeslag,
             -AKW) %>%
      #de 0en en 1en uit de dummytabel worden aan elkaar geplakt in colom 'combo' op basis van de input eerste_var en laatste_var
      unite(col = "combo", .data[[ eerste_var ]]:.data[[ laatste_var ]]) %>%
      group_by(combo) %>%
      summarise(n = n()) %>%
      mutate(n = ifelse(n<11, NA, n)) %>% filter(!is.na(n))

    vars = names(df)[c(4:18)]
    z[,3:17] = str_split_fixed(z$combo, "_", 15)
    colnames(z)[3:17] <- vars
  }

  if (toeslag == "Alle"){
    z <- .data %>% filter(totaal > 2 & totaal_excl_toeslag_kind > 1) %>%
      #de 0en en 1en uit de dummytabel worden aan elkaar geplakt in colom 'combo' op basis van de input eerste_var en laatste_var
      unite(col = "combo", .data[[ eerste_var ]]:.data[[ laatste_var ]]) %>%
      group_by(combo) %>%
      summarise(n = n()) %>%
      mutate(n = ifelse(n<11, NA, n)) %>% filter(!is.na(n))

    vars = names(df)[c(4:23)]
    z[,3:22] = str_split_fixed(z$combo, "_", 20)
    colnames(z)[3:22] <- vars
  }

  #namen van dataframe z aanpassen naar de regeling namen

  #als iemand in een regeling zit, wordt de 1 vervangen door de naam vd regeling
  row = nrow(z)
  col = ncol(z)
  for (i in 1:row) {
    for (j in 3:col) {
      if (z[i,j] == "1") {
        z[i,j] <- colnames(z)[j]
      }
    }
  }

  #plakken we de namen aan elkaar
  z = z %>% unite(col = "combo", Bijstand:ANW)
  z$combinatie = gsub("\\_0", "", z$combo) #verwijderen van alle 0en
  z$combinatie = gsub("0_", "", z$combinatie)
  z <- z %>% select(combinatie, n)
  return(z)
}
