#This code is used to make the initial dataframe 'df'.
pivot_questions <- function(.data){
  .data %>% 
    rename(
      regeling_1 = regeling1,
      regeling_2 = regeling2,
      regeling_3 = regeling3,
      complex1_1 = V03,
      complex2_1 = V04,
      complex3_1 = V05,
      complex1_2 = V24,
      complex2_2 = V25,
      complex3_2 = V26,
      complex1_3 = V34,
      complex2_3 = V35,
      complex3_3 = V36, 
      comm1_1 = V06,
      comm2_1 = V07,
      comm3_1 = V08,
      comm4_1 = V09,
      comm5_1 = V10,
      comm1_2 = V27,
      comm2_2 = V28,
      comm3_2 = V29,
      comm4_2 = V30,
      comm5_2 = V31,
      comm1_3 = V37,
      comm2_3 = V38,
      comm3_3 = V39,
      comm4_3 = V40,
      comm5_3 = V41,
      life_1 = V11,
      life_2 = V32,
      life_3 = V42,
      terugvordering_1 = V22,
      terugvordering_2 = V33,
      terugvordering_3 = V43, 
      terugvorderingschuld_1 = V22_2,
      terugvorderingschuld_2 = V33_2,
      terugvorderingschuld_3 = V43_2, 
    ) %>% 
    select(nomem_encr, starts_with('regeling'), starts_with('com'), starts_with('life'), starts_with('terugvordering')) %>%
    pivot_longer(cols = regeling_1:terugvorderingschuld_3,
                 names_to = c('.value', 'regeling_nr'),
                 names_pattern = '(.*)_(.*)') %>% 
    mutate(across(terugvordering:terugvorderingschuld, ~ as_factor(.))) %>% 
    select(-regeling_nr) %>% 
    filter(regeling != "Geen regeling") %>% 
    ao_uitsplitsen()
}

ao_uitsplitsen <- function(.data){
  ao <- read.csv("data/L_IBO_socialezekerheid_rapport_2.0p.csv") %>% 
    select(nomem_encr, IVA, WGA, WAO) %>% 
    mutate(
      regeling_ao = ifelse(WAO == 1, "WAO", NA),
      regeling_ao = ifelse(IVA == 1, "IVA", regeling_ao),
      regeling_ao = ifelse(WGA == 1, "WGA", regeling_ao)
    ) %>% select(-WAO, -IVA, -WGA)
  
  df <- .data %>% left_join(ao) %>% mutate(
    regeling = ifelse(!is.na(regeling_ao) & regeling == "WGA, IVA of WAO", regeling_ao, regeling) 
  ) %>% 
    select(
      -regeling_ao
    ) %>% filter(regeling != "WGA, IVA of WAO")
}

persoonskenmerken_toevoegen <- function(.data){
  pm <- read_sav("data/L_IBO_socialezekerheid_rapport_2.0p.sav") %>% 
    select(nomem_encr, nettoink, Agreeableness, Conscientiousness, Extraversion, Neuroticism, OpennessExperience, regelingenjaar, V01, woonvorm, woning, oplmet, V12, V13, V15, V16, V18, V46) %>% 
    rename(aantal_inkomstenbronnen = V01, 
           voorspelbaar_inkomen = V12, 
           verplichtingen_kennen = V13,
           werken_naast_regeling = V15,
           meer_inkomen_werk = V16,
           hulp_vinden_werk = V18, 
           bewust_niet_gebruik = V46) %>% 
    mutate(across(aantal_inkomstenbronnen:bewust_niet_gebruik,
                  as_factor)) %>% 
    mutate(stapeling = ifelse(is.na(regelingenjaar), 1, 0)) %>% 
    select(-regelingenjaar)
  
  df <- .data %>% left_join(pm) 
}


beleidsaspecten_koppelen <- function(.data){
  beleidsaspecten <- readxl::read_xlsx('data/voorwaarden_regelingen.xlsx', col_names = T) %>% select(-Partnerinkomenstoets)
  x <- colnames(beleidsaspecten)
  colnames(beleidsaspecten) <- c('Regeling', 'a', 'b', 'c', 'd', 'e', 'h', 'i', 'j', 'k', 'l', 'm')
  beleidsaspecten = beleidsaspecten %>% 
    mutate(k = ifelse(k == "Deels verrekend", "Deels / met uitzonderingen", k),
           k = ifelse(k == "Volledig verrekend, tenzij tijdelijk gedeeltelijk niet/uitzondering", "Deels / met uitzonderingen", k)) %>% 
    mutate(across(c:m, factor))  %>% 
    rename("type" = "a",
           "aantal_voorwaarden" = "b",
           "inkomenstoets" = "c",
           "gebaseerd_dagloon" = "d",
           'vermogenstoets' = "e",
           "verrekening_gebruik_restverdiencapaciteit" = "h",
           "arbeidsverplichtingen" = "i",
           "voorschotten_systematiek" = "j",
           "consequenties_bijverdienen" = "k",
           "hoogte_verandert_tijd" = "l",
           "herlevingsrecht_na_uitstroom" = "m")
  beleidsaspecten$inkomenstoets = relevel(factor(beleidsaspecten$inkomenstoets), ref = "Nee")
  beleidsaspecten$gebaseerd_dagloon = relevel(as.factor(beleidsaspecten$gebaseerd_dagloon), ref = "Nee")
  beleidsaspecten$vermogenstoets = relevel(as.factor(beleidsaspecten$vermogenstoets), ref = "Nee")
  beleidsaspecten$verrekening_gebruik_restverdiencapaciteit = relevel(as.factor(beleidsaspecten$verrekening_gebruik_restverdiencapaciteit), ref = "Nee")
  beleidsaspecten$arbeidsverplichtingen = relevel(as.factor(beleidsaspecten$arbeidsverplichtingen), ref = "Nee")
  beleidsaspecten$voorschotten_systematiek = relevel(as.factor(beleidsaspecten$voorschotten_systematiek), ref = "Nee")
  beleidsaspecten$consequenties_bijverdienen = relevel(as.factor(beleidsaspecten$consequenties_bijverdienen), ref = "Geen")
  beleidsaspecten$hoogte_verandert_tijd = relevel(as.factor(beleidsaspecten$hoogte_verandert_tijd), ref = "Nee")
  beleidsaspecten$herlevingsrecht_na_uitstroom = relevel(as.factor(beleidsaspecten$herlevingsrecht_na_uitstroom), ref = "Gegarandeerd herlevingsrecht als je aan bepaalde voorwaarden voldoet")
  
  df <- .data %>% 
    mutate(regeling = recode(regeling,
                             "Anw (Algemene nabestaandenwet)" = "Anw", 
                             "AOW (Algemene Ouderdomswet)" = "AOW",
                             "Bijstand (Participatiewet)" = "Bijstand",
                             'Bijstand voor zelfstandigen' = "Bbz",
                             "IOW (Inkomensvoorziening Oudere Werklozen)" = "IOW",
                             "Kinderopvangtoeslag" = "Kinderopvangtoeslag",
                             "WW of wachtgeld" = "WW",
                             "Ziektewetuitkering" = "Ziektewet")) %>% 
    left_join(beleidsaspecten, by = c('regeling'='Regeling'))
}
