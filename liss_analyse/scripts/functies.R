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
      life_3 = V42
    ) %>% 
    select(nomem_encr, starts_with('regeling'), starts_with('com'), starts_with('life')) %>%
    pivot_longer(cols = regeling_1:life_3,
                 names_to = c('.value', 'regeling_nr'),
                 names_pattern = '(.*)_(.*)') %>% 
    select(-regeling_nr) %>% 
    filter(regeling != "Geen regeling")  %>% 
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
  pm <- read.csv("data/L_IBO_socialezekerheid_rapport_2.0p.csv") %>% 
    select(nomem_encr, V01, woonvorm, woning, oplmet, nettoink, Agreeableness, Conscientiousness, Extraversion, Neuroticism, OpennessExperience, regelingenjaar) %>% 
    rename(aantal_inkomstenbronnen = V01) %>% 
    mutate(across(aantal_inkomstenbronnen:oplmet,
                  as_factor)) %>% 
    mutate(stapeling = ifelse(is.na(regelingenjaar), 1, 0),
           aantal_inkomstenbronnen = recode(aantal_inkomstenbronnen,
                                            "1" = "Inkomen van één persoon",
                                            "2" = "Inkomen van twee personen",
                                            "3" = "Inkomen van drie of meer personen"))
  
  df <- .data %>% left_join(pm) %>% select(-regelingenjaar)
}

make_graph_regeling <- function(.data, indicator, sort){
  
  x <- .data %>% select(regeling_graph, !!ensym(indicator)) %>% 
    mutate(indic = !!ensym(indicator)) %>% 
    group_by(regeling_graph, indic) %>% summarise(n = n()) %>% 
    filter(!is.na(regeling_graph)) %>% 
    ungroup() %>% 
    filter(!is.na(indic)) %>%  
    pivot_wider(values_from = n, names_from = indic)
#x %>% ggplot(aes(x = regeling_graph, y = perc, fill = indic)) + 
#  geom_bar(stat = "identity") +
#  scale_fill_manual(values = c("#005B8E","#5889B2","#A1b9d3","#f5d4b3","#e17000")) +
#  xlab("Regeling") + ylab("Percentage") +
#  labs(fill = "Antwoord")
  
  x$Eindtotaal = rowSums(x[,c(-1)], na.rm = T)
  return(x)
}

make_graph_dilemma <- function(.data, vraag){
  .data %>% group_by(regeling_graph) %>% summarise(
    vraag = mean(!!ensym(vraag), na.rm = T)) %>% 
    filter(!is.na(vraag)) %>% 
    ggplot(aes(x = reorder(regeling_graph, vraag), y = vraag)) + geom_bar(stat = "identity") +
    xlab('Regeling') + ylab('Gemiddeld antwoord op schaal van 0 tot 100')
}


beleidsaspecten_koppelen <- function(.data){
  beleidsaspecten <- readxl::read_xlsx('Kopie van Complexiteit indicator - 6 sept v2.xlsx', col_names = T)
  x <- colnames(beleidsaspecten)
  colnames(beleidsaspecten) <- c('Regeling', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm')
  beleidsaspecten = beleidsaspecten %>% 
    mutate(k = ifelse(k == "Deels verrekend", "Deels / met uitzonderingen", k),
           k = ifelse(k == "Volledig verrekend, tenzij tijdelijk gedeeltelijk niet/uitzondering", "Deels / met uitzonderingen", k)) %>% 
    mutate(across(c:m, factor))  %>% 
    rename("type" = "a",
           "aantal_voorwaarden" = "b",
           "inkomenstoets" = "c",
           "gebaseerd_dagloon" = "d",
           'vermogenstoets' = "e",
           "kostendelernorm_huishoudenstoets" = "f",
           "partnerinkomenstoets" = "g",
           "verrekening_gebruik_restverdiencapaciteit" = "h",
           "arbeidsverplichtingen" = "i",
           "voorschotten_systematiek" = "j",
           "consequenties_bijverdienen" = "k",
           "hoogte_verandert_tijd" = "l",
           "herlevingsrecht_na_uitstroom" = "m")
  beleidsaspecten$inkomenstoets = relevel(factor(beleidsaspecten$inkomenstoets), ref = "Nee")
  beleidsaspecten$gebaseerd_dagloon = relevel(as.factor(beleidsaspecten$gebaseerd_dagloon), ref = "Nee")
  beleidsaspecten$vermogenstoets = relevel(as.factor(beleidsaspecten$vermogenstoets), ref = "Nee")
  beleidsaspecten$kostendelernorm_huishoudenstoets = relevel(as.factor(beleidsaspecten$kostendelernorm_huishoudenstoets), ref = "Nee")
  beleidsaspecten$partnerinkomenstoets = relevel(as.factor(beleidsaspecten$partnerinkomenstoets), ref = "Nee")
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

