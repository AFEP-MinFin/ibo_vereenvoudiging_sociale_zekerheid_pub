gewogen_complexiteit_score <- function(.data){
  beleidsaspecten <- readxl::read_xlsx('data/voorwaarden_regelingen.xlsx', col_names = T)
  x <- colnames(beleidsaspecten)
  colnames(beleidsaspecten) <- c('Regeling', 'a', 'b', 'c', 'd', 'e', 'g', 'h', 'i', 'j', 'k', 'l', 'm')
  beleidsaspecten = beleidsaspecten %>% 
    mutate(k = ifelse(k == "Deels verrekend", "Deels / met uitzonderingen", k),
           k = ifelse(k == "Volledig verrekend, tenzij tijdelijk gedeeltelijk niet/uitzondering", "Deels / met uitzonderingen", k))
  
  beleidsaspecten$c = dummy.code(beleidsaspecten$c, 'Ja')
  beleidsaspecten$d = dummy.code(beleidsaspecten$d, 'Ja')
  beleidsaspecten$e = dummy.code(beleidsaspecten$e, 'Ja')
  beleidsaspecten$g = dummy.code(beleidsaspecten$g, 'Ja')
  beleidsaspecten$h = dummy.code(beleidsaspecten$h, 'Ja')
  beleidsaspecten$i = dummy.code(beleidsaspecten$i, 'Ja')
  beleidsaspecten$j = dummy.code(beleidsaspecten$j, 'Ja')
  beleidsaspecten$k_1 = dummy.code(beleidsaspecten$k, 'Deels / met uitzonderingen')
  beleidsaspecten$k_2 = dummy.code(beleidsaspecten$k, 'Volledig verrekend')
  beleidsaspecten$l = dummy.code(beleidsaspecten$l, 'Ja')
  beleidsaspecten$m = dummy.code(beleidsaspecten$m, 'Geen')
  beleidsaspecten = beleidsaspecten %>% select(-k) %>% 
    rename("type" = "a",
           "complexiteit_score" = "b",
           "inkomenstoets" = "c",
           "gebaseerd_dagloon" = "d",
           'vermogenstoets' = "e",
           "partnerinkomenstoets" = "g",
           "verrekening_gebruik_restverdiencapaciteit" = "h",
           "arbeidsverplichtingen" = "i",
           "voorschotten_systematiek" = "j",
           "consequenties_bijverdienenDeels" = "k_1",
           "consequenties_bijverdienenVolledigverrekend" = "k_2",
           "hoogte_verandert_tijd" = "l",
           "herlevingsrecht_na_uitstroom" = "m")
  
  gewogen_scores = shuffle_summary %>% select(aspect, median, median_sig)
  weight_inkomenstoets = gewogen_scores$median[grepl('inkomenstoets', gewogen_scores$aspect)][1]
  weight_gebaseerd_dagloon = gewogen_scores$median[grepl('gebaseerd_dagloon', gewogen_scores$aspect)]
  weight_vermogenstoets = gewogen_scores$median[grepl('vermogenstoets', gewogen_scores$aspect)]
  weight_partnerinkomenstoets = gewogen_scores$median[grepl('partnerinkomenstoets', gewogen_scores$aspect)]
  weight_verrekening_gebruik_restverdiencapaciteit = gewogen_scores$median[grepl('verrekening_gebruik_restverdiencapaciteit', gewogen_scores$aspect)]
  weight_arbeidsverplichtingen = gewogen_scores$median[grepl('arbeidsverplichtingen', gewogen_scores$aspect)]
  weight_voorschotten_systematiek = gewogen_scores$median[grepl('voorschotten_systematiek', gewogen_scores$aspect)]
  weight_hoogte_verandert_tijd = gewogen_scores$median[grepl('hoogte_verandert_tijd', gewogen_scores$aspect)]
  weight_herlevingsrecht_na_uitstroom = gewogen_scores$median[grepl('herlevingsrecht_na_uitstroom', gewogen_scores$aspect)]
  weight_consequenties_bijverdienenDeels = gewogen_scores$median[grepl("consequenties_bijverdienenDeels / met uitzonderingen", gewogen_scores$aspect)]
  weight_consequenties_bijverdienenVolledigverrekend = gewogen_scores$median[grepl('consequenties_bijverdienenVolledig verrekend', gewogen_scores$aspect)]
  
  
  
  beleidsaspecten = beleidsaspecten %>% mutate(
    inkomenstoets = ifelse(inkomenstoets == 1, inkomenstoets*weight_inkomenstoets, 0),
    gebaseerd_dagloon = ifelse(gebaseerd_dagloon == 1, gebaseerd_dagloon*weight_gebaseerd_dagloon, 0),
    vermogenstoets = ifelse(vermogenstoets == 1, vermogenstoets*weight_vermogenstoets, 0),
    partnerinkomenstoets = ifelse(partnerinkomenstoets == 1, partnerinkomenstoets*weight_partnerinkomenstoets, 0),
    verrekening_gebruik_restverdiencapaciteit = ifelse(verrekening_gebruik_restverdiencapaciteit == 1, verrekening_gebruik_restverdiencapaciteit*weight_verrekening_gebruik_restverdiencapaciteit, 0),
    arbeidsverplichtingen = ifelse(arbeidsverplichtingen == 1, arbeidsverplichtingen*weight_arbeidsverplichtingen, 0),
    voorschotten_systematiek = ifelse(voorschotten_systematiek == 1, voorschotten_systematiek*weight_voorschotten_systematiek, 0),
    hoogte_verandert_tijd = ifelse(hoogte_verandert_tijd == 1, hoogte_verandert_tijd*weight_hoogte_verandert_tijd, 0),
    herlevingsrecht_na_uitstroom = ifelse(herlevingsrecht_na_uitstroom == 1, herlevingsrecht_na_uitstroom*weight_herlevingsrecht_na_uitstroom, 0),
    consequenties_bijverdienenDeels = ifelse(consequenties_bijverdienenDeels == 1, consequenties_bijverdienenDeels*weight_consequenties_bijverdienenDeels, 0),
    consequenties_bijverdienenVolledigverrekend = ifelse(consequenties_bijverdienenVolledigverrekend == 1, consequenties_bijverdienenVolledigverrekend*weight_consequenties_bijverdienenVolledigverrekend, 0)
  )
  
  significante_aspecten = gewogen_scores$aspect[gewogen_scores$median_sig == 1]
  
  beleidsaspecten$weighted_complexiteit = rowSums(beleidsaspecten[,4:14], na.rm = T)
  beleidsaspecten$weighted_complexiteit_sign = rowSums(beleidsaspecten[,c(7, 8, 11, 14, 15)], na.rm = T)
  beleidsaspecten = beleidsaspecten %>% select(Regeling, weighted_complexiteit, weighted_complexiteit_sign)
  
  df = .data  %>% left_join(beleidsaspecten, by = c('regeling' = 'Regeling'))
}
df = df %>% gewogen_complexiteit_score()
summ_complexiteit = df %>% group_by(regeling) %>% 
  summarise(
    aantal_voorwaarden = mean(aantal_voorwaarden, na.rm = T),
    complexiteit = mean(complexiteit, na.rm = T),
    complexiteit_pm = mean(complexiteit_pm, na.rm = T),
    gewogen_complexiteit = mean(weighted_complexiteit, na.rm =T),
    gewogen_complexiteit_sig = mean(weighted_complexiteit_sign, na.rm = T)
  )

colnames(beleidsaspecten)[significante_aspecten %in% colnames(beleidsaspecten)]

summ_complexiteit %>% ggplot(aes(y = aantal_voorwaarden, x = complexiteit)) + geom_point() + geom_smooth(method = 'lm')
summ_complexiteit %>% ggplot(aes(y = aantal_voorwaarden, x = complexiteit_pm)) + geom_point() + geom_smooth(method = 'lm')

summ_complexiteit %>% ggplot(aes(x = complexiteit, y= gewogen_complexiteit)) + geom_point() + geom_smooth(method = 'lm')
summ_complexiteit %>% ggplot(aes(x = complexiteit_pm, y= gewogen_complexiteit)) + geom_point() + geom_smooth(method = 'lm')

summ_complexiteit %>% ggplot(aes(y = complexiteit, x= gewogen_complexiteit_sig)) + geom_point() + geom_smooth(method = 'lm')
summ_complexiteit %>% ggplot(aes(y = complexiteit_pm, x= gewogen_complexiteit_sig)) + geom_point() + geom_smooth(method = 'lm')

