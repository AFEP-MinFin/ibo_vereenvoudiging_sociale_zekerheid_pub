sm = read_sav('L:/211130 Stapelingsmonitor SZW 2019 V3_selectie9198.sav',
                       col_select = c(RINPERSOONS, RINPERSOON,
                                      huishoudnr_2019,
                                      leeftijd_2019)) %>% filter(leeftijd_2019 > 16)

df_hh <- sm %>% select(-leeftijd_2019) %>% right_join(df %>% select(starts_with('RIN'), starts_with('totaal')))
df_hh_n <- df %>% group_by(huishoudnr_2019) %>% summarise(n = n(),
                                                           n_totaal = sum(totaal > 0, na.rm = T),
                                                           n_totaal_excl_toeslag_kind = sum(totaal_excl_toeslag_kind > 0, na.rm = T),
                                                           n_totaal_excl_toeslag = sum(totaal_excl_toeslag > 0, na.rm = T)
                                                           )
df_hh_reg <- df %>% left_join(sm) %>% group_by(huishoudnr_2019) %>%
  summarise_at(vars(Bijstand:AKW), sum)

df_huishoudens <- df_hh_n %>% full_join(df_hh_reg)

df_huishoudens <- df_huishoudens %>%
  mutate(perc_totaal = n_totaal / n,
         perc_totaal_excl_toeslag_kind = n_totaal_excl_toeslag_kind / n,
         perc_totaal_excl_toeslag = n_totaal_excl_toeslag / n) %>% filter(n < 11)
#write_feather(df_huishoudens, "data/huishoudens.feather")

#In hoeveel huishoudens hebben meerdere mensen sociale zekerheid? ####
meerdere_mensen_in_hh_met_regeling = df_huishoudens %>% hh_meerdere_mensen_met_regeling() %>%
  mutate(perc_regelingen = n_regelingen / sum(n_regelingen),
         perc_excl_ts = n_excl_ts / sum(n_excl_ts),
         perc_excl_ts_kind = n_excl_ts_kind / sum(n_excl_ts_kind))

write.xlsx(meerdere_mensen_in_hh_met_regeling, "Export/meerdere_mensen_in_hh_met_regeling.xlsx")

#Verdeling van hoeveel mensen stapeling hebben ####
stapeling_hh = df_hh_n %>%
  group_by(n, n_totaal) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count < 11, NA, count)) %>%
  ungroup() %>%
  group_by(n) %>%
  mutate(perc = count / sum(count, na.rm = T))

stapeling_hh_excl_ts = df_hh_n %>% filter(n < 11) %>%
  group_by(n, n_totaal_excl_toeslag) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count < 11, NA, count)) %>%
  ungroup() %>%
  group_by(n) %>%
  mutate(perc = count / sum(count, na.rm = T))

stapeling_hh_regelingen = df_hh_n %>% filter(n < 11) %>%
  group_by(n, n_totaal_excl_toeslag_kind) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count < 11, NA, count)) %>%
  ungroup() %>%
  group_by(n) %>%
  mutate(perc = count / sum(count, na.rm = T))

stapeling_hh_sum = stapeling_hh %>%
  full_join(stapeling_hh_excl_ts, by = c("n" = "n", "n_totaal" = "n_totaal_excl_toeslag"), suffix = c('_alles', '')) %>%
  full_join(stapeling_hh_regelingen, by = c("n" = "n", "n_totaal" = "n_totaal_excl_toeslag_kind"), suffix = c('_excl_ts', '_alleen_uitkeringen')) %>%
  filter(n < 3)

writexl::write_xlsx(stapeling_hh_sum, 'Export/hoeveelheid_regelingen_huishoudens_kleine_hh.xlsx')

#Interactie binnen huishouden
df_interactie = df_huishoudens %>%
  select(AIO, Bbz, IOAZ, Bijstand, TW, ANW, IOAW, n, everything())
df_interactie$totaal = rowSums(df_interactie[,2:7])
df_interactie$totaal_met_ts = rowSums(df_interactie[,c(2:7, 21:24)])


interactie = df_interactie %>% filter(n_totaal_excl_toeslag_kind > 0) %>%
  filter(n < 3) %>%
  group_by(n, totaal) %>% summarise(count = n()) %>%
  ungroup() %>%
  group_by(n) %>%
  mutate(perc = count / sum(count, na.rm = T))

interactie_ts = df_interactie %>% filter(n_totaal_excl_toeslag_kind > 0) %>%
  filter(n < 3) %>%
  group_by(n, totaal_met_ts) %>% summarise(count = n()) %>%
  ungroup() %>%
  group_by(n) %>%
  mutate(perc = count / sum(count, na.rm = T))

interactie_ts_zonder_filter = df_interactie %>% filter(n_totaal_excl_toeslag_kind > 0) %>%
  filter(n < 11 & n > 1) %>%
  group_by(n, totaal_met_ts) %>% summarise(count = n())

sum(interactie$count)

writexl::write_xlsx(interactie, 'Export/Interactie_hh.xlsx')

#Hoeveel regelingen zitten er in 1 huishouden?
#Even checken: willen we 'hoeveel unieke regelingen' of 'hoeveel regelingen, de dubbele meenemend'?
df_huishoudens$totaal <- rowSums(df_huishoudens[,6:25])
df_huishoudens$totaal_excl_toeslag <- rowSums(df_huishoudens[,c(6:20, 23, 25)])
df_huishoudens$totaal_excl_toeslag_kind <- rowSums(df_huishoudens[,6:20])

hh_sum_totaal = df_huishoudens %>% group_by(n, totaal) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count < 11, NA, count)) %>%
  filter(n < 11)

hh_sum_totaal_excl_toeslag = df_huishoudens %>% group_by(n, totaal_excl_toeslag) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count < 11, NA, count)) %>%
  filter(n < 11)

hh_sum_totaal_excl_toeslag_kind = df_huishoudens %>% group_by(n, totaal_excl_toeslag_kind) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count < 11, NA, count)) %>%
  filter(n < 11)

hh_sum = hh_sum_totaal %>%
  full_join(hh_sum_totaal_excl_toeslag, by = c('n' = 'n', 'totaal' = 'totaal_excl_toeslag'),
            suffix = c('_alles', '')) %>%
  full_join(hh_sum_totaal_excl_toeslag_kind, by = c('n' = 'n', 'totaal' = 'totaal_excl_toeslag_kind'),
            suffix = c('_excl_ts', '_excl_ts_kind'))

write.xlsx(hh_sum, 'Export/hh_hoeveel_regelingen.xlsx')

#Welke combinaties aan regelingen komen veel voor? ####
df_hh_dummy = df_huishoudens %>% mutate(across(Bijstand:AKW, ~ ifelse(. > 1, 1, .)))
hh_combinaties <- df_hh_dummy %>% meest_voorkomende_combinaties("Bijstand", "toeslag", 'Ja')

write.xlsx(hh_combinaties, "Export/combinaties_huishoudens.xlsx")

hh_uitvoerders = df_hh_dummy %>% aantal_uitvoerders()
hh_welke_uitvoerders = hh_uitvoerders %>%
  group_by(Belastingdienst, UWV, SVB, Gemeenten) %>%
  summarise(n = n()) %>% mutate(n = ifelse(n <11, NA, n))

write.xlsx(hh_welke_uitvoerders, "Export/welke_uitvoerders_huishoudens.xlsx")

#hoeveel stapeling binnen huishouden
df_huishoudens =


#Bijstand_stapeling
bijstand_hh = df_huishoudens %>% rename(huishoudens_grootte = n) %>%
  group_by(huishoudens_grootte, Bijstand) %>% summarise(n = n()) %>%
  mutate(n = ifelse(n < 11, NA, n))

write.xlsx(bijstand_hh, "Bijstand_stapeling.xlsx")

n_totaal_excl_toeslag
hh_meerdere_mensen_met_regeling <- function(.data){
  hh_totaal_1 = df_huishoudens %>% select(n_totaal) %>% mutate(cat = case_when(n_totaal == 0 ~ "Geen regeling",
                                                                               n_totaal == 1 ~ 'Één regeling',
                                                                               n_totaal > 1 ~ "Twee of meer regelingen")) %>%
    group_by(cat) %>%
    summarise(n_regelingen = n())
  hh_totaal_2 = df_huishoudens %>% select(n_totaal_excl_toeslag) %>% mutate(cat = case_when(n_totaal_excl_toeslag == 0 ~ "Geen regeling",
                                                                                            n_totaal_excl_toeslag == 1 ~ 'Één regeling',
                                                                                            n_totaal_excl_toeslag > 1 ~ "Twee of meer regelingen")) %>%
    group_by(cat) %>%
    summarise(n_excl_ts = n())
  hh_totaal_3 = df_huishoudens %>% select(n_totaal_excl_toeslag_kind) %>% mutate(cat = case_when(n_totaal_excl_toeslag_kind == 0 ~ "Geen regeling",
                                                                                                 n_totaal_excl_toeslag_kind == 1 ~ 'Één regeling',
                                                                               n_totaal_excl_toeslag_kind > 1 ~ "Twee of meer regelingen")) %>%
    group_by(cat) %>% summarise(n_excl_ts_kind = n())

  hh_totaal = hh_totaal_1 %>% full_join(hh_totaal_2, by = "cat") %>% full_join(hh_totaal_3, by = "cat")
}

hh_stapeling <- df_huishoudens %>% hh_meerdere_mensen_met_regeling()

write.xlsx(hh_stapeling, "Huishoudens_stapeling.xlsx")
