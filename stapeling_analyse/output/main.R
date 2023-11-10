#opzetten

library(haven)
library(tidyverse)
library(fastDummies)
library(feather)
library(openxlsx)

setwd("H:/Sociale zekerheid/socialezekerheid")

source("R/aantallen_tabel.R")
source("R/stapeling_per_regeling.R")
source("R/meest_voorkomende_combinaties.R")
source("R/output tabellen.R")
source("R/persoonskenmerken.R")
source("R/sociaal_domein.R")
source("R/type.R")

#inladen van stapelingsmonitor. als hier veranderingen in aangebracht moeten worden,
#zie etl/pipeline.R
df <- read_feather('data/dummies_20_2.feather')

df_pivot <- read_feather('data/dummies_pivot_20_2.feather')

#tabellen met aantallen ####
aantallen = df %>% aantallen_tabel()
for (i in 1:nrow(aantallen)){
  for (j in 2:ncol(aantallen)-1){
    if (aantallen[i,j] < 11){ #put categories with 10 or less observaties at NA for CBS privacy rules
      aantallen[i, j] <- NA
    }
  }
}
#writexl::write_xlsx(aantallen, "Export/Aantallen.xlsx")

#tabellen met % ####
perc = aantallen %>%
  mutate_at(vars(Bijstand:AKW),
            list(~ . / Totaal)) %>%
  select(-Totaal)

#writexl::write_xlsx(perc, "Export/Percentages.xlsx")

#type uitkeringen####
type = df %>% type_uitkeringen()

#writexl::write_xlsx(type, "Export/type_regelingen.xlsx")

#hoeveelheid regelingen per persoon ####
#verdeling van de hoeveelheid regelingen
hoeveelheid_regelingen = df %>%
  group_by(aantal = totaal) %>%
  summarise(n_totaal = n()) %>%
  mutate(n_totaal = ifelse(n_totaal <11, NA, n_totaal)) # Remove cells with 10 or less observations for CBS privacy

hoeveelheid_regelingen_excl_toeslagen = df %>%
  group_by(aantal = totaal_excl_toeslag) %>%
  summarise(n_excl_toeslag = n()) %>%
  mutate(n_excl_toeslag = ifelse(n_excl_toeslag <11, NA, n_excl_toeslag)) # Remove cells with 10 or less observations for CBS privacy

hoeveelheid_regelingen_excl_toeslagen_kind = df %>%
  group_by(aantal = totaal_excl_toeslag_kind) %>%
  summarise(n_excl_toeslag_kind = n()) %>%
  mutate(n_excl_toeslag_kind = ifelse(n_excl_toeslag_kind<11, NA, n_excl_toeslag_kind)) # Remove cells with 10 or less observations for CBS privacy

hoeveelheid <- hoeveelheid_regelingen_excl_toeslagen_kind %>%
  full_join(hoeveelheid_regelingen_excl_toeslagen) %>%
  full_join(hoeveelheid_regelingen)

#writexl::write_xlsx(hoeveelheid, "Export/Hoeveelheid_regelingen.xlsx")

#in hoeveel regelingen zitten mensen gemiddeld, als ze ook in regeling X zitten?####
gemiddeld_per_regeling <- df %>% stapeling_per_regeling()

#writexl::write_xlsx(gemiddeld_per_regeling, "Export/Per_regeling_hoeveelheid.xlsx")

#hoeveel stapelaars zijn er per regeling ####
stapelaars <- df_pivot %>%
  group_by(name) %>%
  summarise(n = sum(value == 1),
            stapelaars_alles = sum(value == 1 & totaal > 1),
            stapelaars_excl_ts = sum(value == 1 & totaal_excl_toeslag > 1),
            stapelaars_alleen_regelingen = sum(value == 1 & totaal_excl_toeslag_kind > 1)) %>%
  mutate(perc_alles = stapelaars_alles / n,
         perc_excl_ts = stapelaars_excl_ts / n,
         perc_alleen_regelingen = stapelaars_alleen_regelingen / n)

openxlsx::write.xlsx(stapelaars, file = "Export/Stapelaars.xlsx")

#spreiding binnen regelingen####
spreiding_stapeling_regelingen <- df_pivot %>% group_by(name, value, totaal_excl_toeslag_kind) %>%
  summarise(n = n()) %>%
  filter(value == 1) %>%
  mutate(n = ifelse(n < 11, NA, n))

spreiding_stapeling_incl_alle_toeslagen <- df_pivot %>% group_by(name, value, totaal) %>%
  summarise(n = n()) %>%
  filter(value == 1) %>%
  mutate(n = ifelse(n < 11, NA, n))

spreiding_stapeling_incl_toeslag_kind <- df_pivot %>% group_by(name, value, totaal_excl_toeslag) %>%
  summarise(n = n()) %>%
  filter(value == 1) %>%
  mutate(n = ifelse(n < 11, NA, n))

openxlsx::write.xlsx(list("Spreiding_regelingen" = spreiding_stapeling_regelingen,
                "Spreiding_alles" = spreiding_stapeling_incl_alle_toeslagen,
                "Spreiding_regelingen_en_kind" = spreiding_stapeling_incl_toeslag_kind),
                "Export/Spreiding_stapeling.xlsx")


#uitvoerders####
#hoeveel uitvoerders hebben mensen mee te maken
uitvoerders <- df %>% aantal_uitvoerders()
uitvoerders$Totaal_uitvoerders = rowSums(uitvoerders)
uitvoerders_sum = uitvoerders %>% group_by(Totaal_uitvoerders) %>% summarise(n=n())
#writexl::write_xlsx(uitvoerders_sum, "Export/Hoeveelheid_uitvoerders.xlsx")

#met welke uitvoerders hebben mensen het meest te maken
welke_uitvoerders <- uitvoerders %>%
  group_by(Belastingdienst, UWV, SVB, Gemeenten) %>%
  summarise(n = n()) %>% mutate(n = ifelse(n <11, NA, n)) # Remove cells with 10 or less observations for CBS privacy

#writexl::write_xlsx(welke_uitvoerders, "Export/Welke_uitvoerders.xlsx")

#meest voorkomende combinaties####
meest_voorkomend <- df %>%
  meest_voorkomende_combinaties('Bijstand', 'toeslag', 'Ja')

meest_voorkomend_excl_toeslagen <- df %>%
  meest_voorkomende_combinaties('Bijstand', 'ANW', 'Nee')

meest_voorkomend_alle <- df %>%
  meest_voorkomende_combinaties('Bijstand', "AKW", "Alle")

combinatie_toeslagen <- df %>% meest_voorkomende_combinaties('Bijstand', 'Zorgtoeslag', 'Alle')

write.xlsx(list("Toeslagen_samengenomen" = meest_voorkomend,
                "Zonder toeslagen" = meest_voorkomend_excl_toeslagen,
                "Alle toeslagen los" = meest_voorkomend_alle,
                "Zorg_huurtoeslag" = combinatie_toeslagen),
           "Export/meest_voorkomende_combinaties.xlsx")

#persoonskenmerken: data klaarmaken####
kenmerken = c("geslacht_2019",
              "hbopl_2019",
              "gewichtopl_2019",
              "generatie_2019",
              "leeftijd_2019",
              "stedgem_2019",
              "herkomst_2019")
df_pk <- df %>% koppel_persoonskenmerken(kenmerken)
df_pk <- df_pk %>% recode_pk_samen()

df_pk <- df_pk %>% select(-huishoudnr_2019)
df_pk = df_pk %>% filter(totaal > 0) %>% select(-starts_with('totaal'))
df_pk = df_pk %>%  pivot_longer(Bijstand:AKW)

#Persoonskenmerken: wel geen sociale zekerheid####
#ivm privacy regels worden soms specifieke categorieën eruit gefilterd; dit is omdat deze categorieën met alle uitsplitsingen nooit voldoende observaties opleveren.
pk_1a = df_pk %>% group_by(totaal_excl_toeslag, Geslacht) %>%
  summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal_excl_toeslag != 6)
pk_2a = df_pk  %>% group_by(totaal_excl_toeslag, Opleiding) %>%
  summarise(n = sum(gewichtopl_2019, na.rm = T)) %>%
  mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal_excl_toeslag != 6 &
         Opleiding != "Onbekend")
pk_3a = df_pk  %>% group_by(totaal_excl_toeslag, Generatie) %>%
  summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))
pk_4a = df_pk  %>% group_by(totaal_excl_toeslag, Herkomst) %>%
  summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal_excl_toeslag != 6)
pk_5a = df_pk   %>% group_by(totaal_excl_toeslag, Stedelijkheid) %>%
  summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))

pk_totaal_excl_ts <- list('Totaal_excl_ts_Geslacht' = pk_1a,
                    "Totaal_excl_ts_Opleiding" = pk_2a,
                    "Totaal_excl_ts_Generatie" = pk_3a,
                    "Totaal_excl_ts_Herkomst" = pk_4a,
                    "Totaal_excl_ts_Stedelijkheid" = pk_5a)

openxlsx::write.xlsx(pk_totaal_excl_ts, file = "Export/Persoonskenmerken_per_totaal_excl_ts.xlsx")


#persoonskenmerken per regeling###
pk_1 = df_pk %>% samenvatting_kenmerk("name","Geslacht")
pk_2 = df_pk %>% samenvatting_kenmerk("name","Opleiding")
pk_3 = df_pk %>% samenvatting_kenmerk("name","Generatie")
pk_4 = df_pk %>% samenvatting_kenmerk("name","Herkomst") %>% filter(name != "Bbz" & name != "WAO" & name != "IOAZ")
pk_5 = df_pk %>% samenvatting_kenmerk("name","Stedelijkheid")

pk_regeling <- list('Regeling_Geslacht' = pk_1,
                 "Regeling_Opleiding" = pk_2,
                 "Regeling_Generatie" = pk_3,
                 "Regeling_Herkomst" = pk_4,
                 "Regeling_Stedelijkheid" = pk_5)

openxlsx::write.xlsx(pk_regeling, file = "Export/Persoonskenmerken_per_regeling.xlsx")

#persoonskenmerken per stapeling hoeveelheid

pk_6 = df_pk %>% group_by(totaal, Geslacht) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal != 9)
pk_7 = df_pk %>% group_by(totaal, Opleiding) %>% summarise(n = sum(gewichtopl_2019, na.rm = T)) %>%
  mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal != 9 &
           Opleiding != "Onbekend")

pk_8 = df_pk %>% group_by(totaal, Generatie) %>% summarise(n = n()) %>%
  mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal != 9)
pk_9 = df_pk %>% group_by(totaal, Herkomst) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal != 9)
pk_10 =df_pk %>% group_by(totaal, Stedelijkheid) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal != 9)

pk_totaal <- list('Totaal_Geslacht' = pk_6,
                    "Totaal_Opleiding" = pk_7,
                    "Totaal_Generatie" = pk_8,
                    "Totaal_Herkomst" = pk_9,
                    "Totaal_Stedelijkheid" = pk_10)

openxlsx::write.xlsx(pk_totaal, file = "Export/Persoonskenmerken_totaal.xlsx")

#persoonskenmerken per stapeling excl toeslagen
pk_11 = pk %>% group_by(totaal_excl_toeslag, Geslacht) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))
pk_12 = pk %>% group_by(totaal_excl_toeslag, Opleiding) %>% summarise(n = sum(gewichtopl_2019, na.rm = T)) %>% mutate(n = ifelse(n < 11, NA, n))
pk_13 = pk %>% group_by(totaal_excl_toeslag, Generatie) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))
pk_14 = pk %>% group_by(totaal_excl_toeslag, Herkomst) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))
pk_15 = pk  %>% group_by(totaal_excl_toeslag, Stedelijkheid) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))

pk_totaal_excl_ts <- list('Totaal_ex_ts_Geslacht' = pk_11,
                    "Totaal_ex_ts_Opleiding" = pk_12,
                    "Totaal_ex_ts_Generatie" = pk_13,
                    "Totaal_ex_ts_Herkomst" = pk_14,
                    "Totaal_ex_ts_Stedelijkheid" = pk_15)

openxlsx::write.xlsx(pk_totaal_excl_ts, file = "Export/Persoonskenmerken_totaal_excl_ts.xlsx")

#Persoonskenmerken: alleen uitkeringen####
pk_16 = df_pk %>% group_by(totaal_excl_toeslag_kind, Geslacht) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))
pk_17 = df_pk %>% group_by(totaal_excl_toeslag_kind, Opleiding) %>% summarise(n = sum(gewichtopl_2019, na.rm = T)) %>% mutate(n = ifelse(n < 11, NA, n))
pk_18 = df_pk %>% group_by(totaal_excl_toeslag_kind, Generatie) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))
pk_19 = df_pk %>% group_by(totaal_excl_toeslag_kind, Herkomst) %>%
  summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n)) %>%
  filter(totaal_excl_toeslag_kind != 4)
pk_20 = df_pk  %>% group_by(totaal_excl_toeslag_kind, Stedelijkheid) %>% summarise(n = n()) %>% mutate(n = ifelse(n < 11, NA, n))

pk_totaal_excl_ts_kind <- list('Totaal_ex_ts_kind_Geslacht' = pk_16,
                          "Totaal_ex_ts_kind_Opleiding" = pk_17,
                          "Totaal_ex_ts_kind_Generatie" = pk_18,
                          "Totaal_ex_ts_kind_Herkomst" = pk_19,
                          "Totaal_ex_ts_kind_Stedelijkheid" = pk_20)

openxlsx::write.xlsx(pk_totaal_excl_ts_kind, file = "Export/Persoonskenmerken_totaal_excl_ts_kind.xlsx")

#sociaal domein####
df_sd <- koppel_sociaal_domein()
df_sd <- df_sd %>% sociaal_domein_recode()
df_sd$totaal_incl_sd = rowSums(df_sd[,c(23,26:29)])
df_sd_pivot <- df_sd %>% filter(totaal > 0) %>%  select(-starts_with('totaal'), -huishoudnr_2019)
df_sd_pivot = df_sd_pivot %>% pivot_longer(Bijstand:AKW)

sd_totaal = df_sd %>% sociaal_domein_samenvatting_totaal() %>% filter(totaal != 8 & totaal != 9)
sd_totaal_excl_ts = df_sd %>% sociaal_domein_samenvatting_totaal_excl_toeslag()
sd_totaal_excl_ts_kind = df_sd %>% sociaal_domein_samenvatting_totaal_excl_toeslag_kind()
sd_regeling = df_sd_pivot %>% sociaal_domein_regeling()

sociaal_domein <- list('SD_totaal_regelingen' = sd_totaal,
                          "SD_totaal_excl_ts" = sd_totaal_excl_ts,
                          "SD_totaal_excl_ts_kind" = sd_totaal_excl_ts_kind,
                          "SD_per_regeling" = sd_regeling)

openxlsx::write.xlsx(sociaal_domein, file = "Export/Sociaal_domein.xlsx")



#complexiteit maal gebruik
p_maal_q <- function(.data, welke_score, end_break){
  if (welke_score == "ervaren") {
    df_p_q = df %>%
      select(Bijstand, IOAW, IOAZ, Bbz,
             WW, ZW, IVA, WGA, Wajong, WAO, AOW, ANW,
             Huurtoeslag, Zorgtoeslag, Kinderopvangtoeslag, AKW)
    anw_score = 3.69
    aow_score = 2.19
    bijstand_score = 4.28
    bbz_score = 3.97
    ht_score = 3.44
    ioaw_score = 3.91
    ioaz_score = 3.91
    iva_score = 4.78
    akw_score = 2.61
    kot_score = 4.15
    wajong_score = 4.67
    wao_score = 4.28
    wga_score = 5.11
    ww_score = 3.44
    zw_score = 3.65
    zt_score = 3.75
    df_p_q = df_p_q %>%
      mutate(
        Bijstand = ifelse(Bijstand == 1, bijstand_score, Bijstand),
        IOAW = ifelse(IOAW == 1, ioaw_score, IOAW),
        IOAZ = ifelse(IOAZ == 1, ioaz_score, IOAZ),
        Bbz = ifelse(Bbz == 1, bbz_score, Bbz),
        WW = ifelse(WW == 1, ww_score, WW),
        ZW = ifelse(ZW == 1, zw_score, ZW),
        IVA = ifelse(IVA == 1, iva_score, IVA),
        WGA = ifelse(WGA == 1, wga_score, WGA),
        Wajong = ifelse(Wajong == 1, wajong_score, Wajong),
        WAO = ifelse(WAO == 1, wao_score, WAO),
        AOW = ifelse(AOW == 1, aow_score, AOW),
        ANW = ifelse(ANW == 1, anw_score, ANW),
        Huurtoeslag = ifelse(Huurtoeslag == 1, ht_score, Huurtoeslag),
        Zorgtoeslag = ifelse(Zorgtoeslag == 1, zt_score, Zorgtoeslag),
        Kinderopvangtoeslag = ifelse(Kinderopvangtoeslag == 1, kot_score, Kinderopvangtoeslag),
        AKW = ifelse(AKW == 1, akw_score, AKW))
  }
  if (welke_score == "aantal_voorwaarden") {
    df_p_q = df_p_q = df %>%
      select(Bijstand, Bbz, TW, IOAZ, IOAW, AIO, Wajong,
             WGA, WW, IVA, ZW, WAO,
             ANW, AOW, AKW,
             Huurtoeslag, Zorgtoeslag, Kinderopvangtoeslag, Kindgebondenbudget)
      bijstand_score = 8
      bbz_score = 7
      tw_score = 7
      ioaz_score = 7
      ioaw_score = 6
      aio_score = 6
      wajong_score = 4

      wga_score = 7
      ww_score = 5
      iva_score = 4
      zw_score = 4
      wao_score = 5

      anw_score = 5
      aow_score = 2
      akw_score = 2

      ht_score = 6
      zt_score = 5
      kgb_score = 5
      kot_score = 4

      df_p_q = df_p_q %>%
        mutate(
          Bijstand = ifelse(Bijstand == 1, bijstand_score, Bijstand),
          Bbz = ifelse(Bbz == 1, bbz_score, Bbz),
          TW = ifelse(TW == 1, tw_score, TW),
          IOAW = ifelse(IOAW == 1, ioaw_score, IOAW),
          IOAZ = ifelse(IOAZ == 1, ioaz_score, IOAZ),
          AIO = ifelse(AIO == 1, aio_score, AIO),
          Wajong = ifelse(Wajong == 1, wajong_score, Wajong),


          WW = ifelse(WW == 1, ww_score, WW),
          ZW = ifelse(ZW == 1, zw_score, ZW),
          IVA = ifelse(IVA == 1, iva_score, IVA),
          WGA = ifelse(WGA == 1, wga_score, WGA),
          WAO = ifelse(WAO == 1, wao_score, WAO),

          AOW = ifelse(AOW == 1, aow_score, AOW),
          ANW = ifelse(ANW == 1, anw_score, ANW),
          AKW = ifelse(AKW == 1, akw_score, AKW),

          Huurtoeslag = ifelse(Huurtoeslag == 1, ht_score, Huurtoeslag),
          Zorgtoeslag = ifelse(Zorgtoeslag == 1, zt_score, Zorgtoeslag),
          Kinderopvangtoeslag = ifelse(Kinderopvangtoeslag == 1, kot_score, Kinderopvangtoeslag),
          Kindgebondenbudget = ifelse(Kindgebondenbudget == 1, kgb_score, Kindgebondenbudget))
  }
  df_p_q$totaal_complex = rowSums(df_p_q[,c(1:ncol(df_p_q))])
  p_maal_q = df_p_q %>% group_by(complex_cat = cut(totaal_complex, breaks = c(-Inf, 1:end_break, Inf))) %>% summarise(n = n())
}

p_maal_q_vw = df %>%  p_maal_q("aantal_voorwaarden", 24)
p_maal_q_erv = df %>%  p_maal_q("ervaren", 24)
p_maal_q_alles = full_join(p_maal_q_erv, p_maal_q_vw, suffix = c('_ervaren_complexiteit', '_aantal_voorwaarden'), by = "complex_cat")

#openxlsx::write.xlsx(p_maal_q_alles, file = 'Export/gebruik_keer_complexiteit.xlsx')

risicos = df %>%
  summarise(
    ziekte_ao = ifelse(WGA == 1 | IVA == 1 | WAO == 1 | Wajong == 1 | WAZ == 1, 1, 0),
    ouderdom = ifelse(AOW == 1 | AIO == 1, 1, 0),
    overlijden = ifelse(ANW == 1, 1, 0),
    vangnet_ww = ifelse(Bijstand == 1 | Bbz == 1 | IOAZ == 1 | IOAW == 1 | WW == 1 | TW == 1, 1, 0),
    kinderen = ifelse(AKW == 1 | Kindgebondenbudget == 1, 1, 0)
  )

risicos_sum = data.frame(tibble(
  category = c('ziekte_ao', 'ouderdom', 'overlijden','vangnet_ww', 'kinderen'),
  telling = c(sum(risicos$ziekte_ao), sum(risicos$ouderdom), sum(risicos$overlijden), sum(risicos$vangnet_ww), sum(risicos$kinderen))
))

#writexl::write_xlsx(risicos_sum, 'Export/doelgroepen_summary.xlsx')
