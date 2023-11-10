#' bestanden_koppelen
#'
#'Deze functie laadt de verschillende sociale zekerheidsbestanden in,
#'koppelt deze aan elkaar en maakt het integrale bestand schoon.
#'
#'@import haven tidyverse fastDummies
#'@export

bestanden_koppelen <- function(){
  #eerst worden de functies in maandelijkse data inladen gebruikt om de bestanden in te laden & op te schonen
  sm <- sm_inladen()
  inpatab <- inpatab_inladen()
  anw <- anw_inladen()
  ao <- ao_inladen()
  aow <- aow_inladen()
  bijstand <- bijstand_inladen()
  ww <- ww_inladen()
  zw <- zw_inladen()
  akw <- akw_inladen()

  #stapelingdocument maken
  df <- inpatab %>%
    left_join(sm) %>%
    left_join(aow) %>%
    left_join(anw, by = c('RINPERSOONS' = 'RINpersoonS', 'RINPERSOON' = 'RINpersoon')) %>%
    left_join(bijstand) %>%
    left_join(ao, by = c('RINPERSOONS' = 'RINPersoonS', 'RINPERSOON' = 'RINPersoon')) %>%
    left_join(ww, by = c('RINPERSOONS' = 'RINPersoonS', 'RINPERSOON' = 'RINPersoon')) %>%
    left_join(zw) %>%
    left_join(akw) %>%
    clean_df() %>% filter(leeftijd_2019 > 17) %>%
    select(starts_with('RIN'), huishoudnr_2019,
           Bijstand, IOAW, IOAZ, Bbz,
           WW, ZW, IVA, WGA, Wajong, WAO, WAZ, TW,
           AOW, AIO, ANW,
           Huurtoeslag, Zorgtoeslag, Kindgebondenbudget, Kinderopvangtoeslag, AKW
    )
}



