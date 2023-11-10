koppel_sociaal_domein <- function(){
  persoonskenmerken <- read_sav('L:/211130 Stapelingsmonitor SZW 2019 V3_selectie9198.sav',
                                col_select = c(RINPERSOONS, RINPERSOON,
                                               starts_with('wmo'),
                                               wsnp_2019,
                                               wanbetaler_zorgverzekering_2019,
                                               leeftijd_2019,
                                               basis_ggz_2019,
                                               special_ggz_2019,
                                               lvb_2019))
  df_pk <- df %>% left_join(persoonskenmerken) %>%
    filter(leeftijd_2019 > 17) %>% select(-leeftijd_2019)
}

sociaal_domein_recode <- function(.data){
  .data %>%
    mutate(
      wmo = ifelse(
        wmo_gem_huishoudelijke_hulp_2019 == 1 |
        wmo_cak_ondersteuning_thuis_2019 == 1 |
        wmo_cak_hulpmiddelen_diensten_2019 == 1 |
        wmo_cak_verblijf_opvang_2019 == 1 |
        wmo_gem_huishoudelijke_hulp_2019 == 1 |
        wmo_gem_ondersteuning_thuis_2019 == 1 |
        wmo_gem_hulpmiddelen_diensten_2019 == 1 |
        wmo_gem_verblijf_opvang_2019 == 1,
        1, 0),
      schulden = ifelse(
        wsnp_2019 == 1 | wanbetaler_zorgverzekering_2019 == 1, 1, 0),
      ggz = ifelse(
        basis_ggz_2019 == 1 | special_ggz_2019 == 1, 1, 0
      )
      ) %>%
    select(-starts_with("wmo_"),
           -wsnp_2019, -wanbetaler_zorgverzekering_2019,
           -contains('_ggz')) %>%
    select(everything(), lvb = lvb_2019)
}

sociaal_domein_samenvatting_totaal <- function(.data){
  sd_1 <- df_sd %>% group_by(totaal, wmo) %>% summarise(n = n()) %>% ungroup() %>%
    select(totaal, sociaal_domein = wmo, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_2 <- df_sd %>% group_by(totaal, ggz) %>% summarise(n = n()) %>% ungroup() %>%
    select(totaal, sociaal_domein = ggz, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_3 <- df_sd %>% group_by(totaal, schulden) %>% summarise(n = n()) %>% ungroup() %>%
    select(totaal, sociaal_domein = schulden, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_4 <- df_sd %>% group_by(totaal, lvb) %>% summarise(n = n()) %>% ungroup() %>%
    select(totaal, sociaal_domein = lvb, n) %>% mutate(n = ifelse(n < 11, NA, n))

  sd = sd_1 %>% full_join(sd_2, by = c('totaal', 'sociaal_domein'), suffix = c('_wmo',"_ggz")) %>%
    full_join(sd_3, by = c('totaal', 'sociaal_domein')) %>%
    full_join(sd_4, by = c('totaal', 'sociaal_domein'), suffix = c('_schulden',"_lvb"))
}

sociaal_domein_samenvatting_totaal_excl_toeslag <- function(.data){
  sd_1 <- df_sd %>% group_by(totaal_excl_toeslag, wmo) %>% summarise(n = n()) %>% ungroup() %>% select(totaal_excl_toeslag, sociaal_domein = wmo, n)  %>% mutate(n = ifelse(n < 11, NA, n))
  sd_2 <- df_sd %>% group_by(totaal_excl_toeslag, ggz) %>% summarise(n = n()) %>% ungroup() %>% select(totaal_excl_toeslag, sociaal_domein = ggz, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_3 <- df_sd %>% group_by(totaal_excl_toeslag, schulden) %>% summarise(n = n()) %>% ungroup() %>% select(totaal_excl_toeslag, sociaal_domein = schulden, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_4 <- df_sd %>% group_by(totaal_excl_toeslag, lvb) %>% summarise(n = n()) %>% ungroup() %>% select(totaal_excl_toeslag, sociaal_domein = lvb, n) %>% mutate(n = ifelse(n < 11, NA, n))

  sd = sd_1 %>% full_join(sd_2, by = c('totaal_excl_toeslag', 'sociaal_domein'), suffix = c('_wmo',"_ggz")) %>%
    full_join(sd_3, by = c('totaal_excl_toeslag', 'sociaal_domein')) %>%
    full_join(sd_4, by = c('totaal_excl_toeslag', 'sociaal_domein'), suffix = c('_schulden',"_lvb"))
}

sociaal_domein_samenvatting_totaal_excl_toeslag_kind <- function(.data){
  sd_1 <- df_sd %>% group_by(totaal_excl_toeslag_kind, wmo) %>% summarise(n = n()) %>% ungroup() %>%
    select(totaal_excl_toeslag_kind, sociaal_domein = wmo, n)  %>% mutate(n = ifelse(n < 11, NA, n))
  sd_2 <- df_sd %>% group_by(totaal_excl_toeslag_kind, ggz) %>% summarise(n = n()) %>% ungroup() %>%
    select(totaal_excl_toeslag_kind, sociaal_domein = ggz, n)  %>% mutate(n = ifelse(n < 11, NA, n))
  sd_3 <- df_sd %>% group_by(totaal_excl_toeslag_kind, schulden) %>% summarise(n = n()) %>% ungroup() %>% select(totaal_excl_toeslag_kind, sociaal_domein = schulden, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_4 <- df_sd %>% group_by(totaal_excl_toeslag_kind, lvb) %>% summarise(n = n()) %>% ungroup() %>%
    select(totaal_excl_toeslag_kind, sociaal_domein = lvb, n) %>% mutate(n = ifelse(n < 11, NA, n))

  sd = sd_1 %>% full_join(sd_2, by = c('totaal_excl_toeslag_kind', 'sociaal_domein'), suffix = c('_wmo',"_ggz")) %>%
    full_join(sd_3, by = c('totaal_excl_toeslag_kind', 'sociaal_domein')) %>%
    full_join(sd_4, by = c('totaal_excl_toeslag_kind', 'sociaal_domein'), suffix = c('_schulden',"_lvb"))
}

sociaal_domein_regeling <- function(.data){
  sd_1 = .data %>% samenvatting_kenmerk("name", "wmo") %>% select(name, sociaal_domein = wmo, n)  %>% mutate(n = ifelse(n < 11, NA, n))
  sd_2 = .data %>% samenvatting_kenmerk("name", "ggz") %>% select(name, sociaal_domein = ggz, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_3 = .data %>% samenvatting_kenmerk("name", "schulden") %>% select(name, sociaal_domein = schulden, n) %>% mutate(n = ifelse(n < 11, NA, n))
  sd_4 = .data %>% samenvatting_kenmerk("name", "lvb") %>% select(name, sociaal_domein = lvb, n) %>% mutate(n = ifelse(n < 11, NA, n))

  sd = sd_1 %>% full_join(sd_2, by = c('name', 'sociaal_domein'), suffix = c('_wmo', '_ggz')) %>%
    full_join(sd_3, by = c('name', 'sociaal_domein')) %>%
    full_join(sd_4, by = c('name', 'sociaal_domein'), suffix = c('_schulden', '_lvb'))
}

