#aantal uitvoerders maken####
aantal_uitvoerders <- function(.data){
  .data %>%
    mutate(
      Belastingdienst = ifelse(
        Zorgtoeslag == 1 |
        Huurtoeslag == 1 |
        Kinderopvangtoeslag == 1 |
        Kindgebondenbudget == 1,
        1, 0),
      UWV = ifelse(WW == 1 | TW == 1 | Wajong == 1 | WGA == 1 | IVA == 1 | ZW == 1 | WAO == 1 | WAZ == 1, 1, 0),
      SVB = ifelse(ANW == 1 | AOW == 1 | AIO == 1 | AKW == 1, 1, 0),
      Gemeenten = ifelse(Bijstand == 1 | IOAW == 1 | Bbz == 1 | IOAZ == 1, 1, 0)
    ) %>%
    select(Belastingdienst, UWV, SVB, Gemeenten)
}
