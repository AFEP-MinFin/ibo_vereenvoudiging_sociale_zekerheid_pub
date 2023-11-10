type_uitkeringen <- function(.data){
  .data %>% mutate(
    Volksverzekeringen = ifelse(AOW == 1 | ANW ==  1 | AKW == 1, 1, 0),
    Werknemersverzekeringen = ifelse(WW == 1 | ZW == 1 | IVA == 1 | WGA == 1 | WAO == 1, 1, 0),
    Voorzieningen = ifelse(IOAW == 1 | IOAZ == 1 | Bbz == 1 | Bijstand == 1 | TW == 1 | AIO == 1 | Wajong == 1, 1, 0),
    Toeslagen = ifelse(Huurtoeslag == 1 | Zorgtoeslag == 1 | Kindgebondenbudget == 1 | Kinderopvangtoeslag == 1, 1, 0)
  ) %>% select(Volksverzekeringen, Werknemersverzekeringen, Voorzieningen, Toeslagen) %>%
    group_by(Volksverzekeringen, Werknemersverzekeringen, Voorzieningen, Toeslagen) %>%
    summarise(n = n())
}

