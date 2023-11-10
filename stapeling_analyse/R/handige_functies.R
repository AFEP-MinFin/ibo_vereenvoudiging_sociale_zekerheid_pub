filter_year_to_month_data <- function(.data, eind_datum_var){
  .data %>%
    mutate(
      year_month =  substr(as.numeric(as.character(.data[[eind_datum_var]])), 0, 6)
    ) %>%
    filter(year_month > 201911) #categorie 88888888 wordt nu ook behouden
}

clean_df <- function(.data){
  .data %>%
    mutate(
      TW = ifelse(TW_dummy.x == 1 | TW_dummy.y == 1, 1, 0),
      AIO = ifelse(AOW == 1 & Bijstand == 1, 1, 0)) %>%
    mutate_at(vars(Huurtoeslag:AIO),
              list(~ ifelse(is.na(.), 0, .))) %>%
    mutate(Bijstand = ifelse(AIO == 1, 0, Bijstand)) %>%
    select(-TW_dummy.x, - TW_dummy.y, - contains('code'), - starts_with('INP'), -ends_with('HKW'))
}

