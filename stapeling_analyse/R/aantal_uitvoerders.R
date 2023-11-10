aantal_uitvoerders <- function(.data) {
  regeling_uitvoerders <- data.frame(
    Regeling = c('Zorgtoeslag', 'Huurtoeslag', 'Kinderopvangtoeslag', 'Kindgebondenbudget',
                 'WW', 'TW', 'Wajong', 'WGA', 'IVA', 'ZW', 'WAO', 'WAZ',
                 'ANW', 'AOW', 'AIO', 'AKW',
                 'Bijstand', 'IOAW', 'IOAZ', 'Bbz'),
    uitvoerder = c('Belastingdienst','Belastingdienst', 'Belastingdienst', 'Belastingdienst',
                   'UWV', 'UWV', 'UWV', 'UWV', 'UWV', 'UWV', 'UWV', 'UWV',
                   'SVB', 'SVB', 'SVB','SVB',
                   'Gemeenten', 'Gemeenten', 'Gemeenten', 'Gemeenten')
  )

  x <- .data %>%
    filter(totaal > 0) %>%
    select(-starts_with('totaal')) %>%
    pivot_longer(
      cols = c(-starts_with('RIN')),
      names_to = "Regeling",
      values_to = "Dummy"
    ) %>%
    left_join(regeling_uitvoerders) %>%
    filter(Dummy == 1) %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    summarise(hoeveelheid_uitvoerders = n_distinct(uitvoerder)) %>%
    group_by(hoeveelheid_uitvoerders) %>%
    summarise(n = n())

}

