recode_regression_tables_beleid <- function(.data){
  recode_key = tidy(model_naive)[,1]
  recode_key$Voorwaarde = c('Constante',
                            'Inkomenstoets',
                            'Gebaseerd op dagloon',
                            'Vermogenstoets',
                            'Verrekening van het gebruik van restverdiencapaciteit',
                            'Arbeidsverplichtingen',
                            'Voorschottensystematiek',
                            'Verrekenen van bijverdiensten',
                            'Verrekenen van bijverdiensten',
                            'Veranderende hoogte van de uitkering over tijd',
                            'Herlevingsrecht na uitstroom',
                            'Moeilijke omstandigheden in het leven')
  recode_key$Categorie = c('n.v.t', 
                             'Ja', 
                             'Ja', 
                             'Ja', 
                             'Ja', 
                             'Ja',
                             'Ja',
                             'Deels / met uitzonderingen verrekend',
                             'Volledig verrekend',
                             'Ja',
                             "Geen",
                             'n.v.t.') 
  
  .data %>% full_join(recode_key) %>% select(-term) %>% select(Voorwaarde, Categorie, everything())
}

recode_regression_tables_pm <- function(.data){
  recode_key = tidy(model_pm)[,1]
  recode_key$Persoonskenmerk = c('Constante',
                            'Moeilijke omstandigheden in het leven',
                            'Aantal inkomenstenbronnen',
                            'Aantal inkomenstenbronnen', 
                            'Samenstelling huishouden', 
                            'Samenstelling huishouden', 
                            'Samenstelling huishouden', 
                            'Samenstelling huishouden', 
                            'Woning', 
                            'Woning', 
                            'Opleidingsniveau',
                            'Opleidingsniveau',
                            'Opleidingsniveau',
                            'Opleidingsniveau',
                            'Opleidingsniveau',
                            'Opleidingsniveau',
                            'Opleidingsniveau',
                            'Netto-inkomen',
                            'Vriendelijkheid',
                            'Ordelijkheid',
                            'Extraversie',
                            'Neuroticisme',
                            'Openheid voor ervaringen',
                            'Sprake van stapeling')
  recode_key$Categorie = c('n.v.t', 
                           'Vijf-puntsschaal',
                           'Inkomen van twee personen (uit werk of uitkering)',
                           'Inkomen van drie of meer personen (uit werk of uitkering)',
                           '(On)gehuwd samenwonend, zonder kind(eren)',
                           '(On)gehuwd samenwonend, met kind(eren)',
                           'Alleenstaande, met kind(eren)',
                           'Anders',
                           'Huurwoning', 
                           'Gratis woning',
                           'Vmbo', 
                           'Havo/vwo',
                           'Mbo', 
                           'Hbo',
                           'Wo',
                           'Anders',
                           '(Nog) geen onderwijs afgerond',
                           'In duizend euros',
                           'Schaal van 1 - 5',
                           'Schaal van 1 - 5',
                           'Schaal van 1 - 5',
                           'Schaal van 1 - 5',
                           'Schaal van 1 - 5',
                           'Binair')
  
  .data %>% full_join(recode_key) %>% select(-term) %>% select(Persoonskenmerk, Categorie, everything())
}

recode_voorwaarden <- function(.data, dataframe){
  recode_key = .data %>% select(voorwaarde) %>% distinct()
  recode_key$Voorwaarde = c('Inkomenstoets',
                            'Gebaseerd op dagloon',
                            'Vermogenstoets',
                            'Gebruik van begrip restverdiencapaciteit',
                            'Arbeidsverplichtingen',
                            'Voorschottensystematiek',
                            'Verrekenen van bijverdiensten',
                            'Veranderende hoogte van de uitkering over tijd',
                            'Herlevingsrecht na uitstroom')
  
  recoded = .data %>% full_join(recode_key) %>% select(-voorwaarde) %>% 
    select(Voorwaarde, Categorie, `Aantal observaties`)
}
