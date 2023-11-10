library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(VennDiagram)
setwd("O:/AFEP/Onderhanden werk/Romée Lind/Sociale Zekerheid/Èxport 3")

#grafiek 2: hoeveel stapeling
hoeveelheid_regelingen <- readxl::read_xlsx('Hoeveelheid_regelingen.xlsx')

hoev_stapeling_graph <- hoeveelheid_regelingen %>% pivot_longer(cols = starts_with('n')) %>% 
  mutate(name = factor(name, levels = c('n_totaal', 'n_excl_toeslag', 'n_excl_toeslag_kind'))) %>% 
  mutate(name = recode(name,
                       "n_totaal" = "Alle regelingen, toeslagen, en kindgerelateerde regelingen",
                       "n_excl_toeslag" = "Alle regelingen en toeslagen, exclusief kindgerelateerde regelingen",
                       "n_excl_toeslag_kind" = "Alle regelingen, exclusief toeslagen en kindgerelateerde regelingen")) %>%
  ggplot(aes(x = aantal, y = value)) + geom_bar(stat = "identity", fill = "#42145F") + facet_wrap( ~ name) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlab("Aantal regelingen") +
  ylab("Aantal gebruikers") +
  ggtitle("Stapeling in de sociale zekerheid")

ggsave(filename = "Hoeveelheid_stapeling_graph.png",
       device = "png",
       plot = hoev_stapeling_graph, 
       width = 12,
       path = "O:/AFEP/Onderhanden werk/Romée Lind/Sociale Zekerheid/Enquête/output")

#meeste voorkomende combinaties
combinaties <- readxl::read_xlsx('meest_voorkomende_combinaties.xlsx')

combi_aow = combinaties %>% filter(grepl('AOW', combinatie) & grepl('AIO', combinatie))
sum(combi_aow$n)

combi_wgv_tw = combinaties %>% filter(grepl('WW', combinatie) | 
                                        grepl('Wajong', combinatie) | 
                                        grepl('WAO', combinatie) |
                                        grepl('WGA', combinatie) |
                                        grepl('IVA', combinatie) |
                                        grepl('ZW', combinatie) |
                                        grepl('WAZ', combinatie) |
                                        grepl('IOW', combinatie)) %>% 
  filter(grepl('TW', combinatie)) %>% filter(grepl('Bijstand', combinatie))
sum(combi_wgv_tw$n)

combi_wajong_ao = combinaties %>% filter(grepl('Wajong', combinatie)) %>% 
  filter(grepl('WAZ', combinatie) |
           grepl('IVA', combinatie)|
           grepl('WGA', combinatie)|
           grepl('WAO', combinatie))
sum(combi_wajong_ao$n)

combi_ww_wgv = combinaties %>% filter(grepl('WW', combinatie)) %>% 
  filter(grepl('Wajong', combinatie) |
           grepl('IVA', combinatie)|
           grepl('WGA', combinatie)|
           grepl('WAO', combinatie) |
           grepl('WAZ', combinatie))

sum(combi_ww_wgv$n)

writexl::write_xlsx(combi_ww_wgv, 'WW_ao.xlsx')

combi_bijstand = combinaties %>% filter(grepl('Bijstand', combinatie)) %>% 
  mutate(perc_totaal = round(n / 425340, 4), 
         perc_stapelaars = round(n / sum(n), 4))

combi_bijstand_clean = combi_bijstand %>% mutate(boven_1 = ifelse(perc_stapelaars < 0.01, "Overig", combinatie)) %>% 
  group_by(boven_1) %>% 
  summarise_if(is.numeric, 
               sum) %>% 
  select(combinatie = boven_1, everything()) %>% 
  mutate(regeling = "Bijstand")

combi_bijstand_clean$combinatie = gsub("_", " & ",combi_bijstand_clean$combinatie)
combi_bijstand_clean$combinatie = gsub("Bijstand & ", "",combi_bijstand_clean$combinatie)


combi_bijstand_clean %>% ggplot(aes(y = perc_stapelaars, x = regeling, fill = combinatie))+ coord_flip() + geom_col()
combi_bijstand %>% ggplot(aes(x = reorder(combinatie, n, sum), y = perc_stapelaars)) + coord_flip() + geom_col()

combi_IVA = combinaties %>% filter(grepl('IVA', combinatie)) %>% 
  mutate(perc_totaal = round(n / 121094, 4), 
         perc_stapelaars = round(n / sum(n), 4))

combi_IVA_clean = combi_IVA %>% mutate(boven_1 = ifelse(perc_stapelaars < 0.01, "Overig", combinatie)) %>% 
  group_by(boven_1) %>% 
  summarise_if(is.numeric, 
               sum) %>% 
  select(combinatie = boven_1, everything()) %>% 
  mutate(regeling = "IVA")

combi_IVA_clean$combinatie = gsub("_", " & ",combi_IVA_clean$combinatie)
combi_IVA_clean$combinatie = gsub("IVA & ", "",combi_IVA_clean$combinatie)

combi_IVA_clean %>% ggplot(aes(y = perc_stapelaars, x = regeling, fill = combinatie))+ coord_flip() + geom_col()


combi_WGA = combinaties %>% filter(grepl('WGA', combinatie)) %>% 
  mutate(perc_totaal = round(n / 192666, 4), 
         perc_stapelaars = round(n / sum(n), 4))

combi_Wajong = combinaties %>% filter(grepl('Wajong', combinatie)) %>% 
  mutate(perc_totaal = n / 242426, 
         perc_stapelaars = n / sum(n))



welke_uitvoerders = readxl::read_xlsx('Welke_uitvoerders.xlsx')

grid.newpage()
draw.quad.venn(area1 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1]),
               area2 = sum(welke_uitvoerders$n[welke_uitvoerders$UWV == 1]),
               area3 = sum(welke_uitvoerders$n[welke_uitvoerders$SVB == 1]),
               area4 = sum(welke_uitvoerders$n[welke_uitvoerders$Gemeenten == 1]),
               n12 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1]),
               n13 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$SVB == 1]),
               n14 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$Gemeenten == 1]),
               n23 = sum(welke_uitvoerders$n[welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1]), 
               n24 = sum(welke_uitvoerders$n[welke_uitvoerders$UWV == 1 & welke_uitvoerders$Gemeenten == 1]), 
               n34 = sum(welke_uitvoerders$n[welke_uitvoerders$SVB == 1 & welke_uitvoerders$Gemeenten == 1]), 
               n123 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1]),
               n124 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$Gemeenten == 1]),
               n234 = sum(welke_uitvoerders$n[welke_uitvoerders$Gemeenten == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1]),
               n134 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$SVB == 1 & welke_uitvoerders$Gemeenten == 1]),
               n1234 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1 & welke_uitvoerders$Gemeenten == 1]),
               category = c('Belastingdienst', 'UWV', 'SVB', 'Gemeenten'))


#terug naar dummy tabel
regelingen <- c("AIO", "ANW",'AOW', "Bbz", "Bijstand", "IOAW", "IOAZ", "IVA", "Wajong", "WAO", "WAZ", "WGA", "WW", "ZW", "TW","toeslag")

combi_dummy <- combinaties %>% 
  mutate(
    AIO = ifelse(grepl('AIO', combinaties$combinatie), 1, 0),
    ANW = ifelse(grepl('ANW', combinaties$combinatie), 1, 0),
    AOW = ifelse(grepl('AOW', combinaties$combinatie), 1, 0),
    Bbz = ifelse(grepl('Bbz', combinaties$combinatie), 1, 0),
    Bijstand = ifelse(grepl('Bijstand', combinaties$combinatie), 1, 0),
    IOAW = ifelse(grepl('IOAW', combinaties$combinatie), 1, 0),
    IOAZ = ifelse(grepl('IOAZ', combinaties$combinatie), 1, 0),
    IVA = ifelse(grepl('IVA', combinaties$combinatie), 1, 0),
    Wajong = ifelse(grepl('Wajong', combinaties$combinatie), 1, 0),
    WAO = ifelse(grepl('WAO', combinaties$combinatie), 1, 0),
    WAZ = ifelse(grepl('WAZ', combinaties$combinatie), 1, 0),
    WGA = ifelse(grepl('WGA', combinaties$combinatie), 1, 0),
    WW = ifelse(grepl('WW', combinaties$combinatie), 1, 0),
    ZW = ifelse(grepl('ZW', combinaties$combinatie), 1, 0),
    TW = ifelse(grepl('TW', combinaties$combinatie), 1, 0),
    toeslag = ifelse(grepl('toeslag', combinaties$combinatie), 1, 0),
  )
write.csv(combi_dummy, "O:/AFEP/Onderhanden werk/Romée Lind/Sociale Zekerheid/Meeste_combinaties_tabel.csv")

#Hoeveel mensen ondersteund in de SoZa
type_regelingen = read_xlsx('type_regelingen.xlsx')

grid.newpage()
draw.quad.venn(area1 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1]),
               area2 = sum(type_regelingen$n[type_regelingen$Werknemersverzekeringen == 1]),
               area3 = sum(type_regelingen$n[type_regelingen$Voorzieningen == 1]),
               area4 = sum(type_regelingen$n[type_regelingen$Toeslagen == 1]),
               n12 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1 & type_regelingen$Werknemersverzekeringen == 1]),
               n13 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1 & type_regelingen$Voorzieningen == 1]),
               n14 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1 & type_regelingen$Toeslagen == 1]),
               n23 = sum(type_regelingen$n[type_regelingen$Werknemersverzekeringen == 1 & type_regelingen$Voorzieningen == 1]), 
               n24 = sum(type_regelingen$n[type_regelingen$Werknemersverzekeringen == 1 & type_regelingen$Toeslagen == 1]), 
               n34 = sum(type_regelingen$n[type_regelingen$Voorzieningen == 1 & type_regelingen$Toeslagen == 1]), 
               n123 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1 & type_regelingen$Werknemersverzekeringen == 1 & type_regelingen$Voorzieningen == 1]),
               n124 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1 & type_regelingen$Werknemersverzekeringen == 1 & type_regelingen$Toeslagen == 1]),
               n234 = sum(type_regelingen$n[type_regelingen$Toeslagen == 1 & type_regelingen$Werknemersverzekeringen == 1 & type_regelingen$Voorzieningen == 1]),
               n134 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1 & type_regelingen$Voorzieningen == 1 & type_regelingen$Toeslagen == 1]),
               n1234 = sum(type_regelingen$n[type_regelingen$Volksverzekeringen == 1 & type_regelingen$Werknemersverzekeringen == 1 & type_regelingen$Voorzieningen == 1 & type_regelingen$Toeslagen == 1]),
               category = c('Volksverzekeringen', 'Werknemersverzekeringen', 'Voorzieningen', 'Toeslagen'))

#Er is verschil in hoeveel er wordt gestapeld in regelingen


x = readxl::read_xlsx('Spreiding_stapeling.xlsx', sheet = "Spreiding_regelingen") %>% filter(name != "AOW") %>% filter(name != "AKW") %>% filter(name != "Zorgtoeslag") %>% filter(name != "Huurtoeslag")
x %>% ggplot(aes(x = totaal_excl_toeslag_kind, y = n)) + geom_col() + facet_wrap(~ name)

readxl::read_xlsx('Spreiding_stapeling.xlsx', sheet = "Spreiding_regelingen_kindtoesla") %>% ggplot(aes(x = totaal_excl_ts, y = n)) + geom_col() + facet_wrap(~ name)
readxl::read_xlsx('Spreiding_stapeling.xlsx', sheet = "Spreiding_alle_toeslagen")%>% filter(name != "AOW") %>% filter(name != "AKW") %>% filter(name != "Zorgtoeslag") %>% filter(name != "Huurtoeslag") %>% ggplot(aes(x = totaal, y = n)) + geom_col() + facet_wrap(~ name)




welke_uitvoerders = readxl::read_xlsx('welke_uitvoerders_huishoudens.xlsx')

grid.newpage()
draw.quad.venn(area1 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1]),
               area2 = sum(welke_uitvoerders$n[welke_uitvoerders$UWV == 1]),
               area3 = sum(welke_uitvoerders$n[welke_uitvoerders$SVB == 1]),
               area4 = sum(welke_uitvoerders$n[welke_uitvoerders$Gemeenten == 1]),
               n12 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1]),
               n13 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$SVB == 1]),
               n14 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$Gemeenten == 1]),
               n23 = sum(welke_uitvoerders$n[welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1]), 
               n24 = sum(welke_uitvoerders$n[welke_uitvoerders$UWV == 1 & welke_uitvoerders$Gemeenten == 1]), 
               n34 = sum(welke_uitvoerders$n[welke_uitvoerders$SVB == 1 & welke_uitvoerders$Gemeenten == 1]), 
               n123 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1]),
               n124 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$Gemeenten == 1]),
               n234 = sum(welke_uitvoerders$n[welke_uitvoerders$Gemeenten == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1]),
               n134 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$SVB == 1 & welke_uitvoerders$Gemeenten == 1]),
               n1234 = sum(welke_uitvoerders$n[welke_uitvoerders$Belastingdienst == 1 & welke_uitvoerders$UWV == 1 & welke_uitvoerders$SVB == 1 & welke_uitvoerders$Gemeenten == 1]),
               category = c('Belastingdienst', 'UWV', 'SVB', 'Gemeenten'))

