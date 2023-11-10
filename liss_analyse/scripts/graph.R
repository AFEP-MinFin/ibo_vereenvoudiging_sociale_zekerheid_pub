library(magrittr)

setwd("~/liss/LISS Data")
source('functies.R')

df <- read_sav("data/L_IBO_socialezekerheid_rapport_2.0p.sav") %>% pivot_questions() %>% ao_uitsplitsen() %>% mutate(
  regeling_graph = recode(regeling,
                          "Anw (Algemene nabestaandenwet)" = "Anw", 
                          "AOW (Algemene Ouderdomswet)" = "AOW",
                          "Bijstand (Participatiewet)" = "Bijstand",
                          'Bijstand voor zelfstandigen' = "Bbz",
                          "IOW (Inkomensvoorziening Oudere Werklozen)" = "IOW",
                          "Kinderopvangtoeslag" = "KOT",
                          "WW of wachtgeld" = "WW",
                          "Ziektewetuitkering" = "ZW")) %>% 
  mutate(complex1 = as_factor(complex1),
         complex2 = as_factor(complex2),
         complex3 = as_factor(complex3),
         comm1 = as_factor(comm1),
         comm2 = as_factor(comm2),
         comm3 = as_factor(comm3),
         comm4 = as_factor(comm4),
         comm5 = as_factor(comm5),
         life = as_factor(life)) %>% 
  mutate(across(complex1:comm5, ~ case_when(
    . == "Zeer oneens" ~ "(Zeer) oneens",
    . == "Oneens" ~ "(Zeer) oneens",
    . == "Zeer eens" ~ "(Zeer) eens",
    . == "Eens" ~ "(Zeer) eens",
    . == "Niet oneens/niet eens" ~ "Niet oneens/niet eens"
  ))) %>% 
  mutate(complex1 = factor(complex1, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),
         complex2 = factor(complex2, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),
         complex3 = factor(complex3, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),
         comm1 = factor(comm1, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),
         comm2 = factor(comm2, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),
         comm3 = factor(comm3, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),
         comm4 = factor(comm4, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),
         comm5 = factor(comm5, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')),)

 
x = df %>% make_graph_regeling("life", "Heel veel") %>% ggplot(aes(x = regeling_graph, y = perc, fill = indic)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#005B8E","#5889B2","#A1b9d3","#f5d4b3","#e17000")) +
  xlab("Regeling") + ylab("Percentage") +
  labs(fill = "Antwoord")



#problemen


terugvordering =liss %>% make_graph_regeling("terugvordering", "Nee")
terugvordering_schuld = liss %>% make_graph_regeling("terugvordering_schuld", "Nee")



voorwaarden = liss %>% make_graph_regeling('V13', 'Zeer eens')

niet_gebruik = liss %>% make_graph_regeling('V46')

hulp = liss %>%
  mutate(V18 = as.character(V18)) %>%  
  mutate(
    antwoord = recode(V18,
      '1' = "UWV",
      '2' = "Gemeente",
      '3' = "UWV én gemeente",
      '4' = "Geen van beiden"
    )
  ) %>% 
  make_graph_regeling('antwoord', 'Geen van beiden') 

werk = liss %>%
  mutate(antwoord = recode(V15, 
                           "1" = "Ja",
                           "2" = "Nee")) %>% 
  make_graph_regeling('antwoord', 'Ja') 

werk_inkomen = liss %>% make_graph_regeling('V16') 

voorspelbaar_inkomen = liss %>% make_graph_regeling('V12', '(Zeer) oneens')

extra = list('Terugvorderingen' = terugvordering,
             "Terugvorderingen_schuld" = terugvordering_schuld,
             'Voorwaarden_kennen' = voorwaarden,
             "Hulp_vinden_werk" = hulp,
             'heeft_gewerkt' = werk,
             'werk_inkomen' = werk_inkomen,
             'uitkering_voorspelbaar_inkomen' = voorspelbaar_inkomen)

openxlsx::write.xlsx(extra, 'output/Grafieken_gebruik.xlsx')

#dilemmas
dilemma <- read_sav("data/L_IBO_socialezekerheid_rapport_2.0p.sav") %>% select(nomem_encr, regeling_1 = regeling1, regeling_2 = regeling2, regeling_3 = regeling3, V47, V48, V49, V50) %>%
  pivot_longer(cols = regeling_1:regeling_3,
               names_to = c('.value', 'regeling_nr'),
               names_pattern = '(.*)_(.*)') %>% 
  select(regeling, everything(), - regeling_nr) %>% 
  ao_uitsplitsen() %>% 
  mutate(
    regeling_graph = recode(regeling,
                            "Anw (Algemene nabestaandenwet)" = "Anw", 
                            "AOW (Algemene Ouderdomswet)" = "AOW",
                            "Bijstand (Participatiewet)" = "Bijstand",
                            'Bijstand voor zelfstandigen' = "Bbz",
                            "IOW (Inkomensvoorziening Oudere Werklozen)" = "IOW",
                            "Kinderopvangtoeslag" = "KOT",
                            "WW of wachtgeld" = "WW",
                            "Ziektewetuitkering" = "ZW")) %>% 
  filter(regeling != "Geen regeling") %>% 
  filter()

dilemma %>% make_graph_dilemma('V47') + 
  ggtitle('Hebt u liever op tijd een uitkering (0), of liever dat u vooraf zeker weet dat het bedrag helemaal klopt en sowieso niets hoeft terug te betalen (100)?')

dilemma %>% make_graph_dilemma('V48') + 
  ggtitle('Welke manier van dienstverlening is voor u eenvoudiger? Digitaal (0) of persoonlijk (100)?')

dilemma %>% make_graph_dilemma('V49') +
  ggtitle('Hebt u liever dat de overheid u actief benadert als er een kans bestaat dat u recht hebt op een regeling (0), of dat u zelf onderzoek moet doen (100)?')

dilemma %>% make_graph_dilemma('V50') + 
  ggtitle('Hebt u liever dat een regeling precies hetzelfde is voor iedereen (0) of dat de overheid de regeling afstemt op uw persoonlijke situatie (100)?')

#pm analyse ####
df <- read_sav("L_IBO_socialezekerheid_1.0p.sav") %>% pivot_3q() %>% ao_uitsplitsen() %>% pm_toevoegen() %>% mutate(
  regeling_graph = recode(regeling,
                          "Anw (Algemene nabestaandenwet)" = "Anw", 
                          "AOW (Algemene Ouderdomswet)" = "AOW",
                          "Bijstand (Participatiewet)" = "Bijstand",
                          'Bijstand voor zelfstandigen' = "Bbz",
                          "IOW (Inkomensvoorziening Oudere Werklozen)" = "IOW",
                          "Kinderopvangtoeslag" = "KOT",
                          "WW of wachtgeld" = "WW",
                          "Ziektewetuitkering" = "ZW"))

correlation_matrix <- cor(df[,c(3:10)], use = "pairwise.complete.obs")
scree(correlation_matrix) 
pca <- principal(correlation_matrix, rotate = "varimax", nfactors = 1)
df$complexiteit <- predict(pca, data = df[,c(3:10)])
complex_q1 <- as.numeric(quantile(df$complexiteit, na.rm = T)[2])
complex_q2 <- as.numeric(quantile(df$complexiteit, na.rm = T)[3])
complex_q3 <- as.numeric(quantile(df$complexiteit, na.rm = T)[4])

df %>% group_by(regeling_graph, oplcat) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n / sum(n)) %>% 
  filter(regeling_graph != "WGA, IVA of WAO") %>% 
  ggplot(aes(x = regeling_graph, y = perc, fill = oplcat)) + geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#005B8E","#5889B2","#A1b9d3","#fff4db","#ffe9b8","#ffb612")) 

df %>% group_by(regeling_graph, woonvorm) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n / sum(n)) %>% 
  filter(regeling_graph != "WGA, IVA of WAO") %>% 
  ggplot(aes(x = regeling_graph, y = perc, fill = woonvorm)) + geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#005B8E","#5889B2","#A1b9d3","#fff4db","#ffe9b8","#ffb612")) 


df <- df %>% mutate(
  complexiteit_cat = case_when(
    complexiteit <= complex_q1 ~ "Laagste kwartiel",
    complexiteit > complex_q1 & complexiteit <= complex_q2 ~ "Tweede kwartiel",
    complexiteit > complex_q2 & complexiteit <= complex_q3 ~ "Derde kwartiel",
    complexiteit > complex_q3 ~ "Hoogste kwartiel"))%>% 
  mutate(complexiteit_cat = factor(complexiteit_cat, levels = c('Hoogste kwartiel', "Derde kwartiel", "Tweede kwartiel", "Laagste kwartiel")))

df %>% group_by(oplmet) %>% 
  summarise(n = n(), 
            complex = mean(complexiteit, na.rm = T)) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = oplmet, y = complex)) + geom_bar(stat = "identity") 

df %>% group_by(woonvorm) %>% 
  summarise(n = n(), 
            complex = mean(complexiteit, na.rm = T)) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = woonvorm, y = complex)) + geom_bar(stat = "identity")

df %>% group_by(nettocat) %>% 
  summarise(n = n(), 
            complex = mean(complexiteit, na.rm = T)) %>% 
  mutate(perc = n / sum(n), 
         nettocat = as_factor(nettocat)) %>% 
  ggplot(aes(x = nettocat, y = complex)) + geom_bar(stat = "identity")

  
df %>% group_by(oplcat, complexiteit_cat) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(complexiteit_cat)) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = complexiteit_cat, y = perc, fill = oplcat)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#005B8E","#5889B2","#A1b9d3","#f5d4b3","#e17000")) 
 
df %>% 
  mutate(inkomen = as_factor(nettocat)) %>% 
  group_by(inkomen, complexiteit_cat) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(inkomen)) %>% 
  filter(!is.na(complexiteit_cat)) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = inkomen, y = perc, fill = complexiteit_cat)) +
  geom_bar(stat = "identity")

df %>% group_by(woonvorm, complexiteit_cat) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(complexiteit_cat)) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = woonvorm, y = perc, fill = complexiteit_cat)) +
  geom_bar(stat = "identity")

df %>% ggplot(aes(x = oplmet, y = complexiteit)) + geom_boxplot() 
df %>% ggplot(aes(x = nettoink, y = complexiteit)) + geom_point()
df %>% ggplot(aes(x = woonvorm, y = complexiteit)) + geom_boxplot()
