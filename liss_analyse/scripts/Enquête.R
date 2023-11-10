library(haven)
library(tidyr)
library(dplyr)
library(psych)
library(ggplot2)
library(readxl)
library(stargazer)
set.seed(123)

#data inladen####
setwd("O:/AFEP/Onderhanden werk/Romée Lind/Sociale Zekerheid/Enquête")
source('functies.R')

df <- read_sav("data/L_IBO_socialezekerheid_1.0p.sav") %>% pivot_3q() %>% ao_uitsplitsen() %>% pm_toevoegen() %>% 
  beleidsaspecten_koppelen()
names(df)
#SUGGESTIE
#df_b = df %>% mutate(
#  afhankelijk_huishouden = ifelse(kostendelernorm_huishoudenstoets == "Ja", 1, 0)) %>% mutate(
#    afhankelijk_huishouden = ifelse(partnerinkomenstoets == "Ja", 1, afhankelijk_huishouden)
#  )
#persoon_aspecten <- lm(data = df_b[,c(20:22, 25:30, 33, 34)],
#                       complexiteit_corr_pm ~ .)
#
#summary(persoon_aspecten)

#index aanmaken voor regressies####
index_complex <- c(3:10)
index_pm <- c(11:21)
index_beleid = c(24:34)
#stap 1: principal-component analysis ####
correlation_matrix <- cor(df[,index_complex], use = "pairwise.complete.obs")
scree(correlation_matrix) 

pca <- principal(correlation_matrix, rotate = "varimax", nfactors = 1)
print(pca)

#stap 2: samenvattingen per regeling ####
#create dependent variabel
df$complexiteit_unscaled <- predict(pca, data = df[,index_complex])[,1] #maak kolom aan met de pca uitkomsten
df$complexiteit <- ((df$complexiteit_unscaled - min(df$complexiteit_unscaled, na.rm = T))/(max(df$complexiteit_unscaled, na.rm = T)-min(df$complexiteit_unscaled, na.rm = T)))*10
df = df %>% select(-complexiteit_unscaled)
ggplot(df, aes(x = complexiteit)) + geom_histogram() + facet_wrap(~regeling) + theme_bw() #verschillende histogrammen maken met verdeling

complex_q1 <- as.numeric(quantile(df$complexiteit, na.rm = T)[2])
complex_q2 <- as.numeric(quantile(df$complexiteit, na.rm = T)[3])
complex_q3 <- as.numeric(quantile(df$complexiteit, na.rm = T)[4])

complex_sum <- df %>% 
  group_by(regeling) %>% 
  summarise(
    mean_score = mean(complexiteit, na.rm = T),
    perc_very_negative = sum(complexiteit < complex_q1, na.rm = T) / n(),
    perc_very_positive = sum(complexiteit > complex_q3, na.rm = T) / n(),
    perc_above_average = sum(complexiteit > complex_q2, na.rm = T) / n(),
    sd = sd(complexiteit, na.rm = T), 
    n = n()
  ) 

complex_sum %>% ggplot(aes(x = mean_score, y = sd)) + geom_point() #voor de check: is er een grotere spreiding van meningen binnen regelingen die minder / meer ingewikkeld gevonden worden

#stap 4: scores op complexiteit gecorrigeerd voor persoonskenmerken ####
#stap 4a: regressies met persoonskenmerken, om voor populatie-gecorrigeerde afhankelijke variabel te creeëren
persoon<- lm(data= df[,c(index_pm, 35)], complexiteit ~ .)
summary(persoon)

df$complexiteit_pm <- predict(persoon, df)
df$complexiteit_corr_pm <- df$complexiteit - df$complexiteit_pm

df %>% group_by(regeling) %>% 
  summarise(
    complex = mean(complexiteit_corr_pm, na.rm = T),
    pm = mean(complexiteit_pm, na.rm = T)
  )


persoon_regeling <- lm(data= df, 
                       complexiteit ~ 
                         as.factor(V01) + as.factor(woonvorm) + as.factor(woning) + as.numeric(nettoink) + as.factor(oplmet) + life + regeling)
summary(persoon_regeling)  

#conclusie: ook als we regeling meenemen, samen met persoonskenmerken, zien we dat beiden uitmaken --> als we alleen kijken naar persoonskenmerken, verschillen de resultaten niet enorm. 

#stap 4b: nu kunnen we kijken of - gecorrigeerd voor de populatie - de beleidsaspecten veel verklarende waarde hebben

persoon_aspecten <- lm(data = df[,c(index_beleid, 33, 34)],
                       complexiteit_corr_pm ~ .)

summary(persoon_aspecten)


#stap 5: alles in 1 regressie ####
beleid_pm <- lm(data = df[,c(index_beleid, index_pm, 35)], complexiteit ~ .)
summary(beleid_pm)

predict_data <- df[,c(2,index_beleid, index_pm, 35)] %>% 
  mutate(life = 1,
         V01 = "Inkomen van twee personen (uit werk of uitkering)",
         woonvorm = "(On)gehuwd samenwonend, zonder kind(eren)",
         woning = "Koopwoning",
         oplmet = "mbo",
         nettoink = median(nettoink, na.rm = T),
         Agreeableness = 3,
         Conscientiousness = 3,
         Extraversion = 3, 
         Neuroticism = 3,
         OpennessExperience = 3)

predict_data$predict_complex <- predict(beleid_pm, predict_data)
predict_data_mean = predict_data %>% group_by(regeling) %>% summarise(mean_pm = mean(predict_complex, na.rm = T)) 
vergelijking = complex_sum %>% left_join(predict_data_mean) %>% 
  mutate(diff = mean_pm - mean_score) %>% 
  select(regeling, mean_score, mean_pm, everything())

#stap 6: toevoegen van kenmerken van gebruik --> stapelng en persoonlijke situatie #####
gebruik <- read_sav("L_IBO_socialezekerheid_1.0p.sav") %>% select(nomem_encr, regelingenjaar) %>% 
  mutate(stapeling = ifelse(is.na(regelingenjaar), 0, 1)) %>% 
  select(-regelingenjaar)

df <- df %>% left_join(gebruik)
stap5 <- lm(data = df, 
            complexiteit_corr_pm ~ life + stapeling)
summary(stap5)

#vergleijken van numerieke complexiteitsscore ####
regeling_scores <- tribble(
  ~regeling, ~complexiteit_score_original, ~complexiteit_sec,
  'Zorgtoeslag', 6, 5,
  "WGA", 7, 7,
  "IVA", 4, 5,
  "WAO", 5, 5,
  "AOW (Algemene Ouderdomswet)", 2, 3, 
  "Anw (Algemene nabestaandenwet)", 4.5, 5,
  "WW of wachtgeld", 4.5, 5,
  "Ziektewetuitkering", 5.5, 4,
  "Wajong", 6, 4,
  "Kinderbijslag", 2, 2,
  "Kinderopvangtoeslag", 6, 4,
  "Bijstand (Participatiewet)", 8.5, 8,
  "Huurtoeslag", 6, 6,
  "IOW (Inkomensvoorziening Oudere Werklozen)", 4.5, 4,
  "IOAW/IOAZ", 8, 6.5,
  "Bijstand voor zelfstandigen", 8.5, 7
)

#complexiteitsindicatoren toevoegen
complex_sum %>% left_join(regeling_scores) %>%
  ggplot(aes(x = complexiteit_sec, y = mean_score)) + geom_point() + geom_smooth(method = 'lm')

