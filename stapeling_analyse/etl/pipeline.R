library(haven)
library(tidyverse)
library(fastDummies)
library(feather)

setwd("H:/Sociale zekerheid/socialezekerheid")

source("R/handige_functies.R")
source("R/bestanden_koppelen.R")
source("R/maandelijkse data inladen.R")

#df = stapelingsmonitor met dummies voor alle regelingen
df <- bestanden_koppelen()

#optellen per persoon####
df$totaal = rowSums(df[,-c(1:3)], na.rm =  T)
df$totaal_excl_toeslag_kind = rowSums(df[,-c(1:3, 19:24)], na.rm = T)
df$totaal_excl_toeslag = rowSums(df[,-c(1:3, 19, 20, 22, 24:25)], na.rm = T)

df_pivot <- df %>% pivot_longer(cols = Bijstand:AKW)

#wegschrijven dfs
write_feather(df, "data/dummies_20_2.feather")
write_feather(df_pivot, "data/dummies_pivot_20_2.feather")
