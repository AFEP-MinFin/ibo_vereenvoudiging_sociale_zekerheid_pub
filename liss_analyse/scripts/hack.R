
df_complex_indicatoren <- df %>% left_join(regeling_scores)
persoon<- lm(data= df_complex_indicatoren, gecorr_compl ~ complexiteit_score_original + as.factor(V01) + leeftijd + as.factor(woonvorm) + as.factor(woning) + nettoink + as.factor(oplmet) + life)
summary(persoon)

#hoeveel mensen hebben een life-event meegemaakt
life = df %>% select(regeling, life) %>% 
  mutate(life = recode(as.character(life),
                       "1" = "Heel weinig",
                       "2" = "Weinig",
                       "3" = "Niet veel, niet weinig",
                       "4" = "Veel",
                       "5" = "Heel veel"))

life_samenv = life %>%  filter(!is.na(life)) %>% 
  group_by(life) %>%
  summarise(perc = n()/1916)

life_sum = life %>% filter(!is.na(life)) %>% 
  group_by(regeling, life) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(regeling) %>% 
  mutate(perc = n / sum(n, na.rm = T))

getwd()
writexl::write_xlsx(life_samenv, 'life_events_gem.xlsx')
writexl::write_xlsx(life_sum, 'life_events_reg.xlsx')


#aspecten


aspecten <- lm(data = df_aspecten[,-c(1:10, 17, 18, 20, 21, 23, 24)], gecorr_compl ~ .)
summary(aspecten)

names(df_aspecten)


#decision-tree
tree <- ctree(data = df_aspecten %>% select(where(is.numeric)) %>% na.omit(), formula = complex_comm_pca ~ .)
plot(tree)

tree <- rpart(data = df_tree, formula = complexiteit ~ ., maxdepth = 10, minbucket = 2, minsplit = 2, cp = 0.0001, method = 'anova')
prp(tree)

tree <- rpart(data = df_aspecten %>% select(where(is.numeric)) %>% na.omit(), formula = complex_comm_pca ~ ., maxdepth = 10, minbucket = 10, minsplit = 10, cp = 0.00001, method = 'anova')
prp(tree, varlen = 0)

library(randomForest)
data = df_aspecten %>% filter(!is.na(complex_comm_pca))

rf <- randomForest(complex_comm_pca ~ ., data = df_aspecten %>% filter(!is.na(complex_comm_pca)) %>% data.frame(.))
summary(rf)

rf <- randomForest(factor(complex_comm_pca) ~ ., data = df_aspecten %>% filter(!is.na(complex_comm_pca)) %>% 
                     mutate(complex_comm_pca = complex_comm_pca < -0.6) %>% select(-nomem_encr) %>% data.frame(.))
print(rf)



data_asp_pm <- df_aspecten %>% left_join(pm) %>% select(-nomem_encr) %>% na.omit() %>% select(-V04, -V01)

data_asp_pm = data_asp_pm %>% mutate(
  woonvorm = as.factor(woonvorm),
  woning = as.factor(woning), 
  oplmet = as.factor(oplmet)
) 
reg <- lm(complex_comm_pca ~ ., data = data_asp_pm %>% filter(!is.na(complex_comm_pca)) %>% data.frame(.))
summary(reg)

rf <- randomForest(complex_comm_pca ~ ., data = data_asp_pm %>% filter(!is.na(complex_comm_pca)) %>% data.frame(.))
summary(rf)

varImpPlot(rf)

tree <- rpart(data = data_asp_pm %>% select(where(is.numeric)) %>% na.omit(), formula = complex_comm_pca ~ ., maxdepth = 5, minbucket = 100, minsplit = 10, cp = 0.01, method = 'anova')
prp(tree, varlen = 0)

#regressie####