beleidsaspecten_koppelen <- function(.data){
  beleidsaspecten <- readxl::read_xlsx('Kopie van Complexiteit indicator - 6 sept v2.xlsx', sheet = "Blad2", col_names = T)
  x <- colnames(beleidsaspecten)
  colnames(beleidsaspecten) <- c('Regeling', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm')
  beleidsaspecten = beleidsaspecten %>% 
    mutate(k = ifelse(k == "Deels verrekend", "Deels / met uitzonderingen", k),
           k = ifelse(k == "Volledig verrekend, tenzij tijdelijk gedeeltelijk niet/uitzondering", "Deels / met uitzonderingen", k)) %>% 
    mutate(across(c:m, factor))  %>% 
    rename("type" = "a",
           "complexiteit_score" = "b",
           "inkomenstoets" = "c",
           "gebaseerd_dagloon" = "d",
           'vermogenstoets' = "e",
           "kostendelernorm_partnertoets" = "f",
           "kolom1" = "g",
           "verrekening_gebruik_restverdiencapaciteit" = "h",
           "arbeidsverplichtingen" = "i",
           "voorschotten_systematiek" = "j",
           "consequenties_bijverdienen" = "k",
           "hoogte_verandert_tijd" = "l",
           "herlevingsrecht_na_uitstroom" = "m")
  beleidsaspecten$inkomenstoets = relevel(factor(beleidsaspecten$inkomenstoets), ref = "Nee")
  beleidsaspecten$gebaseerd_dagloon = relevel(as.factor(beleidsaspecten$gebaseerd_dagloon), ref = "Nee")
  beleidsaspecten$vermogenstoets = relevel(as.factor(beleidsaspecten$vermogenstoets), ref = "Nee")
  beleidsaspecten$kostendelernorm_partnertoets = relevel(as.factor(beleidsaspecten$kostendelernorm_partnertoets), ref = "Nee")
  beleidsaspecten$verrekening_gebruik_restverdiencapaciteit = relevel(as.factor(beleidsaspecten$verrekening_gebruik_restverdiencapaciteit), ref = "Nee")
  beleidsaspecten$arbeidsverplichtingen = relevel(as.factor(beleidsaspecten$arbeidsverplichtingen), ref = "Nee")
  beleidsaspecten$voorschotten_systematiek = relevel(as.factor(beleidsaspecten$voorschotten_systematiek), ref = "Nee")
  beleidsaspecten$consequenties_bijverdienen = relevel(as.factor(beleidsaspecten$consequenties_bijverdienen), ref = "Geen")
  beleidsaspecten$hoogte_verandert_tijd = relevel(as.factor(beleidsaspecten$hoogte_verandert_tijd), ref = "Nee")
  beleidsaspecten$herlevingsrecht_na_uitstroom = relevel(as.factor(beleidsaspecten$herlevingsrecht_na_uitstroom), ref = "Gegarandeerd herlevingsrecht als je aan bepaalde voorwaarden voldoet")
  
  df <- .data %>% 
    mutate(regeling = recode(regeling,
                             "Anw (Algemene nabestaandenwet)" = "Anw", 
                             "AOW (Algemene Ouderdomswet)" = "AOW",
                             "Bijstand (Participatiewet)" = "Bijstand",
                             'Bijstand voor zelfstandigen' = "Bbz",
                             "IOW (Inkomensvoorziening Oudere Werklozen)" = "IOW",
                             "Kinderopvangtoeslag" = "Kinderopvangtoeslag",
                             "WW of wachtgeld" = "WW",
                             "Ziektewetuitkering" = "Ziektewet")) %>% 
    left_join(beleidsaspecten, by = c('regeling'='Regeling'))
}

index_complex <- c(3:10)
index_pm <- c(11:22)
index_beleid = c(25:34)

correlation_matrix <- cor(df[,index_complex], use = "pairwise.complete.obs")
scree(correlation_matrix) 

pca <- principal(correlation_matrix, rotate = "varimax", nfactors = 1)
print(pca)

df$complexiteit_unscaled <- predict(pca, data = df[,index_complex])[,1] #maak kolom aan met de pca uitkomsten
df$complexiteit <- (max(df$complexiteit_unscaled, na.rm = T) - df$complexiteit_unscaled)/(max(df$complexiteit_unscaled, na.rm = T)-min(df$complexiteit_unscaled, na.rm = T))*10
df = df %>% select(-complexiteit_unscaled)

#regressiemodellen####
#model 1: alleen persoonskenmerken (+ aanmaken variabel om voor persoonlijkheid te corrigeren)
model_pm <- lm(data= df[,c(index_pm, 37)], complexiteit ~ .)
df$complexiteit_pm <- predict(model_pm, df)
df$error_pm <- df$complexiteit - df$complexiteit_pm

#model 2: beleidsaspecten met voor persoonskenmerken gecorrigeerde complexiteit
model_beleid <- lm(data = df[,c(index_beleid, 39)], error_pm ~ .)

#model 3: zowel beleidsaspecten als persoonskenmerken
model_pm_beleid <- lm(data = df[,c(index_beleid, index_pm, 36)], complexiteit ~ .)

#model 4: fixed effects
model_fe <- plm(data = df, 
                complexiteit ~ inkomenstoets + gebaseerd_dagloon + vermogenstoets + kostendelernorm_partnertoets +
                  verrekening_gebruik_restverdiencapaciteit + arbeidsverplichtingen + voorschotten_systematiek + consequenties_bijverdienen +
                  hoogte_verandert_tijd + herlevingsrecht_na_uitstroom + life,
                model = "within",
                index = "nomem_encr")

summary(model_pm)
summary(model_beleid)
summary(model_pm_beleid)
summary(model_fe)



regressions_regeling <- function(excluded_regeling){
  df_excluded_regeling <- df %>% filter(regeling != excluded_regeling) 
  
  regressie <- plm(data = df_excluded_regeling, 
                   complexiteit ~ inkomenstoets + gebaseerd_dagloon + vermogenstoets + kostendelernorm_partnertoets +
                     verrekening_gebruik_restverdiencapaciteit + arbeidsverplichtingen + voorschotten_systematiek + consequenties_bijverdienen +
                     hoogte_verandert_tijd + herlevingsrecht_na_uitstroom + life,
                   model = "within",
                   index = "nomem_encr")
  
  summ = summary(regressie)
  
  rsq = c('R2', summ$r.squared[1], NA, NA, NA)
  mse = c('MSE', mean(summ$residuals^2, na.rm = T), NA, NA, NA)
  regressie_resultaten <- data.frame(broom::tidy(regressie))
  
  regressie_resultaten = rbind.data.frame(regressie_resultaten, rsq, mse)
  
}

make_table_shuffling_regression <- function(output){
  regeling_namen <- unique(df$regeling)
  
  coefficients = data.frame(tidy(model_fe)[,1])
  coefficients = rbind.data.frame(coefficients, 'R2', 'MSE')
  p_values = data.frame(tidy(model_fe)[,1])
  p_values = rbind.data.frame(p_values, 'R2', 'MSE')
  
  for (i in 1:length(regeling_namen)) {
    regressie_coefficient <- regressions_regeling(regeling_namen[i])[,c(1,2)]
    regressie_pvalue <- regressions_regeling(regeling_namen[i])[,c(1,5)]
    
    coefficients <- coefficients %>% left_join(regressie_coefficient, by = "term")
    p_values <- p_values %>% left_join(regressie_pvalue, by = "term")
    
  }
  
  
  colnames(coefficients)[2:17] <- regeling_namen
  colnames(p_values)[2:17] <- regeling_namen
  
  term <- coefficients$term
  
  coef_fe <- tidy(model_fe)[,1:2] %>% select(term, fixed_effects = estimate)
  rsq_fe = c('R2', summary(model_fe)$r.squared[1])
  mse_fe = c('MSE', mean(summary(model_fe)$residuals^2))
  coef_fe = rbind.data.frame(coef_fe, rsq_fe, mse_fe)
  
  pv_fe <- tidy(model_fe)[,c(1,5)] %>% select(term, fixed_effects = p.value)
  coefficients <- coefficients %>% full_join(coef_fe)
  p_values <- p_values %>% full_join(pv_fe)
  term <- coefficients$term
  coefficients$term = NULL
  p_values$term = NULL
  
  
  p_values = p_values %>% select(fixed_effects, everything())
  coefficients = coefficients %>% select(fixed_effects, everything())
  
  coefficients = data.frame(t(coefficients))
  p_values = data.frame(t(p_values))
  
  coefficients$excluded_regeling <- rownames(coefficients)
  p_values$excluded_regeling <- rownames(p_values)
  rownames(coefficients) = NULL
  rownames(p_values) = NULL
  
  
  coefficients = coefficients %>% select(excluded_regeling, everything()) %>% 
    mutate(across(X1:X14, as.numeric))
  p_values = p_values %>% select(excluded_regeling, everything()) %>% 
    mutate(across(X1:X14, as.numeric)) %>% 
    mutate_if(is.numeric,
              ~ round(., 3))
  p_sign = p_values %>% select(excluded_regeling, everything()) %>% 
    mutate_if(is.numeric,
              ~ ifelse(. < 0.05, TRUE, FALSE))
  
  names(coefficients)[2:15] = term
  names(p_values)[2:15] = term
  names(p_sign)[2:15] = term
  
  p_values = p_values %>% select(-R2, -MSE)
  p_sign = p_sign %>% select(-R2, -MSE)
  
  p_both <- p_values %>% full_join(p_sign, by = "excluded_regeling", suffix = c('_pvalue', '_sign')) 
  p_both = p_both %>% select(excluded_regeling, sort(names(p_both), decreasing = T)) %>% select(excluded_regeling, everything())
  
  if (output == "coef") {
    coefficients = coefficients %>% select(excluded_regeling, R2, MSE, everything())
    return(coefficients)
  }
  
  if (output == "p val"){
    return(p_values)
  }
  
  if (output == "p sign") {
    return(p_sign)
  }
  
  if (output == "p both") {
    return(p_both)
  }
  
  if (output == "alles") {
    coefficients = coefficients %>% select(excluded_regeling, R2, MSE, everything())
    colnames(coefficients)[4:15] = paste(colnames(coefficients)[4:15], "coef" , sep= "_")
    alles = p_both %>% full_join(coefficients, by = "excluded_regeling") 
    alles = alles %>% select(sort(names(alles), decreasing = F)) %>% select(excluded_regeling, R2, MSE, everything())
  }
}

summary <- make_table_shuffling_regression('coef')


