regressions_regeling <- function(excluded_regeling){
  df_excluded_regeling <- df %>% filter(regeling != "WAO") %>% score_koppelen()
  
  regressie <- plm(data = df, 
                   complexiteit ~ inkomenstoets + gebaseerd_dagloon + vermogenstoets + kostendelernorm_huishoudenstoets + partnerinkomenstoets +
                     verrekening_gebruik_restverdiencapaciteit + arbeidsverplichtingen + voorschotten_systematiek + consequenties_bijverdienen +
                     hoogte_verandert_tijd + herlevingsrecht_na_uitstroom + score,
                   model = "within",
                   index = "nomem_encr")
  
  
  regressie_resultaten <- data.frame(broom::tidy(regressie))
  
}
regeling_namen <- unique(df$regeling)

res <- regressions_regeling("WAO")

coefficients <- data.frame(matrix(ncol = 0, nrow = 13))
p_values <- data.frame(matrix(ncol = 0, nrow = 13))

coefficients = data.frame(tidy(model_beleid)[,1])
coefficients[14,1] <- "score" 
p_values = data.frame(tidy(model_beleid)[,1])
p_values[14,1] <- "score" 

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
pv_fe <- tidy(model_fe)[,c(1,5)] %>% select(term, fixed_effects = p.value)
coefficients <- coefficients %>% left_join(coef_fe)
p_values <- p_values %>% left_join(pv_fe)
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

names(coefficients)[1:13] = term
names(p_values)[1:13] = term

coefficients = coefficients %>% select(excluded_regeling, everything())
p_values = p_values %>% select(excluded_regeling, everything())
p_values_sign = p_values %>% 
  mutate_if(is.numeric,
            ~ ifelse(. < 0.05, TRUE, FALSE))

coefficients_long <- coefficients %>% pivot_longer(cols = `(Intercept)`:`herlevingsrecht_na_uitstroomGegarandeerd herlevingsrecht als je aan bepaalde voorwaarden voldoet`)

coefficients_bandwidth <- coefficients_long %>% group_by(name) %>% 
  summarise(min = min(value, na.rm = T),
            max = max(value, na.rm = T),
            median = median(value, na.rm = T)) %>% 
  left_join(coef_fe, by = c('name' = 'term')) %>% 
  left_join(pv_fe, by = c('name' = 'term')) %>% 
  mutate(highly_sign = ifelse(fixed_effects.y < 0.05, T, F),
         sign = ifelse(fixed_effects.y < 0.10, T, F)) %>% 
  select(name, fixed_effects_est = fixed_effects.x, sign, everything(), -fixed_effects.y)

coefficients_long %>% ggplot(aes(x = value)) + geom_density() + facet_wrap(~name)
