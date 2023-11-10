modellen_beleid_vergelijken <- function(){
  #Make dataframe with regression results
  #Step 1: make dataframes with the simple regression results of the four main models
  model1 = data.frame(tidy(model_naive)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  model2 = data.frame(tidy(model_beleid)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  model3 = data.frame(tidy(model_pm_beleid)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  model4 = data.frame(tidy(model_fe)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  
  #Merge the model results; summarise to table with significance, min and max coefficients
  modellen <- model1 %>% full_join(model2, by = "term", suffix = c('_naive', '_beleid')) %>% 
    full_join(model3, by = "term") %>% 
    full_join(model4, by = "term", suffix = c('_pmbeleid', "_fixedeffects")) %>% 
    filter(!is.na(estimate_fixedeffects)) %>% 
    select(-contains('p.value')) %>% 
    pivot_longer(cols = -term,
                 names_to = c('var', 'model'),
                 names_pattern = '(.*)_(.*)') %>% 
    group_by(term) %>%
    summarise(
      `Hoogste coëfficiënt` = round(max(value[which(var == 'estimate')], na.rm = T), 2),
      `Laagste coëfficiënt` = round(min(value[which(var == 'estimate')], na.rm = T), 2),
      `Significantie in # modellen` = sum(var == 'sign' & value == 1, na.rm = T)
    ) %>% 
    arrange(`Significantie in # modellen`, desc = F)
  
  
}

regressions_regeling <- function(excluded_regeling){
  df_excluded_regeling <- df %>% filter(regeling != excluded_regeling) 
  
  regressie <- plm(data = df_excluded_regeling, 
                   formula_fe,
                   model = "within",
                   index = "nomem_encr")
  
  summ = summary(regressie)
  
  rsq = c('R2', summ$r.squared[1], NA, NA, NA)
  mse = c('MSE', mean(summ$residuals^2, na.rm = T), NA, NA, NA)
  regressie_resultaten <- data.frame(broom::tidy(regressie))
  regressie_resultaten = rbind.data.frame(regressie_resultaten, rsq, mse)
  
}

make_table_shuffling_regression <- function(output){
  #You can run this function without arguments as well, until the point where you need to decide on the output (see only argument)
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
  
  #attach names to columns, based on what regeling was excluded
  colnames(coefficients)[2:17] <- regeling_namen
  colnames(p_values)[2:17] <- regeling_namen
  
  #store results from fixed effects model to compare
  coef_fe <- tidy(model_fe)[,1:2] %>% select(term, fixed_effects = estimate)
  rsq_fe = c('R2', summary(model_fe)$r.squared[1])
  mse_fe = c('MSE', mean(summary(model_fe)$residuals^2))
  coef_fe = rbind.data.frame(coef_fe, rsq_fe, mse_fe)
  pv_fe <- tidy(model_fe)[,c(1,5)] %>% select(term, fixed_effects = p.value)
  #merge fixed effects results with shuffle_regression results
  coefficients <- coefficients %>% full_join(coef_fe)
  p_values <- p_values %>% full_join(pv_fe)
  
  #give correct term names for the variables for which we show the results
  term <- coefficients$term
  coefficients$term = NULL
  p_values$term = NULL

  #transpose the dataframes to have eligibility criteria in the columns and the excluded regeling in the first column
  coefficients = data.frame(t(coefficients))
  p_values = data.frame(t(p_values))
  
  coefficients$excluded_regeling <- rownames(coefficients)
  p_values$excluded_regeling <- rownames(p_values)
  rownames(coefficients) = NULL
  rownames(p_values) = NULL
  
 #make coefficients numeric, order the table correctly, and make p-values and significance legible.
  coefficients = coefficients %>% select(excluded_regeling, everything()) %>% 
    mutate(across(X1:X12, as.numeric))
  p_values = p_values %>% select(excluded_regeling, everything()) %>% 
    mutate(across(X1:X12, as.numeric)) %>% 
    mutate_if(is.numeric,
              ~ round(., 3))
  p_sign = p_values %>% select(excluded_regeling, everything()) %>% 
    mutate_if(is.numeric,
              ~ ifelse(. < 0.05, TRUE, FALSE))
  
  #give correct names to the columns: eligibility criteria in the columns
  names(coefficients)[2:14] = term
  names(p_values)[2:14] = term
  names(p_sign)[2:14] = term
  
  #remove non-sensible columns
  p_values = p_values %>% select(-R2, -MSE)
  p_sign = p_sign %>% select(-R2, -MSE)

  #Here you can choose the type of output you want. 
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
    
    p_both <- p_values %>% full_join(p_sign, by = "excluded_regeling", suffix = c('_pvalue', '_sign')) 
    p_both = p_both %>% select(excluded_regeling, sort(names(p_both), decreasing = T)) %>% select(excluded_regeling, everything())
    return(p_both)
  }
  
  if (output == "alles") {
    coefficients = coefficients %>% select(excluded_regeling, R2, MSE, everything())
    colnames(coefficients)[4:14] = paste(colnames(coefficients)[4:14], "coef" , sep= "_")
    p_both <- p_values %>% full_join(p_sign, by = "excluded_regeling", suffix = c('_pvalue', '_sign')) 
    p_both = p_both %>% select(excluded_regeling, sort(names(p_both), decreasing = T)) %>% select(excluded_regeling, everything())
    alles = p_both %>% full_join(coefficients, by = "excluded_regeling") 
    alles = alles %>% select(sort(names(alles), decreasing = F)) %>% select(excluded_regeling, R2, MSE, everything())
  }
}

model_vergelijking_maken <- function(){
  model1 = data.frame(tidy(model_naive)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  model2 = data.frame(tidy(model_beleid)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  model3 = data.frame(tidy(model_pm_beleid)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  model4 = data.frame(tidy(model_fe)[,c(1,2,5)]) %>% mutate(sign = ifelse(p.value < 0.05, TRUE, FALSE))
  
  summ_m1 = summary(model_pm)
  mse_1 = c("MSE", mean(summ_m1$residuals^ 2), NA, NA)
  rsq_1 = c('R2', summ_m1$r.squared, NA, NA)
  summ_m2 = summary(model_beleid)
  mse_2 = c(mean(summ_m2$residuals^ 2), NA, NA)
  rsq_2 = c(summ_m2$r.squared, NA, NA)
  summ_m3 = summary(model_pm_beleid)
  mse_3 = c(mean(summ_m3$residuals^ 2), NA, NA)
  rsq_3 = c(summ_m3$r.squared, NA, NA)
  summ_m4 = summary(model_fe)
  mse_4 = c(mean(summ_m4$residuals^ 2), NA, NA)
  rsq_4 = c(summ_m4$r.squared[1], NA, NA)
  
  mse = c(mse_1, mse_2, mse_3, mse_4)
  rsq = c(rsq_1, rsq_2, rsq_3, rsq_4)
  
  modellen <- model1 %>% full_join(model2, by = "term", suffix = c('_pm', '_beleid')) %>% 
    full_join(model3, by = "term") %>% 
    full_join(model4, by = "term", suffix = c('_pm_beleid', "_fixedeffects"))
  
  modellen = rbind.data.frame(modellen, mse)
  modellen = rbind.data.frame(modellen, rsq)
}