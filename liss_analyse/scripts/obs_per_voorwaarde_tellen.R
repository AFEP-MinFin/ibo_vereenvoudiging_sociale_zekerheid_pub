obs_per_voorwaarden_tellen <- function(){
  voorwaarden = names(df)[33:41]
  voorwaarden_telling = df %>% group_by(!!sym(voorwaarden[1])) %>% 
    summarise(n = n()) %>% 
    mutate(Voorwaarde = voorwaarden[1]) %>% 
    rename('Categorie' = voorwaarden[1])
  
  for (i in 2:length(voorwaarden)) {
    voorwaarde_count = df %>% group_by(!!sym(voorwaarden[i])) %>% 
      summarise(n = n())%>% 
      mutate(Voorwaarde = voorwaarden[i]) %>% 
      rename('Categorie' = voorwaarden[i])
    
    voorwaarden_telling = rbind.data.frame(voorwaarden_telling, voorwaarde_count)
  }
  voorwaarden_telling = voorwaarden_telling %>% select(voorwaarde = Voorwaarde, Categorie, `Aantal observaties` = n)
  return(voorwaarden_telling)
}
