#In this file, you can find useful code for repeated manipulations. 
recode_vars_for_graphs <- function(.data){
  .data %>% 
    mutate(across(complex1:life, ~ as_factor(.))) %>% 
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
           comm5 = factor(comm5, levels = c('(Zeer) eens', 'Niet oneens/niet eens', '(Zeer) oneens')))
  
}

#FOR GRAPHS
make_graph_regeling <- function(.data, indicator, sort){
  
  x <- .data %>% select(regeling, indic = !!ensym(indicator)) %>%
    mutate(indic = as_factor(indic)) %>% 
    group_by(regeling, indic) %>% summarise(n = n()) %>% 
    filter(!is.na(regeling)) %>% 
    ungroup() %>% 
    filter(!is.na(indic)) %>% 
    group_by(regeling) %>% 
    mutate(perc = n / sum(n, na.rm = T))
  
  order <- x %>% filter(indic == sort) %>% arrange(perc) %>% extract2("regeling")
  order <- append(unique(df$regeling[!df$regeling %in% order]), order) #necessary in case a regeling does not have an answer in the sorting category (otherwise, regeling turns into NA)
  x <- x %>% mutate(regeling_graph = factor(regeling, order))
  
  plot = x %>% ggplot(aes(x = regeling_graph, perc, y = perc, fill = indic)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#005B8E","#5889B2","#A1b9d3","#f5d4b3","#e17000")) +
    xlab("Regeling") + ylab("Percentage") +
    labs(fill = "Antwoord") + coord_flip() +
    theme_bw()
}





#FOR TABLES
make_table_regeling <- function(.data, indicator, sort){
  
  x <- .data %>% select(regeling, !!ensym(indicator)) %>% 
    mutate(indic = !!ensym(indicator)) %>% 
    group_by(regeling, indic) %>% summarise(n = n()) %>% 
    filter(!is.na(regeling)) %>% 
    ungroup() %>% 
    filter(!is.na(indic)) %>% 
    mutate(perc = n / sum(n, na.rm = T))
  #pivot_wider(names_from = indic, values_from = c(n, perc))
}