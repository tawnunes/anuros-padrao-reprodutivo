# PACOTES ------------------------------------------------------------------------------------------------------

library(tidyverse) # manipulação do dados
library(ggplot2) # visualização gráfica
library(lme4)


# IMPORTANTDO OS DADOS -----------------------------------------------------------------------------------------

anuros_2 <- read.csv2("data/processed/dados_anuros_completo_2.csv")

anuros_2 <- anuros_2 %>% 
    mutate(male_svl_mm = as.numeric(male_svl_mm),
           fecundity_measure = as.numeric(fecundity_measure),
           evidences_territoriality = factor(tolower(evidences_territoriality)))


anuros_2 %>% filter(fecundity_unit == "eggs per clutch") %>% 
    group_by(evidences_territoriality) %>% 
    summarise(mean_male_size = mean(male_svl_mm, na.rm = T),
              median_male_size = median(male_svl_mm, na.rm = T),
              mean_fecundity = mean(fecundity_measure),
              median_fecundity = median(fecundity_measure))


boxplot(anuros_2$fecundity_measure ~ anuros_2$evidences_territoriality)




anuros_territ <- anuros_2 %>% 
    mutate(territorial = ifelse(anuros_2$evidences_territoriality == "yes", 1, 0)) %>% 
    filter(fecundity_unit != "actual clutches") %>% 
    filter(fecundity_unit != "oocytes")

modelo1 <- glmer(territorial ~ male_svl_mm +(1|family), family = binomial, data = anuros_territ)
summary(modelo1)

modelo2 <- glmer(territorial ~ scale(fecundity_measure) +(1|family), family = binomial, data = anuros_territ)
summary(modelo2)
