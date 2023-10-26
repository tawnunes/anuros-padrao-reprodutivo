# Universidade Federal do Paraná
# 
# Ecologia Comportamental 2023
# 
# Prof. Dr. Lilian Tonelli Manica
# 
# Discentes: Jonathan, Ruth Oliveira, Tawane Nunes
# 
# Comportamento Reprodutivo em anuros
# 
# Script 04: Familia Ranidae

# PACOTES ------------------------------------------------------------------------------------------------------

library(tidyverse) # manipulação dos dados
library(ggplot2) # visualização gráfica

# IMPORTANTDO OS DADOS -----------------------------------------------------------------------------------------

ranidae <- read.csv("data/processed/ranidae.csv")

ranges_ranidae <- read.csv("data/processed/ranges_ranidae.csv")

ranges_ranidae <- ranges_ranidae %>% 
    rename(species = especie)

# UNINDO INFORMAÇÕES --------------------------------------------------------------------------------------------

ranidae_completo <- ranidae %>% 
    left_join(ranges_ranidae, by = "species") %>% 
    mutate(breeding_pattern = tolower(breeding_pattern),
           prolonged = ifelse(breeding_pattern == "prolonged", 1,0)) %>% 
    mutate(lat_max = abs(lat_max),
           lat_min = abs(lat_min))


ranidae_completo %>% group_by(breeding_pattern) %>% 
summarise(n_species = n(),
          min_area = min(total_area),
          max_area = max(total_area),
          mediana = median(total_area))

ranidae_completo %>% group_by(breeding_pattern) %>% 
    summarise(n_species = n(),
              min_lat = min(lat_min),
              max_lat = max(lat_max),
              mediana = mean(amplitude_lat))

# ANÁLISES ------------------------------------------------------------------------------------------------------

modelo1 <- glm(prolonged ~ total_area_norm + amplitude_lat + centroide_lat, family = binomial, data = ranidae_completo)
summary(modelo1)

modelo2 <- glm(prolonged ~ amplitude_lat + centroide_lat, family = binomial, data = ranidae_completo)
summary(modelo2)

modelo6 <- glm(prolonged ~ amplitude_lat, family = binomial, data = ranidae_completo)
summary(modelo6)

modelo3 <- glm(prolonged ~ centroide_lat, family = binomial, data = ranidae_completo)
summary(modelo3)

modelo4 <- glm(prolonged ~ lat_max, family = binomial, data = ranidae_completo)
summary(modelo4)

modelo5 <- glm(prolonged ~ lat_min, family = binomial, data = ranidae_completo)
summary(modelo5)

boxplot(ranidae_completo$centroide_lat~ranidae_completo$breeding_pattern)
boxplot(ranidae_completo$amplitude_lat~ranidae_completo$breeding_pattern)
boxplot(ranidae_completo$lat_max~ranidae_completo$breeding_pattern)
boxplot(ranidae_completo$lat_min~ranidae_completo$breeding_pattern)

# Procedimento stepwise ---------------------------------------------------------------------------------------------

modelo_completo <- glm(prolonged ~ total_area_norm + lat_max +
                           amplitude_lat,  family = binomial, data = ranidae_completo)
library(PerformanceAnalytics)
library(jtools)
library(stargazer)


chart.Correlation((ranidae_completo[26:31]), histogram = TRUE, pch = "+")

summary(modelo_completo)
anova(modelo_completo)

step_ranidae <- step(modelo_completo, k = 3.841459)
summary(step_ranidae)

export_summs(step_ranidae, scale = F, digits = 5)

confint(step_ranidae, level = 0.95) # siginificância 5%
plot_summs(step_ranidae, colors = "#440154FF") #função 'plot_summs' do pacote 'ggstance'


plot_summs(step_ranidae, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_completo, step_ranidae, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

stargazer(modelo_latmax, nobs = T, type = "text")

modelo_latmax <- glm(prolonged ~ lat_max, family = "binomial", data = ranidae_completo)
summary(modelo_latmax)
export_summs(modelo_latmax, scale = F, digits = 5)
stargazer(modelo_latmax, nobs = T, type = "text")

logLik(modelo_latmax)
logLik(step_ranidae)


ranidae_completo %>% 
        ggplot() +
        geom_point(aes(x = lat_max, y = prolonged), color = "darkorange", size = 2.5) +
        geom_smooth(aes(x = lat_max, y = prolonged), 
                    method = "glm", formula = y ~ x, 
                    method.args = list(family = "binomial"), 
                    se = FALSE,
                    color = "darkgray", size = 1.5) +
        labs(x = "Latitude Máxima",
             y = "Reprodução Prolongada") +
        theme_classic()
