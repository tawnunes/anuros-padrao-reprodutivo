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
    mutate(breeding_pattern = as.factor(tolower(breeding_pattern)),
           prolonged = ifelse(breeding_pattern == "prolonged", 1,0),
           s.total_area = as.numeric(scale(total_area))) %>% 
    mutate(lat_max = abs(lat_max),
           lat_min = abs(lat_min)) %>% 
    select(species, breeding_pattern, total_area, s.total_area,lat_min,lat_max,amplitude_lat,centroide_lat)


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

# VERIFICANDO COLINEARIDADE -------------------------------------------------

# Verificando colinearidade (graficamente)
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(ranidae_completo[,2:8], upper.panel=panel.cor, diag.panel=panel.hist)



# ANÁLISES ------------------------------------------------------------------------------------
names(ranidae_completo)

modelo1 <- glm(breeding_pattern ~ s.total_area + lat_min + lat_max + amplitude_lat + centroide_lat, 
               family = binomial, data = ranidae_completo)
library(car)
vif(modelo1)


library(PerformanceAnalytics)
chart.Correlation((ranidae_completo[4:8]), histogram = TRUE, pch = "+")

modelo2 <- glm(breeding_pattern ~ s.total_area + lat_min + amplitude_lat + centroide_lat, 
               family = binomial, data = ranidae_completo)
vif(modelo2)

modelo3 <- glm(breeding_pattern ~ s.total_area + amplitude_lat + centroide_lat, 
               family = binomial, data = ranidae_completo)
vif(modelo3)
simulateResiduals(modelo3, plot =T, n = 1000)

modelo4 <- glm(breeding_pattern ~ s.total_area + centroide_lat, 
               family = binomial, data = ranidae_completo)
vif(modelo4)
library(DHARMa)
simulateResiduals(modelo4, plot =T, n = 1000)

anova(modelo3, modelo5, test = "Chisq")

modelo5 <- glm(breeding_pattern ~ amplitude_lat + centroide_lat, 
               family = binomial, data = ranidae_completo)
vif(modelo5)

simulateResiduals(modelo5, plot =T, n = 1000)
AIC(modelo4, modelo5)

summary(modelo5)

drop1(modelo5, test = "Chisq")


modelo6 <- glm(breeding_pattern ~ amplitude_lat + lat_max, 
               family = binomial, data = ranidae_completo)
vif(modelo6)

simulateResiduals(modelo6, plot =T, n = 1000)
AIC(modelo5, modelo6)

summary(modelo6)

drop1(modelo6, test = "Chisq")


modelo7 <- glm(breeding_pattern ~ s.total_area + centroide_lat, 
               family = binomial, data = ranidae_completo)
vif(modelo7)

simulateResiduals(modelo7, plot =T, n = 1000)
AIC(modelo6, modelo7)

summary(modelo7)

drop1(modelo7, test = "Chisq")



