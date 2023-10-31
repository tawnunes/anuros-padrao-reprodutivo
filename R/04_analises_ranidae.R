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
# Script 04: Análises da relação entre padrão reprodutivo e latitude para a familia Ranidae

# PACOTES ------------------------------------------------------------------------------------------------------

library(tidyverse) # manipulação dos dados
library(ggplot2) # visualização gráfica
library(car) # para teste de colinearidade
library(DHARMa) # validação do modelo por análise dos resíduos

# IMPORTANTDO OS DADOS -----------------------------------------------------------------------------------------

ranidae <- read.csv("data/processed/ranidae.csv")

ranges_ranidae <- read.csv("data/processed/ranges_ranidae.csv")

ranges_ranidae <- ranges_ranidae %>% 
    rename(species = especie)

# UNINDO INFORMAÇÕES --------------------------------------------------------------------------------------------

ranidae_completo <- ranidae %>% 
    # unindo as informações
    left_join(ranges_ranidae, by = "species") %>%
    # definindo variáveis
    mutate(f.breeding_pattern = as.factor(tolower(breeding_pattern)),
           prolonged = ifelse(f.breeding_pattern == "prolonged", 1,0),
           # utilizando valores absolutos para refletir distância da região equatorial
           lat_max = lat_max,
           abs.lat_min = abs(lat_min))

# ESTATÍSTICA DESCRITIVA ------------------------------------------------------------------------------------------

ranidae_completo %>% filter(lat_min < 0) %>%
    select(species, lat_max, lat_min)

ranidae_completo %>% filter(lat_max < 0) %>%
    select(species, lat_max, lat_min)


ranidae_completo %>% group_by(breeding_pattern) %>% 
    summarise(n_species = n(),
              min_area = min(area_occ_km2),
              max_area = max(area_occ_km2),
              mediana = median(area_occ_km2))

# Latititude minima
ranidae_completo %>% group_by(breeding_pattern) %>% 
    summarise(n_species = n(),
              min = min(lat_min),
              max = max(lat_min),
              mediana = median(lat_min))

# Latititude minima
ranidae_completo %>% group_by(breeding_pattern) %>% 
    summarise(n_species = n(),
              min = min(lat_max),
              max = max(lat_max),
              mediana = median(lat_max))

# Centroide latitudinal
ranidae_completo %>% group_by(breeding_pattern) %>% 
    summarise(n_species = n(),
              min = min(centroide_lat),
              max = max(centroide_lat),
              mediana = median(centroide_lat))

# Amplitude latitudinal
ranidae_completo %>% group_by(breeding_pattern) %>% 
    summarise(n_species = n(),
              min = min(amplitude_lat),
              max = max(amplitude_lat),
              mediana = median(amplitude_lat))

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
    r <- abs(cor(x, y, method = "spearman"))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(ranidae_completo[,c(10:14,17)], upper.panel=panel.cor, diag.panel=panel.hist)



# ANÁLISES ------------------------------------------------------------------------------------
names(ranidae_completo)

modelo1 <- glm(f.breeding_pattern ~ lat_min + lat_max + amplitude_lat + lat_min*amplitude_lat + lat_max*amplitude_lat, 
               family = binomial, data = ranidae_completo)

summary(modelo1)
# nenhuma interação significativa

modelo2 <- glm(f.breeding_pattern ~ lat_min + lat_max + amplitude_lat + lat_max*amplitude_lat, 
               family = binomial, data = ranidae_completo)

summary(modelo2)
# interação não significativa


modelo3 <- glm(f.breeding_pattern ~ lat_min + lat_max + amplitude_lat, 
               family = binomial, data = ranidae_completo)

vif(modelo3)
# algum erro 


modelo4 <- glm(f.breeding_pattern ~ lat_max + amplitude_lat, 
               family = binomial, data = ranidae_completo)

vif(modelo4)


simulateResiduals(modelo4, plot =T, n = 1000)
# modelo validado

drop1(modelo4, test = "Chisq")

summary(modelo4)

modelo5 <- glm(f.breeding_pattern ~ lat_max, 
               family = binomial, data = ranidae_completo)

simulateResiduals(modelo5, plot =T, n = 1000)
# modelo validado

summary(modelo5)
