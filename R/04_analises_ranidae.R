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
library(ggeffects)

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
           s.area_occ_km2 = as.numeric(scale(area_occ_km2)),
           norm.area_occ = ((area_occ_km2 - min(area_occ_km2))/ (max(area_occ_km2) - min(area_occ_km2))),
           sqrt.area_occ = sqrt(area_occ_km2),
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
names(ranidae_completo)

png("figures/suplementar_01_correlograma.png", 
    width = 177, height = 120, units = "mm", res = 300, pointsize = 8)
pairs(ranidae_completo[,c(10,21,13, 14, 18, 19,20, 17)], upper.panel=panel.cor, diag.panel=panel.hist)
dev.off()


# ANÁLISES ------------------------------------------------------------------------------------
names(ranidae_completo)

modelo1 <- glm(f.breeding_pattern ~ abs.lat_min + lat_max + norm.area_occ + abs.lat_min*norm.area_occ + lat_max*norm.area_occ, 
               family = binomial, data = ranidae_completo)
# modelo não convergiu

modelo1a <- glm(f.breeding_pattern ~ abs.lat_min + lat_max + norm.area_occ + lat_max*norm.area_occ, 
               family = binomial, data = ranidae_completo)
# modelo não convergiu

modelo1b <- glm(f.breeding_pattern ~ abs.lat_min + lat_max + norm.area_occ, 
                family = binomial, data = ranidae_completo)

vif(modelo1b)
# alta colinearidade entre lat max e área

modelo2 <- glm(f.breeding_pattern ~ abs.lat_min + lat_max, 
               family = binomial, data = ranidae_completo)

vif(modelo2)
# baixa colineadirade

png("figures/suplementar_02_modelo2.png", 
    width = 177, height = 120, units = "mm", res = 300, pointsize = 8)
simulateResiduals(modelo2, plot =T, n = 1000)
# modelo valido
dev.off()

drop1(modelo2, test = "Chisq")
# apenas lat max é importante


modelo3 <- glm(f.breeding_pattern ~ lat_max, 
               family = binomial, data = ranidae_completo)

png("figures/suplementar_03_modelo3.png", 
    width = 177, height = 120, units = "mm", res = 300, pointsize = 8)
simulateResiduals(modelo3, plot =T, n = 1000)
# modelo validado
dev.off()

summary(modelo3)

# GRÁFICO DO MODELO ----------------------------------------------------------------------------------------------------------

mod <- ggpredict(modelo3, terms = "lat_max")

pred.mod <- data.frame(x = mod$x, y=mod$predicted, cilow = mod$conf.low, ciup = mod$conf.high)

breaks <- c(0, 0.25, 0.5, 0.75, 1)
labels <- paste0(breaks*100, "%")

png("figures/02_modelo3.png", 
    width = 177, height = 120, units = "mm", res = 300, pointsize = 8)
ggplot()+
    geom_point(aes(lat_max, prolonged, color = breeding_pattern), data = ranidae_completo, size = 2.5)+
    geom_smooth(data = pred.mod, aes(x,y), method = "loess", 
                color = "gray10", linewidth = 0.5, linetype = 1, se = F)+
    geom_ribbon(data = pred.mod, aes(x, ymin =cilow, ymax = ciup), 
                fill = "gray20", alpha = 0.1)+
    theme_classic() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 9),
          axis.line = element_line(colour = "gray50"),
          plot.margin = margin(20,20,20,20))+
    scale_color_manual(name = "Padrão reprodutivo:",labels = c("Explosivo", "Prolongado"),values = c("darkolivegreen","darkorange")) +
    scale_y_continuous(breaks = breaks, labels = labels)+
    scale_x_continuous(breaks = c(10,20,30,40,50,60,70))+
    labs(y = "Probabilidade Predita",
         x= "Latitude Máxima da Distribuição Geográfica")
dev.off()
