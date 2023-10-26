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
library(car)
library(DHARMa)
library(lme4)
library(ggeffects)

# IMPORTANTDO OS DADOS -----------------------------------------------------------------------------------------

# dados das familias
ranidae <- read.csv("data/processed/ranidae.csv")

hylidae <- read.csv("data/processed/hylidae.csv")

ranidae_hylidae <- rbind(ranidae, hylidae)
# selecionando variáveis 
ranidae_hylidae <- ranidae_hylidae %>% 
    select(c(1:7,11,23:25))

# dados espaciais
ranges_ranidae <- read.csv("data/processed/ranges_ranidae.csv")

# alterando nome para posterior união
ranges_ranidae <- ranges_ranidae %>% 
    rename(species = especie)

ranges_hylidae <- read.csv("data/processed/ranges_hylidae.csv")
# alterando nome para posterior união
ranges_hylidae <- ranges_hylidae %>% 
    rename(species = especie)

ranges <- rbind(ranges_ranidae, ranges_hylidae)

# UNINDO INFOMAÇÕES ---------------------------------------------------------------------------------

ranidae_hylidae <- ranidae_hylidae %>% 
    left_join(ranges, by = "species") 


ranidae_hylidae <- ranidae_hylidae %>% 
    mutate(breeding_pattern = as.factor(tolower(breeding_pattern)),
           family = as.factor(family)) %>% 
    mutate(lat_max = abs(ranidae_hylidae$lat_max),
           lat_min = abs(ranidae_hylidae$lat_min),
           centroide_lat = abs(ranidae_hylidae$centroide_lat),
           sqrt.area = sqrt(area_occ_km2),
           explosive = as.numeric(ifelse(breeding_pattern == "explosive", 1, 0)))


ranidae_hylidae %>% group_by(breeding_pattern) %>% 
    summarise(n_species = n(),
              min_area = min(area_occ_km2, na.rm = T),
              max_area = max(area_occ_km2, na.rm = T),
              mediana = median(area_occ_km2, na.rm = T))

ranidae_hylidae %>% group_by(breeding_pattern, family) %>% 
    summarise(n_species = n(),
              min_lat = min(lat_min, na.rm = T),
              max_lat = max(lat_max, na.rm = T),
              mediana = mean(amplitude_lat, na.rm = T))

# selecionando dados para o modelo

ranidae_hylidae <- ranidae_hylidae %>%
    filter(!is.na(breeding_pattern)) %>% 
    filter(!is.na(area_occ_km2))

# defindindo referencia como prolongado

ranidae_hylidae$breeding_pattern <- relevel(ranidae_hylidae$breeding_pattern, ref= "prolonged")


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

pairs(ranidae_hylidae[,c(1,7, 12:17)], upper.panel=panel.cor, diag.panel=panel.hist)

# Alta correlação entre centroide_lat e lat_max; entre amplitude e área de ocorrencia
# alta correlação entre centroide e lat_min e com família
# Alta correlação entre amplitude latitudinal e área de ocorrência

# vamos selecionar entre as correlacionadas aquelas que tem maior correlação com avariável resposta

pairs(ranidae_hylidae[,c(7, 12, 14:16)], upper.panel=panel.cor, diag.panel=panel.hist)
# neste caso latitude máxima e área de ocorrência

# ANÁLISES Incluindo família ----------------------------------------------------------------------------------------
names(ranidae_hylidae)

modelo1 <- glmer(breeding_pattern ~ lat_max + sqrt.area + family +
                     lat_max*family + sqrt.area*family + (1|family), 
                 data = ranidae_hylidae, family = binomial)

#verificandoa a significancia das interações
summary(modelo1)
# interação entre area e família não significativa

modelo2 <- glmer(breeding_pattern ~ lat_max + sqrt.area + family +
                     lat_max*family + (1|family), 
                 data = ranidae_hylidae, family = binomial)

#verificandoa a significancia das interações
summary(modelo2)
# interação significativa

# vendo colinearidade
vif(modelo2)
# colinearidade causada pela interação

# validação do modelo

simulateResiduals(modelo2, plot = T, n= 1000)
# modelo válido

drop1(modelo2, test = "Chisq")
# apenas lat_max dependendo da família é importante para explicar 
# o padrão reprodutivo em anuros

summary(modelo2) # referencia prolongado

library(lsmeans)
compar<- lsmeans(modelo2,
                 pairwise ~ lat_max:family,
                 adjust = "tukey")
compar
cld(compar, Letters =T)


# GRAFICAMENTE ------------------------------------------------------------------------
mod.ggpred <- ggpredict(modelo2, terms =c("lat_max[all]", "family"))

# Armazenando em um dataframe
mod.pred <- data.frame(x = mod.ggpred$x, y = mod.ggpred$predicted, 
                       family = mod.ggpred$group, cilow = mod.ggpred$conf.low, ciup = mod.ggpred$conf.high)

# criando dois dataframes para cada família usando o pacote dplyr
mod.pred.h <- mod.pred %>% 
    filter( family == "Hylidae")
mod.pred.r <- mod.pred %>% 
    filter( family == "Ranidae")

# criando o plot
ggplot()+
    geom_point(aes(x = lat_max , y= explosive, color = family), data = ranidae_hylidae, shape = 19)+
    geom_smooth(data = mod.pred, aes(x,y, color = family), method = "loess", 
                linewidth = 0.8, se =F)+
    geom_ribbon(data = mod.pred.h, aes(x, ymin =cilow, ymax = ciup), 
                fill = "purple", alpha = 0.2)+
    geom_ribbon(data = mod.pred.r, aes(x, ymin =cilow, ymax = ciup), 
                fill = "green", alpha = 0.2)+
    theme_classic()+
    theme(legend.position = "bottom")+
    scale_color_manual(name="Família",labels = c("Hylidae", "Ranidae"), 
                       values = c("purple", "green"))+
    labs(y = "Probabilidade de Estratégia Reprodutiva Explosiva",http://127.0.0.1:10935/graphics/plot_zoom_png?width=1745&height=934
         x = "Valor Absoluto da Latitude Máxima")


# ANÁLISES sem família ----------------------------------------------------------------------------------------
names(ranidae_hylidae)

modelo3 <- glmer(breeding_pattern ~ lat_max + sqrt.area + lat_max*sqrt.area + (1|family), 
                 data = ranidae_hylidae, family = binomial)

#verificandoa a significancia das interações
summary(modelo3)
# interação  não significativa

modelo4 <- glmer(breeding_pattern ~ lat_max + sqrt.area + (1|family), 
                 data = ranidae_hylidae, family = binomial)

# vendo colinearidade
vif(modelo4)
# colinearidade causada pela interação

# validação do modelo

simulateResiduals(modelo4, plot = T, n= 1000)
# modelo válido
