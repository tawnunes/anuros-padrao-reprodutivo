
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
# Script 02: Análises gerais

# PACOTES ------------------------------------------------------------------------------------------------------

library(tidyverse) # manipulação dos dados
library(ggplot2) # visualização gráfica
library(lme4) # modelos mistos

# IMPORTANTDO OS DADOS -----------------------------------------------------------------------------------------

anuros <- read.csv("data/processed/dados_anuros_completo.csv")

# SELECIONANDO DADOS ----------------------------------------------------------------------------------------

anuros_reprod <- anuros %>% 
    # filtrando dados com espécies que tenham informação sobre padrões reprodutivos
    filter(!is.na(breeding_pattern)) %>% 
    # filtrando dados com informação do range de ocorrência
    filter(!is.na(total_area)) %>% 
    mutate(breeding_pattern = factor(breeding_pattern))

# listando número de espécies por família
n_familia <- anuros_reprod %>% 
    group_by(family) %>% 
    summarise(n =  n())

# filtrando famílias com três ou mais espécies
n_familia <- n_familia %>% 
    filter(n >=3)

# filtrando os dados para famílias que contém três ou mais espécies
anuros_reprod <- anuros_reprod %>% 
    filter(family %in% n_familia$family)

# Número de espécies: 180
n_distinct(anuros_reprod$species)

# Número de famílias: 14
n_distinct(anuros_reprod$family)

# Estatística descritiva
anuros_reprod %>% 
    group_by(breeding_pattern) %>% 
    summarise(n_sp = n(),
              média_area = mean(total_area),
              mediana_area = median(total_area),
              min_area = min(total_area),
              max_area = max(total_area),
              desviop_area = sd(total_area))

# NORMALIZANDO OS DADOS DE ÁREA DO RANGE DE OCORRÊNICA ----------------------------------------------------------

# aplicando uma transformação linear sem perder a proporção entre os valores
# normalização min-max
anuros_reprod <- anuros_reprod %>% 
    mutate(total_area_norm = ((total_area - min(total_area))/ (max(total_area) - min(total_area))),
           sqrt.total_area = sqrt(total_area),
           s.total_area = scale(total_area))

# VISUALIZAÇÃO GRÁFICA ------------------------------------------------------------------------------------------

# area range original
anuros_reprod %>% 
    ggplot(aes(x = total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()


# area range normalizada
anuros_reprod %>% 
    ggplot(aes(x = total_area_norm, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()

# area range normalizada
anuros_reprod %>% 
    ggplot(aes(x = sqrt.total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()


# ANÁLISES PARA O TESTE DA HIPÓTESE: Padrão reprodutivo ------------------------------------------------------

modelo_nulo <- glmer(breeding_pattern ~ 1 +(1|family), family = binomial, data = anuros_reprod)
summary(modelo_nulo)
logLik(modelo_nulo)

modelo1 <- glmer(breeding_pattern ~ s.total_area + (1|family), family = binomial, data = anuros_reprod)
library(DHARMa)
simulateResiduals(modelo1, plot=T, n = 1000)
summary(modelo1)

export_summs(modelo1, scale = F, digits = 5)
confint(modelo1, level = 0.95)
stargazer(modelo1, nobs = T, type = "text")

modelo2 <- glmer(breeding_pattern ~ small_range +(1|family), family = binomial, data = anuros_reprod)
summary(modelo2)



# Hipotese reprodução e habito de vida --------------------------------------------------------------------------------

anuros_habit <- anuros %>% 
    # filtrando dados com espécies que tenham informação sobre padrões reprodutivos
    filter(!is.na(breeding_pattern)) %>% 
    # filtrando dados com informação do range de ocorrência
    filter(!is.na(habit)) %>% 
    mutate(breeding_pattern = factor(breeding_pattern))

# listando número de espécies por família
n_familia <- anuros_habit %>% 
    group_by(family) %>% 
    summarise(n =  n())

# filtrando famílias com três ou mais espécies
n_familia <- n_familia %>% 
    filter(n >=3)

# filtrando os dados para famílias que contém três ou mais espécies
anuros_habit<- anuros_habit %>% 
    filter(family %in% n_familia$family) %>% 
    filter(family == "Hylidae")

# Número de espécies: 22
n_distinct(anuros_habit$species)

# Número de famílias: 2
n_distinct(anuros_habit$family)





# Análisar fecundidade e tamanho da área de ocorrência --------------------------------------------------------

anuros_fert <- anuros %>% 
    filter(!is.na(fecundity_measure)) %>% 
    filter(fecundity_unit == "eggs per clutch")

n_per_family <- anuros_fert %>% 
            group_by(family) %>% 
            summarise(N = n())

anuros_fert %>% 
    ggplot(aes(x = fecundity_measure)) +
    geom_density(alpha = 0.5)+
    theme_classic()

# Análises -----
fert_modelo_nulo <- glmer(fecundity_measure ~ 1 +(1|family), family = Gamma(link = "log"), data = anuros_fert)
summary(fert_modelo_nulo)

fert_modelo1 <- glmer(fecundity_measure ~ scale(total_area) + (1|family), family = Gamma(link = "log"), data = anuros_fert)
summary(fert_modelo1)

fert_modelo2 <- glmer(fecundity_measure ~ scale(total_area)+ breeding_pattern + (1|family), family = Gamma(link = "log"), data = anuros_fert)
summary(fert_modelo2)

fert_modelo3 <- glmer(fecundity_measure ~ scale(total_area)+ breeding_pattern + scale(total_area)*breeding_pattern + (1|family), family = Gamma(link = "log"), data = anuros_fert)
summary(fert_modelo3)

fert_modelo4 <- glmer(fecundity_measure ~ breeding_pattern + (1|family), family = Gamma(link = "log"), data = anuros_fert)
summary(fert_modelo4)

ggplot(anuros_fert, aes(x = scale(total_area), y = fecundity_measure, fill = family)) +
    geom_point() +  # Adiciona os pontos de dispersão
    geom_smooth(method = "glm", se = TRUE, color = "gray") +  # Adiciona a linha de regressão
    labs(title = "Relação entre Total Area e Fecundity Measure",
         x = "Total Area (Padronizado)",
         y = "Fecundity Measure") +
    theme_minimal()


fert_modelo2 <-  glmer(fecundity_measure ~ small_range +(1|family), family = Gamma(link = "log"), data = anuros_fert)
summary(fert_modelo2)




# SELECIONANDO FAMÍLIA PARA REFINAMENTO DA ÁREA DE OCORRÊNICA ---------------------------------------------------------

## Hylidae -----------------------------------------------------

hylidae <- anuros_reprod %>% 
    filter(family == "Hylidae")


hylidae %>%  group_by(breeding_pattern) %>% 
    summarise(n = n(),
              mean_area = mean(total_area))

write.csv2(hylidae, "data/hylidae.csv", row.names = F)

hylidae %>% 
    ggplot(aes(x = total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()

write.csv(hylidae, "data/processed/hylidae.csv", row.names = F)

## Ranidae SELECIONADA ------------------------------------------------------

ranidae <- anuros_reprod %>% 
    filter(family == "Ranidae")

ranidae %>%  group_by(breeding_pattern) %>% 
    summarise(n = n(),
              mean_area = mean(total_area))

ranidae %>% 
    ggplot(aes(x = total_area_norm, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()

write.csv(ranidae, "data/processed/ranidae.csv", row.names = F)

## Leptodactylidae ------------------------------------------------------

Leptodactylidae <- anuros_reprod %>% 
    filter(family == "Leptodactylidae")

Leptodactylidae %>%  group_by(breeding_pattern) %>% 
    summarise(n = n(),
              mean_area = mean(total_area))

Leptodactylidae %>% 
    ggplot(aes(x = total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()

##  Bufonidae ------------------------------------------------------

Bufonidae <- anuros_reprod %>% 
    filter(family == "Bufonidae")

Bufonidae %>%  group_by(breeding_pattern) %>% 
    summarise(n = n(),
              mean_area = mean(total_area))

Bufonidae %>% 
    ggplot(aes(x = total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()
