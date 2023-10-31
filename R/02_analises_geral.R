
# Universidade Federal do Paraná
# 
# Ecologia Comportamental 2023
# 
# Prof. Dr. Lilian Tonelli Manica
# 
# Discentes: Jonathan, Ruth Oliveira, Tawane Nunes
# 
# Padrões Reprodutivos em Anuros: Uma Análise Latitude-dependente
# 
# Script 02: Análises gerais

# PACOTES ------------------------------------------------------------------------------------------------------

library(tidyverse) # manipulação dos dados
library(ggplot2) # visualização gráfica
library(lme4) # modelos mistos
library(DHARMa) # análise dos resíduos do modelo

# IMPORTANTDO OS DADOS -----------------------------------------------------------------------------------------

anuros <- read.csv("data/processed/dados_anuros_completo.csv")

# Número de espécies: 718
n_distinct(anuros$species)

# Número de famílias: 38
n_distinct(anuros$family)

# SELECIONANDO DADOS ----------------------------------------------------------------------------------------

anuros_reprod <- anuros %>% 
    # filtrando dados com espécies que tenham informação sobre padrões reprodutivos
    filter(!is.na(breeding_pattern)) %>% 
    # filtrando dados com informação da extenção geográfica de ocorrência
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

# ESTATÍSTICA DESCRITIVA ------------------------------------------------------

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

# Avaliando a distribuição da variável exlicativa
hist(anuros_reprod$total_area)
# considerando a grande dispersão dos dados da área de distribuição
# Pode ser necessário transformações para a realização da análise

# Avaliando distribuição da variável resposta
plot(anuros_reprod$breeding_pattern)
# proporção semelhante entre as duas categorias

# visualizando relação entre variável resposta e explicativa
plot(anuros_reprod$total_area, anuros_reprod$breeding_pattern)
# não é possível identificar um padrão claro

# GRÁFICO DAS FAMÍLIAS -------------------------------------------------------------------------------------------

# criando data frame com número de espécies por família
anuros_graph <- anuros_reprod %>% 
    group_by(family, breeding_pattern) %>% 
    summarise(n_species = n()) %>% 
    arrange(breeding_pattern)

# número de barras vazias entre as categorias de padrão reprodutivo
empty_bar <- 3
# adicionando em um dataframe vazio e preenchendo com as informações do dataframe original
to_add <- data.frame( matrix(NA, empty_bar*nlevels(anuros_graph$breeding_pattern), ncol(anuros_graph)) )
colnames(to_add) <- colnames(anuros_graph)
to_add$breeding_pattern <- rep(levels(anuros_graph$breeding_pattern), each=empty_bar)
# adicionando as barras vazias ao data frame com os dados
data <- rbind(anuros_graph, to_add)
data <- data %>% arrange(breeding_pattern, n_species)
data$id <- seq(1, nrow(data))

# Para adicionar o texto com o nome das famílias
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar # definindo angulo do texto
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# criando o gráfico e salvando em svg para colocar a legenda mais próxima do gráfico usando o inkscape
svg("figures/01_especies_familias.svg", width = 6.9, height = 4, pointsize = 8)

ggplot(data, aes(x=as.factor(id), y=n_species, fill=breeding_pattern)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", alpha=0.5) +
    ylim(-20,50) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 20, 0),
    ) +
    coord_polar() + 
    scale_fill_manual(name = "Padrão Reprodutivo:", 
                      labels =c("Explosivo", "Prolongado"),
                      values = c("darkolivegreen","darkorange"))+
     
    geom_text(data=label_data, aes(x=id, y=n_species+5, 
                                   label=paste0("(",n_species,") ",family), hjust=hjust), 
              color="black", size=2.5, angle= label_data$angle, inherit.aes = FALSE)

dev.off()


# TRANSFORMANDO OS DADOS DE ÁREA DO RANGE DE OCORRÊNICA ----------------------------------------------------------

# aplicando diferentes transformações
anuros_reprod <- anuros_reprod %>% 
    mutate(
        # aplicando uma transformação linear sem perder a proporção entre os valores
        # normalização min-max
        norm.total_area = ((total_area - min(total_area))/ (max(total_area) - min(total_area))),
        # extraindo a raiz quadrada   
        sqrt.total_area = sqrt(total_area),
        # padronizando
        s.total_area = as.numeric(scale(total_area)))

# reavaliando as relações
plot(anuros_reprod$norm.total_area, anuros_reprod$breeding_pattern)
plot(anuros_reprod$sqrt.total_area, anuros_reprod$breeding_pattern)
plot(anuros_reprod$s.total_area, anuros_reprod$breeding_pattern)
# aparentemente raiz quadrada é mais efetiva

# COMPARANDO AS DISTRIBUIÇÕES ------------------------------------------------------------------------------------------

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
    ggplot(aes(x = norm.total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²) (normalizada)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()

# area range raiz quadrada
anuros_reprod %>% 
    ggplot(aes(x = sqrt.total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Raiz quadrada da área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()


# ANÁLISES PARA O TESTE DA HIPÓTESE: Padrão reprodutivo ------------------------------------------------------

modelo1 <- glmer(breeding_pattern ~ sqrt.total_area + (1|family), family = binomial, data = anuros_reprod,
                 glmerControl(optimizer = "bobyqa",
                              optCtrl = list(maxfun = 100000)))
# mesmo aumentando o número de iterações e alterando o otimizador o modelo não converge

# utilizando a extensão geográfica padronizada
modelo2 <- glmer(breeding_pattern ~ s.total_area + (1|family), family = binomial, data = anuros_reprod)
# modelo convergiu

simulateResiduals(modelo2, plot=T, n = 1000)
# modelo válido

summary(modelo2)
# não significativo

# SELECIONANDO FAMÍLIA PARA REFINAMENTO DA ÁREA DE OCORRÊNICA ---------------------------------------------------------

## Hylidae -----------------------------------------------------

hylidae <- anuros_reprod %>% 
    filter(family == "Hylidae")


hylidae %>%  group_by(breeding_pattern) %>% 
    summarise(n = n(),
              mean_area = mean(total_area))

hylidae %>% 
    ggplot(aes(x = total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Área total do Range de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(values = c("Explosive" = "green", "Prolonged" = "red"))+
    theme_classic()

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

## Bufonidae ------------------------------------------------------

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
