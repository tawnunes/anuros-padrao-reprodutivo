
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
# Script 01: Reunindo e organizando as bases de dados


# PACOTES ------------------------------------------------------------------------------------------------------

library(tidyverse) # manipulação do dados

# IMPORTANTDO OS DADOS -----------------------------------------------------------------------------------------

## DADOS NELI et al. 2014 ---------------------------------------

# dados de Neli et al 2014 https://doi.org/10.1086/678455
# acessados em: http://dx.doi.org/10.5061/dryad.270sf.
anuros <- read.csv2("data/raw/frogs_dataset_Nalietal2014.csv")

# excluindo linhas a mais que contém apenas NA
anuros <- anuros %>% 
    filter(rowSums(is.na(.)) != ncol(.))

## DADOS WORLD BANK ---------------------------------------------

# dados do World Bank https://datacatalog.worldbank.org/search/dataset/0063384/Global-Species-Database
# atualizado em Jan 9, 2023
globalspecies <- read.csv("data/raw/Species_Database_wb_datanam.csv")

#mudando nome da coluna para corresponder com anuros
globalspecies <- globalspecies %>%
    rename(species = binomial)

# JUNTANDO AS INFORMAÇÕES -------------------------------------------------------------------------------------

anuros_completo <- anuros %>% 
    # extraindo apenas a abrangência geográfica de ocorrência (km²) de ocorrência e idenficação se o range é menor que 25 km²
    left_join(globalspecies[,c(2,8:9)], by = "species", multiple = "first", keep = F)


# organizando planilha
anuros_completo <- anuros_completo %>% 
    #colocando as referências em uma coluna só
    mutate(Nelietal_refs = paste(reference1, reference2, reference3, reference4, sep = ", ")) %>% 
    # selecionando apenas as variáveis de interesse
    select(c(1,2,6,14,16))
    
# formatando a limpando as referências
anuros_completo <- anuros_completo %>% 
    mutate(Nelietal_refs = gsub("NA","", Nelietal_refs),
           Nelietal_refs = gsub("<e1>","a", Nelietal_refs))

# exportando como csv

write.csv(anuros_completo, "data/processed/dados_anuros_completo.csv", row.names = F)
