
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
# Script 01: Reunindo e organizando as bases de dados


# PACOTES ------------------------------------------------------------------------------------------------------

library(tidyverse) # manipulação do dados
library(xml2) # manipulação de dados em xml
library(httr) # acessar http
library(stringr) # operações com strings (texto)
library(writexl) # exportar em formato do excel

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

## DADOS AMPHIBIAM WEB -------------------------------------------------------

# Dados provenientes do site amphibian web https://amphibiaweb.org/api/ws.html
# acessando os dados em xml
url <- "https://amphibiaweb.org/amphib_dump.xml"
response <- GET(url)

xml_content <- content(response, "text", encoding = "UTF-8")
xml <- read_xml(xml_content)

# extraindo as informação de cada espécie
species_elements <- xml %>%
    xml_find_all(".//species")

# Extrair os elementos filhos de cada 'species' como uma lista
species_data <- lapply(species_elements, function(species) {
    data <- species %>%
        xml_children() %>%
        xml_text()
    names(data) <- xml_name(xml_children(species))
    return(data)
})

# Converter a lista em um dataframe
anfibios_df <- as.data.frame(do.call(rbind, species_data))

# Criando uma coluna com o nome da espécie

anfibios_df <- anfibios_df %>% 
    mutate(species = paste(genus, specificepithet, sep = "")) %>% 
    mutate(species = str_replace_all(species, "\n", " "),# Substituir '\n' por espaço
           species = str_replace_all(species, "\\s+", " ")) %>% # Remover espaços em branco extras
    mutate(species = str_trim(species))


# JUNTANDO AS INFORMAÇÕES -------------------------------------------------------------------------------------

anuros_completo <- anuros %>% 
    # extraindo apenas o range total (km²) de ocorrência e idenficação se o range é menor que 25 km²
    left_join(globalspecies[,c(2,6,8:9)], by = "species", multiple = "first", keep = F) %>% 
    # extraindo mais informações informações do amphibian web
    left_join(anfibios_df[,c(13,18:24,26:27)], by = "species", keep = F)


# organizando planilha
anuros_completo <- anuros_completo %>% 
    mutate(Nelietal_refs = paste(reference1, reference2, reference3, reference4, sep = ", ")) %>% 
    rename(occ_countries = isocc,
           amphibianweb_refs = refs) %>% 
    select(c(1:9, 14:26)) 

# tirando caracteres de código html
anuros_completo <- anuros_completo %>% 
    mutate_at(vars(14:21), ~str_replace_all(., "<.*?>", ""))

# EXTRAINDO MAIS INFORMAÇÕES ----------------------------------------------------------------------

## HABITOS DE VIDA: DIURNO OU NOTURNO -------------------------------------------------------------
# habitos de vida (noturno)
anuros_habit_nocturnal <- anuros_completo %>% 
    select(species, life_history) %>% 
    filter(grepl("nocturnal", life_history, ignore.case = T))

# habitos de vida (diurno)
anuros_habit_diurnal <- anuros_completo %>% 
    select(species, life_history) %>% 
    filter(grepl("diurnal", life_history, ignore.case = T))

# ambos os termos no texto
anuros_habit_both <- anuros_habit_diurnal %>% 
    filter(species %in% anuros_habit_nocturnal$species)

# habitos de vida (diurno)
anuros_habit_diurnal <- anuros_habit_diurnal %>% 
    filter(!species %in% anuros_habit_both$species)

# adicionando a informação no datframe completo
anuros_completo <- anuros_completo %>% 
    mutate(habit = ifelse(species %in% anuros_habit_nocturnal$species, "nocturnal",
                          ifelse(species %in% anuros_habit_diurnal$species, "diurnal", NA)))

## CUIDADO PARENTAL -------------------------------------------------------------------------------------

anuros_parental <- anuros_completo %>% 
    select(species, life_history) %>% 
    filter(grepl("parental care", life_history, ignore.case = T))

anuros_guard <- anuros_completo %>% 
    select(species, life_history) %>% 
    filter(grepl("guards the eggs", life_history, ignore.case = T))
# adicionando a informação no datframe completo
anuros_completo <- anuros_completo %>% 
    mutate(parental_care = ifelse(species %in% anuros_parental$species | species %in% anuros_guard$species,
                                  1,0)) # zero pode ser NA

## TERRITORIALIDADE --------------------------------------------------------------------------------------

# listando os que já são classificados por Neli et al. 2014 como territorialistas
anuros_territ <- anuros_completo %>% 
    filter(evidences_territoriality != "NA")

# extraindo a informação dos dados do amphibian web
anuros_territ_aw <- anuros_completo %>% 
    select(species, life_history) %>% 
    filter(grepl("territorial", life_history, ignore.case = T))

# adicionando a informação no datframe completo
anuros_completo <- anuros_completo %>% 
    mutate(territorial = ifelse(species %in% anuros_territ$species | species %in% anuros_territ_aw$species, 1, 0))

# exportando como xlsx
write_xlsx(anuros_completo, "data/processed/dados_anuros_completo.xlsx")

# exportando como csv

write.csv(anuros_completo, "data/processed/dados_anuros_completo.csv", row.names = F)
