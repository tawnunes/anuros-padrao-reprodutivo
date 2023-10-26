#
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
# Script 03: extrair lat min e max do range de ocorrência da família Hylidae

# PACOTES ------------------------------------------------------------------

library(sf) # manipulação de dados espaciais 
library(tidyverse) # manipulação dos dados

# IMPORTANDO DADOS ---------------------------------------------------------

# List all files and subdirectories recursively
ranidae.shp <- list.files("data/raw/shp_ranidae",pattern = ".shp", recursive = TRUE, full.names = TRUE)

ranges_ranidae <- data.frame(especie = character(0),
                             lat_max = numeric(0),
                             lat_min = numeric(0),
                             area_occ = numeric(0),
                             amplitude_lat = numeric(0),
                             centroide_lat = numeric(0),
                             centroide_long = numeric(0))


# EXTRAINDO INFORMAçÔES do shp ---------------------------------------------
# Loop através dos shapefiles
for (shapefile in ranidae.shp) {
    # lendo o shapefile
    dados_shapefile <- st_read(shapefile)
    
    # Excluindo os polígonos que representam áreas onde a espécie está extinta
    dados_shapefile <- dados_shapefile %>%
        filter(!grepl("Extinct", LEGEND, ignore.case = TRUE))
    
    # unindo os poligonos restantes
    merged_geometry<- st_union(dados_shapefile$geometry)
    
    # mantendo os atributos
    atributos <- dados_shapefile[1,]
    dados_shapefile <- st_sf(atributos , geometry = merged_geometry)
    
    # Extraindo a latitude máxima, mínima e centroide caso o shp ainda tenha polígonos
    if (nrow(dados_shapefile) > 0) {
        # pegando o nome da espécie
        nome_especie <- dados_shapefile$SCI_NAME
        # unindo todos os polígonos dentro do shapefile
        dados_shapefile <- st_union(dados_shapefile)
        # extraindo lat max e min
        lat_max <- max(st_coordinates(dados_shapefile)[, "Y"])
        lat_min <- min(st_coordinates(dados_shapefile)[, "Y"])
        # extraindo centroides do range de ocorrência
        centroide_lat <- st_coordinates(st_centroid(dados_shapefile))[,"Y"]
        centroide_long <- st_coordinates(st_centroid(dados_shapefile))[,"X"]
        
        # Calculando a área do poligono
        area_occ <- st_area(dados_shapefile)
        
        # crindo um novo dataframe com os resultados
        novo_registro <- data.frame(especie = nome_especie,
                                    lat_max = lat_max,
                                    lat_min = lat_min,
                                    area_occ_km2 = as.numeric(area_occ)/ 1000000,
                                    amplitude_lat = lat_max - lat_min,
                                    centroide_lat = centroide_lat,
                                    centroide_long = centroide_long)
        
        # Combine o novo registro com o dataframe de resultados
        ranges_ranidae <- rbind(ranges_ranidae, novo_registro)
    }
}


# exportar dados como csv

write.csv(ranges_ranidae, "data/processed/ranges_ranidae.csv", row.names = F)
