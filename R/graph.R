

# library
library(tidyverse)
library(viridis)

# TODAS AS ESPÉCIES --------------------------------------------------------------------------------
anuros_graph <- anuros_reprod %>% 
    group_by(family, breeding_pattern) %>% 
    summarise(n_species = n()) %>% 
    arrange(breeding_pattern)


# Create dataset
data <- data.frame(
    individual=paste( "Mister ", seq(1,60), sep=""),
    group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
    value=sample( seq(10,100), 60, replace=T)
)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 1
to_add <- data.frame( matrix(NA, empty_bar*nlevels(anuros_graph$breeding_pattern), ncol(anuros_graph)) )
colnames(to_add) <- colnames(anuros_graph)
to_add$breeding_pattern <- rep(levels(anuros_graph$breeding_pattern), each=empty_bar)
data <- rbind(anuros_graph, to_add)
data <- data %>% arrange(breeding_pattern, n_species)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
    group_by(breeding_pattern) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=n_species, fill=breeding_pattern)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", alpha=0.5) +
    ylim(-20,50) +
    theme_minimal() +
    theme(
        legend.position = "top",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        #plot.margin = unit(rep(-1,4), "cm") 
    ) +
    scale_fill_manual(name = "Padrão Reprodutivo", labels =c("Explosivo", "Prolongado"),values = c("darkolivegreen","darkorange"))+
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=n_species+5, label=paste0("(",n_species,") ",family), hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE)#+

# Add base line information
#geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  #+
#    geom_text(data=base_data, aes(x = title, y = -18, label=breeding_pattern), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)
p



# HIPOTESE 1 -------------------------------------------------

anuros_reprod %>% 
    filter(total_area > Lower & total_area < Upper) %>% 
    ggplot(aes(x = total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Extensão Geográfica de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(name = "Padrão Reprodutivo", labels =c("Explosivo", "Prolongado"),values = c("darkolivegreen","darkorange"))+
    theme_classic()+
    theme(legend.position = "bottom")

quartiles <- quantile(anuros_reprod$total_area, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(anuros_reprod$total_area)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data, data$Sepal.Width > Lower & data$Sepal.Width < Upper)

dim(data_no_outlier)


anuros_reprod %>% 
    filter(total_area > Lower & total_area < Upper) %>% 
    ggplot(aes(y = total_area, x = breeding_pattern, fill = breeding_pattern)) +
    geom_boxplot()+
    labs(         x = "Extensão Geográfica de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(name = "Padrão Reprodutivo", labels =c("Explosivo", "Prolongado"),values = c("darkolivegreen","darkorange"))+
    theme_classic()+
    theme(legend.position = "none")

ranidae_completo %>% 
    ggplot(aes(x = total_area, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Extensão Geográfica de Ocorrência (km²)",
         y = "Densidade") +
    scale_fill_manual(name = "Padrão Reprodutivo", labels =c("Explosivo", "Prolongado"),values = c("darkolivegreen","darkorange"))+
    theme_classic()+
    theme(legend.position = "bottom")


ranidae_completo %>% 
    ggplot(aes(y = amplitude_lat, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Densidade",
         y = "Amplitude Latitudinal") +
    scale_fill_manual(name = "Padrão Reprodutivo", labels =c("Explosivo", "Prolongado"),values = c("darkolivegreen","darkorange"))+
    theme_classic()+
    theme(legend.position = "bottom")

ranidae_completo %>% 
    ggplot(aes(y = lat_max, fill = breeding_pattern)) +
    geom_density(alpha = 0.5)+
    #geom_histogram(binwidth = 100000, position = "identity", alpha = 0.5) +
    labs(title = "Sobreposição de Curvas de densidade",
         x = "Densidade",
         y = "Latitude Máxima") +
    scale_fill_manual(name = "Padrão Reprodutivo", labels =c("Explosivo", "Prolongado"),values = c("darkolivegreen","darkorange"))+
    theme_classic()+
    theme(legend.position = "bottom")
