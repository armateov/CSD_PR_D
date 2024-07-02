View(dengue)
library(ggplot2)
library(ggmap)
library(tmap)
library(tools)
library(tidyverse)
library(lubridate)
library(sf)

# Ensure you have your API key for Google Maps
# register_google(key = "your_api_key")

setwd("C:\\Users\\dgfer\\OneDrive\\Documents\\R GIT DATA")

#dealing with ?? and empty spaces 
dengue_data$PROVINCIA <- gsub("\\?\\?", "AM", dengue_data$Hora.Reporte)
dengue_data$Hora.Reporte <- ifelse(grepl("AM|PM", dengue_data$Hora.Reporte), dengue_data$Hora.Reporte, paste(dengue_data$Hora.Reporte, "AM"))

#getting rid of accents in shapedata SHP ó, á, í, é
shape_data <- shape_data %>% mutate(PROVINCIA = case_when(PROVINCIA == "Dajabón"~"Dajabon", PROVINCIA == "Samaná"~"Samana", PROVINCIA == "María Trinidad Sánchez
"~"Maria Trinidad Sanchez", PROVINCIA == "San Cristóbal"~"San Cristobal", PROVINCIA == "San Pedro de Macorís"~"San Pedro de Macoris", PROVINCIA == "Santiago Rodríguez"~"Santiago Rodriguez", PROVINCIA == "Monseñor Nouel" ~ "Monsenor Nouel", PROVINCIA == "Sánchez Ramírez"~"Sanchez Ramirez", PROVINCIA == "San José de Ocoa"~"San Jose de Ocoa"))

#Confirmed Cases Graph 
dengue_data <- read.csv("dengue_data.csv", stringsAsFactors = TRUE, fileEncoding = "latin1")
dengue_data$PROVINCIA <-  toTitleCase(tolower(dengue_data$PROVINCIA))

#creating map 
aggregated_data <- dengue_data %>% group_by((PROVINCIA)) %>% summarize(total_confirmed_cased = sum(Casos.Ingresados.Confirmados, na.rm = TRUE))

unzip("C:\\Users\\dgfer\\OneDrive\\Documents\\R GIT DATA\\do_shp.zip", exdir = "C:\\Users\\dgfer\\OneDrive\\Documents\\R GIT DATA\\do_shp")
shape_data <- st_read("C:\\Users\\dgfer\\OneDrive\\Documents\\R GIT DATA\\do_shp\\do.shp")

#renaming to PROVINCIA 
shape_data <- shape_data %>% rename(PROVINCIA = name)
colnames(shape_data)

# Remove parentheses from column names
names(aggregated_data) <- gsub("\\(|\\)", "", names(aggregated_data))
colnames(aggregated_data)
# View the updated data frame

#merge shapefile 
shape_data_merged <- shape_data %>% left_join(aggregated_data, by = "PROVINCIA")

#severity map 
ggplot() + geom_sf(data =  shape_data_merged, aes(fill = total_confirmed_cased)) + scale_fill_gradientn(colors = c("white", "yellow", "red"), na.value = "gray", name = "Confirmed Cases") + labs(title = "Geographical Distribution of Confirmed Dengue Cases", fill =  "Confirmed Cases", x = "Longitude", y = "Latitutde") + theme_minimal() + theme(legend.position = "right")

