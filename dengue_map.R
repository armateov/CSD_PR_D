# Loading Data -----------------------------------------------------------

library(sf)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tools)
library(stringi)

setwd("D:/2024SummerIntern/DengueProcessing")

dengue_data <- read.csv("dengue_data.csv", stringsAsFactors = TRUE, fileEncoding = "latin1")
dengue_data$PROVINCIA <- toTitleCase(tolower(dengue_data$PROVINCIA))
#dengue_data$PROVINCIA <- gsub("ﾑ", "n", dengue_data$PROVINCIA)
dengue_data$PROVINCIA <- stri_trans_general(tolower(dengue_data$PROVINCIA), "Latin-ASCII")
dengue_data$PROVINCIA <- toTitleCase(dengue_data$PROVINCIA)

# dealing with ?? and empty spaces in Hora Reporte
dengue_data$Hora.Reporte <- gsub("\\?\\?", "AM", dengue_data$Hora.Reporte)

dengue_data$Hora.Reporte <- ifelse(grepl("AM|PM", dengue_data$Hora.Reporte),
                                   dengue_data$Hora.Reporte,
                                   paste(dengue_data$Hora.Reporte, "AM"))

# Combine report date and time into "datetime" and filtering NA values
datetime <- as.POSIXct(paste(dengue_data$Fecha.Reporte, dengue_data$Hora.Reporte), format="%d/%m/%Y %I:%M:%S %p", tz="EST")

dengue_data <- dengue_data %>% #adding datetime to dataframe
  mutate(datetime = datetime)

filtered_sorted_dengue_data <- dengue_data %>%
  filter(!is.na(datetime))


# Creating Dengue Maps ---------------------------------------------------------------------

# Aggregate data by province to merge with online shapefile
aggregated_data <- filtered_sorted_dengue_data %>%
  group_by(PROVINCIA) %>%
  summarize(total_confirmed_cases = sum(Casos.Ingresados.Confirmados, na.rm = TRUE))

#colnames(aggregated_data) # looking at column names of aggregated data

unzip("D:/2024SummerIntern/DengueProcessing/do_shp.zip", exdir = "D:/2024SummerIntern/DengueProcessing/do_shp")
shape_data <- st_read("D:/2024SummerIntern/DengueProcessing/do_shp/do.shp")

#colnames(shape_data)

#debugging mismatching province names
unique_provincias <- unique(dengue_data$PROVINCIA)
print(unique_provincias)

unique_provincias_shapefile <- unique(shape_data$name)  # Replace 'name' with the actual column name
print(unique_provincias_shapefile)

shape_data$name <- stri_trans_general(tolower(shape_data$name), "Latin-ASCII")
shape_data$name <- toTitleCase(shape_data$name)
shape_data$name <- gsub("Hermanas", "Hermanas Mirabal", shape_data$name)
shape_data$name <- gsub("La Estrelleta", "Elias Pina", shape_data$name)
shape_data$name <- gsub("El Seybo", "El Seibo", shape_data$name)

#can use casewhen to manually enter names
#Hermanas IS Hermanas Mirabal in csv file

# renaming the province column in the shp files from "name" to "PROVINCIA"
shape_data <- shape_data %>%
  rename(PROVINCIA = name)

# Merge shapefile data with aggregated dengue data
shape_data_merged <- shape_data %>%
  left_join(aggregated_data, by = "PROVINCIA")

# Check for NA values in the merged data
#shape_data_merged$total_confirmed_cases[is.na(shape_data_merged$total_confirmed_cases)] <- 0

# Plotting the severity map (2 colors)
ggplot() +
  geom_sf(data = shape_data_merged, aes(fill = total_confirmed_cases)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray", name = "Confirmed Cases") +
  labs(title = "Geographical Distribution of Confirmed Dengue Cases",
       fill = "Confirmed Cases",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

# Calculate the centroids of each province for labeling
centroids <- shape_data_merged %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(shape_data_merged)

# Split long province names into two lines
#centroids$PROVINCIA <- gsub(" ", "\n", centroids$PROVINCIA, fixed = TRUE)

#severity map (3 colors)
ggplot() +
  geom_sf(data = shape_data_merged, aes(fill = total_confirmed_cases)) +
  scale_fill_gradientn(colors = c("white", "yellow", "red"), na.value = "gray", name = "Confirmed Cases", limits = c(0, max(aggregated_data$total_confirmed_cases, na.rm = TRUE))) +
  geom_text(data = centroids, aes(x = X, y = Y, label = PROVINCIA), size = 2.5, color = "black", fontface = "bold") +
  labs(title = "Geographical Distribution of Confirmed Dengue Cases",
       fill = "Confirmed Cases",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.box.background = element_rect(color = "black", size = 0.5),
        legend.box.margin = margin(5, 5, 5, 5)) +
  guides(fill = guide_colorbar(title.position = "top",
                               barwidth = unit(1, "lines"),
                               barheight = unit(10, "lines"),
                               frame.colour = "black",
                               ticks.colour = "black",
                               ticks.linewidth = 0.5))

#, check_overlap = TRUE

# Now addressing map of all the centers

# Extract health center data
health_centers <- dengue_data %>%
  select(LAT, LONG, CENTRO) %>%
  filter(!is.na(LAT) & !is.na(LONG))

# Create the map showing only health centers
ggplot() +
  geom_sf(data = shape_data, fill = "white", color = "black") +
  geom_point(data = health_centers, aes(x = LONG, y = LAT), color = "blue", size = 2) +
  labs(title = "Locations of Health Centers",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "none")

