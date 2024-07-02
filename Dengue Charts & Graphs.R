#working with chat assistance 
# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)

setwd("C:\\Users\\dgfer\\OneDrive\\Documents\\R GIT DATA")


# Specify the path to the uploaded file
csv_file_path <- "C:\\Users\\dgfer\\OneDrive\\Documents\\R GIT DATA"

# Specify the locale with LATIN1 encoding
locale_setting <- locale(encoding = "LATIN1")

# Read the CSV file with the specified locale
dengue_data <- read_csv("dengue_data.csv", locale = locale_setting)

# Replace '??' with 'AM' in the Hora Reporte column
dengue_data <- dengue_data %>%
  mutate(`Hora Reporte` = gsub("\\?\\?", "AM", `Hora Reporte`))

# Add 'AM' to times missing 'AM' or 'PM'
dengue_data <- dengue_data %>%
  mutate(`Hora Reporte` = ifelse(grepl("AM|PM", `Hora Reporte`), `Hora Reporte`, paste(`Hora Reporte`, "AM")))

# Verify the changes
print(dengue_data$`Hora Reporte`)

dengue_data <- dengue_data %>%
  mutate(`Fecha Reporte` = as.Date(`Fecha Reporte`, format="%d/%m/%Y"))

# Scatter plot of cases based on latitude and longitude
ggplot(dengue_data, aes(x = LONG, y = LAT, color = `Casos Ingresados Confirmados`, size = `Casos Ingresados Confirmados`)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Geographical Distribution of Dengue Cases",
       x = "Longitude",
       y = "Latitude",
       color = "Confirmed Cases",
       size = "Confirmed Cases") +
  theme_minimal()

# Plot the availability of hospital beds over time
ggplot(dengue_data, aes(x = `Fecha Reporte`, y = `Camas Disponibles`, color = CENTRO)) +
  geom_line() +
  labs(title = "Availability of Hospital Beds Over Time",
       x = "Date",
       y = "Number of Available Beds",
       color = "Hospital Center") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot of confirmed cases by region
ggplot(dengue_data, aes(x = reorder(PROVINCIA, `Casos Ingresados Confirmados`, sum), y = `Casos Ingresados Confirmados`, fill = PROVINCIA)) +
  geom_bar(stat = "identity") +
  labs(title = "Confirmed Dengue Cases by Region",
       x = "Region",
       y = "Total Confirmed Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_viridis_d()
       
# Horizontal Bar Plot of Confirmed Cases by Region
ggplot(dengue_data, aes(x = reorder(PROVINCIA, `Casos Ingresados Confirmados`, sum), y = `Casos Ingresados Confirmados`, fill = PROVINCIA)) +
  geom_bar(stat = "identity") +
  labs(title = "Confirmed Dengue Cases by Region",
       x = "Total Confirmed Cases",
       y = "Region") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_fill_viridis_d() +
  coord_flip()

# Ensure latitude and longitude are numeric
dengue_data <- dengue_data %>%
  mutate(LAT = as.numeric(LAT), LONG = as.numeric(LONG))

#inspect for missing values
summary(dengue_data$LAT)
summary(dengue_data$LONG)

#remove rows with missing lat and long 

# Create a leaflet map AGAIN 
# Load necessary libraries
library(leaflet)
library(dplyr)
library(readr)
library(mapview)
library(webshot)

# Install PhantomJS if not already installed
webshot::install_phantomjs()

# Read the updated CSV file
csv_file_path <- "dengue_data.csv"  # Replace with your actual file path
dengue_data <- read_csv(csv_file_path, locale = locale(encoding = "LATIN1"))

# Print the first few rows to verify data
print(head(dengue_data))

# Ensure latitude and longitude are numeric and filter out invalid coordinates
dengue_data <- dengue_data %>%
  mutate(LAT = as.numeric(LAT), LONG = as.numeric(LONG)) %>%
  filter(!is.na(LAT) & !is.na(LONG))

# Verify the filtered data
print(summary(dengue_data$LAT))
print(summary(dengue_data$LONG))

# Create a leaflet map
map <- leaflet(data = dengue_data) %>%
  addTiles() %>%
  setView(lng = -70.1627, lat = 18.7357, zoom = 8) %>%
  addCircleMarkers(
    ~LONG, ~LAT,
    radius = 5,
    color = ~ifelse(`Casos Ingresados Confirmados` > 0, 'red', 'blue'),
    fill = TRUE,
    fillOpacity = 0.7,
    popup = ~paste("Centro:", CENTRO, "<br>",
                   "Provincia:", PROVINCIA, "<br>",
                   "Confirmed Cases:", `Casos Ingresados Confirmados`, "<br>",
                   "Available Beds:", `Camas Disponibles`)
  ) %>%
  addLegend(
    "bottomright",
    colors = c("blue", "red"),
    labels = c("No Confirmed Cases", "Confirmed Cases"),
    title = "Dengue Cases"
  )

# Print the map to display it in the viewer
print(map)

# Render the map in an external browser
mapshot(map, file = "map.html")
browseURL("map.html")

### this does not render a map, and idk why????


#graphing confirmed cases per month by region 
# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# Read the updated CSV file
csv_file_path <- "dengue_data.csv"  
dengue_data <- read_csv(csv_file_path, locale = locale(encoding = "LATIN1"))

# Ensure 'Fecha Reporte' is in Date format
dengue_data <- dengue_data %>%
  mutate(`Fecha Reporte` = as.Date(`Fecha Reporte`, format="%d/%m/%Y"))

# Filter data to include only up to June 2024
dengue_data <- dengue_data %>%
  filter(`Fecha Reporte` <= as.Date("2024-06-30"))

# Aggregate confirmed cases per month by region
agg_data <- dengue_data %>%
  mutate(Month = floor_date(`Fecha Reporte`, "month")) %>%
  group_by(Month, PROVINCIA) %>%
  summarize(Confirmed_Cases = sum(`Casos Ingresados Confirmados`, na.rm = TRUE)) %>%
  ungroup()

# Create a bar plot of confirmed cases per month by region
ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, fill = PROVINCIA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       fill = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Alternatively, create a line chart of confirmed cases per month by region
ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, color = PROVINCIA, group = PROVINCIA)) +
  geom_line() +
  geom_point() +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#improved visual 
# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# Read the updated CSV file
csv_file_path <- "dengue_data.csv"  # Path to the uploaded file
dengue_data <- read_csv(csv_file_path, locale = locale(encoding = "LATIN1"))

# Ensure 'Fecha Reporte' is in Date format
dengue_data <- dengue_data %>%
  mutate(`Fecha Reporte` = as.Date(`Fecha Reporte`, format="%d/%m/%Y"))

# Filter data to include only up to June 2024
dengue_data <- dengue_data %>%
  filter(`Fecha Reporte` <= as.Date("2024-06-30"))

# Aggregate confirmed cases per month by region
agg_data <- dengue_data %>%
  mutate(Month = floor_date(`Fecha Reporte`, "month")) %>%
  group_by(Month, PROVINCIA) %>%
  summarize(Confirmed_Cases = sum(`Casos Ingresados Confirmados`, na.rm = TRUE)) %>%
  ungroup()

# Improved bar plot of confirmed cases per month by region with facets
ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, fill = PROVINCIA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       fill = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired") +  # Use a distinct color palette
  facet_wrap(~ PROVINCIA, scales = "free_y")  # Facet by region

# Alternatively, create an improved line chart of confirmed cases per month by region with facets
ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, color = PROVINCIA, group = PROVINCIA)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Paired") +  # Use a distinct color palette
  facet_wrap(~ PROVINCIA, scales = "free_y")  # Facet by region

#making a cleaner graph with viridis 
# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(viridis)

# Read the updated CSV file
csv_file_path <- "dengue_data.csv"  # Path to the uploaded file
dengue_data <- read_csv(csv_file_path, locale = locale(encoding = "LATIN1"))

# Ensure 'Fecha Reporte' is in Date format
dengue_data <- dengue_data %>%
  mutate(`Fecha Reporte` = as.Date(`Fecha Reporte`, format="%d/%m/%Y"))

# Filter data to include only up to June 2024
dengue_data <- dengue_data %>%
  filter(`Fecha Reporte` <= as.Date("2024-06-30"))

# Aggregate confirmed cases per month by region
agg_data <- dengue_data %>%
  mutate(Month = floor_date(`Fecha Reporte`, "month")) %>%
  group_by(Month, PROVINCIA) %>%
  summarize(Confirmed_Cases = sum(`Casos Ingresados Confirmados`, na.rm = TRUE)) %>%
  ungroup()

# Improved bar plot of confirmed cases per month by region with facets
ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, fill = PROVINCIA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       fill = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d() +  # Use the viridis color palette from ggplot2
  facet_wrap(~ PROVINCIA, scales = "free_y")  # Facet by region

# Alternatively, create an improved line chart of confirmed cases per month by region with facets
ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, color = PROVINCIA, group = PROVINCIA)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d() +  # Use the viridis color palette from ggplot2
  facet_wrap(~ PROVINCIA, scales = "free_y")  # Facet by region

# Improved bar plot of confirmed cases per month by region with facets
bar_plot <- ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, fill = PROVINCIA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       fill = "Region") +
  theme_minimal(base_size = 15) +  # Increase base font size for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Move legend to the bottom
        legend.text = element_text(size = 10),  # Smaller legend text
        legend.title = element_text(size = 12)) +
  scale_fill_viridis_d() +  # Use the viridis color palette from ggplot2
  facet_wrap(~ PROVINCIA, scales = "free_y")  # Facet by region

# Save the plot with increased dimensions
ggsave("bar_plot.png", plot = bar_plot, width = 16, height = 12)
View(bar_plot)

# Save the plot with increased dimensions
ggsave("bar_plot.png", plot = bar_plot, width = 16, height = 12)

# Improved line chart of confirmed cases per month by region with facets
line_chart <- ggplot(agg_data, aes(x = Month, y = Confirmed_Cases, color = PROVINCIA, group = PROVINCIA)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Confirmed Dengue Cases per Month by Region",
       x = "Month",
       y = "Confirmed Cases",
       color = "Region") +
  theme_minimal(base_size = 15) +  # Increase base font size for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Move legend to the bottom
        legend.text = element_text(size = 10),  # Smaller legend text
        legend.title = element_text(size = 12)) +
  scale_color_viridis_d() +  # Use the viridis color palette from ggplot2
  facet_wrap(~ PROVINCIA, scales = "free_y")  # Facet by region

# Save the plot with increased dimensions
ggsave("line_chart.png", plot = line_chart, width = 16, height = 12)
