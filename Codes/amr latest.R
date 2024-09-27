url <- "https://apps.who.int/gho/data/node.main.175?lang=en"
data <- read.csv(url)
head(data)
View(data)
data <- read.csv("\Users\Covenant University\Downloads/data(1).csv")
View("data.csv")
read.csv(data)
install.packages("dplyr")
install.packages("ggplot2")
# Load libraries

library(dplyr)
library(ggplot2)

data_1_df <- data_1_ %>%
  mutate(Year_range = case_when(
    Year >= 1940 & Year <= 1949 ~ "1940-1949",
    Year >= 1950 & Year <= 1959 ~ "1950-1959",
    Year >= 1960 & Year <= 1969 ~ "1960-1969",
    Year >= 1970 & Year <= 1979 ~ "1970-1979",
    Year >= 1980 & Year <= 1989 ~ "1980-1989",
    Year >= 1990 & Year <= 1999 ~ "1990-1999",
    Year >= 2000 & Year <= 2009 ~ "2000-2009",
    Year >= 2010 & Year <= 2019 ~ "2010-2019",
    Year >= 2020 & Year <= 2029 ~ "2020-2029",
    TRUE ~ "Other"  # For any years outside the specified ranges
  ))

colnames(data_1_df) <- c("Country", "Year", "No_of_cases", "Year_range")
# Count total cases in each year range
Year_counts <- data_1_df %>%
  group_by(Year_range) %>%
  summarise(total = sum(No_of_cases, na.rm = TRUE)) %>%
  arrange(Year_range)  # Order by year range
View(Year_counts)


data_2_df <- data_2_ %>%
  mutate(Year_range = case_when(
    Year >= 1940 & Year <= 1949 ~ "1940-1949",
    Year >= 1950 & Year <= 1959 ~ "1950-1959",
    Year >= 1960 & Year <= 1969 ~ "1960-1969",
    Year >= 1970 & Year <= 1979 ~ "1970-1979",
    Year >= 1980 & Year <= 1989 ~ "1980-1989",
    Year >= 1990 & Year <= 1999 ~ "1990-1999",
    Year >= 2000 & Year <= 2009 ~ "2000-2009",
    Year >= 2010 & Year <= 2019 ~ "2010-2019",
    Year >= 2020 & Year <= 2029 ~ "2020-2029",
    TRUE ~ "Other"  # For any years outside the specified ranges
  ))

colnames(data_2_df) <- c("Country", "Year", "No_of_death", "Year_range")
# Count total death cases in each year range
Year_counts_2 <- data_2_df %>%
  group_by(Year_range) %>%
  summarise(total = sum(as.numeric(No_of_death), na.rm = TRUE)) %>%
  arrange(Year_range)  # Order by year range
View(Year_counts_2)


# Merge the tables with full join
combined_table <- Year_counts %>%
  full_join(Year_counts_2, by = "Year_range")
colnames(combined_table) <- c("Year_range", "No_of_cases", "No_of_death")
View(combined_table)

# Install tidyr if you haven't already
install.packages("tidyr")

# Load the tidyr library
library(tidyr)

# Reshape the data from wide to long format
Cholera_Table <- combined_table %>%
  pivot_longer(cols = c(No_of_cases, No_of_death), 
               names_to = "Metric", 
               values_to = "Value")
View(Cholera_Table)

# Create the side-by-side bar chart
ggplot(Cholera_Table, aes(x = Year_range, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "WHO Cholera outbreak data report between 1940-2019",
       x = "Year_range",
       y = "Count") +
  scale_fill_manual(values = c("No_of_death" = "red", 
                               "No_of_cases" = "blue")) +
  theme_minimal()



#Drawing map representation of the fatality rate data
# Install packages if not already installed
install.packages(c("maps", "mapdata"))
library(mapdata)
library(maps)

# rename columns
colnames(data_3) <- c("Country", "Year", "Fatality_rate")
View(data_3)
# Select only the columns you want
new_data_3_table <- data_3 %>%
  select("Country", "Fatality_rate")  # Specify the columns to keep

# Get the map data for the world
Cholera_Fatality_rate_distribution <- map_data("world")

# Remove duplicates in data_3 based on Country
data_3 <- data_3 %>%
  distinct(Country, .keep_all = TRUE)  # Keep the first occurrence
# Merge your data with the world map data
map_data <- Cholera_Fatality_rate_distribution %>%
  left_join(data_3, by = c("region" = "Country"))  # Adjust 'region' as needed


# Check the structure of your map_data
str(map_data)
# Convert Fatality_rate to numeric
map_data$Fatality_rate <- as.numeric(map_data$Fatality_rate)

# Check for any warnings during conversion
if (any(is.na(map_data$Fatality_rate))) {
  warning("There are NAs in Fatality_rate after conversion. Please check your data.")
}
# Remove rows with NA in Fatality_rate
map_data <- map_data %>% filter(!is.na(Fatality_rate))

#Create the map plot
ggplot(data = map_data, aes(x = long, y = lat, group = group, fill = Fatality_rate)) +
  geom_polygon(color = NA) +  # Borders between countries
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "World Map Representation of Cholera Fatality Rate (1940-2019)",
       fill = "Fatality Rate") +
  theme_minimal() +
  coord_fixed(1.3)  # Fix aspect ratio


# 3. create a time series plot for cholera outbreak over the period of years
# rename columns
colnames(data_1_) <- c("Country", "Year", "No_of_cases")
View(data_1_)
# Select only the columns you want
new_data_1_table <- data_1_ %>%
  select("Year", "No_of_cases")  # Specify the columns to keep
# Create a time series plot using ggplot
ggplot(data = new_data_1_table, aes(x = Year, y = No_of_cases)) +
  geom_line(color = "blue", size = 1) +      # Line to represent the cases over time
  geom_point(color = "red", size = 2) +      # Points for each year to emphasize individual data points
  labs(title = "Cholera Outbreak Cases Over Time", 
       x = "Year", 
       y = "Number of Cases") +              # Title and labels
  theme_minimal()                            # A clean, minimal theme


# 4. plot a geographic heat map to represent the data 
install.packages("rnaturalearth")
install.packages("sf")   # For handling spatial data
install.packages("rnaturalearthdata")

library(rnaturalearth)
library(sf)
library(rnaturalearthdata)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join cholera data with world map data
world_cholera <- world %>%
  left_join(data_3, by = c("name" = "Country"))
# Convert Fatality_rate to numeric
world_cholera$Fatality_rate <- as.numeric(world_cholera$Fatality_rate)

# Plot the map with cholera fatality rates
ggplot(data = world_cholera) +
  geom_sf(aes(fill = Fatality_rate), color = NA) +  # White borders between countries
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "Global Cholera Fatality Rate",
       fill = "Fatality Rate (%)") +
  theme_minimal() +
  theme(axis.text = element_blank(),  # Remove axis labels
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +  # Clean theme
  coord_sf()  # For proper geographic projection


# Plot an epidemiological curve

ggplot(data = data_1_, aes(x = Year, y = No_of_cases)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +  # Bar plot for cases
  geom_line(aes(group = 1), color = "red", size = 1) +           # Red line for trend
  geom_point(color = "red", size = 3) +                          # Points on the trend line
  labs(title = "Epidemiological Curve of Cholera Outbreak", 
       x = "Year", 
       y = "Number of Cases") +
  theme_minimal()  # Clean theme


colnames(data_2_) <- c("Country", "Year", "No_of_death")



# create shiny app for all the plots
# Install necessary packages
install.packages("shiny")
install.packages("leaflet")
install.packages("rmarkdown") # for generating reports

# Load libraries
library(shiny)
library(leaflet)
library(rmarkdown)


# User interface
ui <- fluidPage(
  titlePanel("Cholera Outbreak Data Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select Plot Type",
                  choices = c("WHO Cholera Report", 
                              "Epidemiological Curve", 
                              "Cholera Fatality Rate Map", 
                              "Global Cholera Fatality Rate", 
                              "Cholera Cases Over Time"))
    ),
    
    mainPanel(
      plotOutput("plot_output")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$plot_output <- renderPlot({
    if (input$plot_type == "WHO Cholera Report") {
      ggplot(Cholera_Table, aes(x = Year_range, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "WHO Cholera outbreak data report between 1940-2019",
             x = "Year_range",
             y = "Count") +
        scale_fill_manual(values = c("No_of_death" = "red", 
                                     "No_of_cases" = "blue")) +
        theme_minimal()
      
      
    } else if (input$plot_type == "Epidemiological Curve") {
      ggplot(data = data_1_, aes(x = Year, y = No_of_cases)) +
        geom_bar(stat = "identity", fill = "blue", color = "black") + 
        geom_line(aes(group = 1), color = "red", size = 1) +           
        geom_point(color = "red", size = 3) +                          
        labs(title = "Epidemiological Curve of Cholera Outbreak", 
             x = "Year", 
             y = "Number of Cases") +
        theme_minimal()
      
    } else if (input$plot_type == "Cholera Fatality Rate Map") {
      ggplot(data = map_data, aes(x = long, y = lat, group = group, fill = Fatality_rate)) +
        geom_polygon(color = "black") +
        scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
        labs(title = "World Map Representation of Cholera Fatality Rate (1940-2019)",
             fill = "Fatality Rate") +
        theme_minimal() +
        coord_fixed(1.3)
      
    } else if (input$plot_type == "Global Cholera Fatality Rate") {
      ggplot(data = world_cholera) +
        geom_sf(aes(fill = Fatality_rate), color = NA) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
        labs(title = "Global Cholera Fatality Rate",
             fill = "Fatality Rate (%)") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank()) +
        coord_sf()
      
    } else if (input$plot_type == "Cholera Cases Over Time") {
      ggplot(data = new_data_1_table, aes(x = Year, y = No_of_cases)) +
        geom_line(color = "blue", size = 1) +      
        geom_point(color = "red", size = 2) +      
        labs(title = "Cholera Outbreak Cases Over Time", 
             x = "Year", 
             y = "Number of Cases") +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

total_cases <- sum(combined_table$No_of_cases, na.rm = TRUE)
View(total_cases)
