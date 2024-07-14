install.packages("forcats")
install.packages("leaflet")
install.packages("sp")
install.packages("sf")


library(ggplot2)
library(dplyr)
library(forcats)
library(leaflet)
library(sf)
library(sp)

#https://catalog.data.gov/dataset/electric-vehicle-population-data
df <- Electric_Vehicle_Population_Data

#Understanding The Data
colnames(df)

# Bernoulli distribution for electric vehicle type
electric_vehicle_type <- ifelse(df$Electric.Vehicle.Type == "Battery Electric Vehicle (BEV)", 1, 0)
p_bev <- mean(electric_vehicle_type)
p_bev

# Displaying the electric veichle types
unique(df$Electric.Vehicle.Type)

# BEV Percentage 
bev_percentage <- p_bev * 100
phev_percentage <- 100 - bev_percentage

# Adding data to a df 
bev_data <- data.frame(
  Type = c("BEV", "PHEV"),
  Percentage = c(bev_percentage, phev_percentage)
)

# Pie Chart
ggplot(bev_data, aes(x = "", y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Percentage of BEV and PHEV in Electric Vehicles",
       x = "",
       y = "") +
  theme_void() +
  scale_fill_manual(values = c("#5BD4D0", "#D3D3D3")) +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))


# Removing electrical range = 0 from the data set
df_filtered <- df[df$Electric.Range > 0, ]

# Normal distribution for electric range
electric_range <- df_filtered$Electric.Range
mean_range <- mean(electric_range, na.rm = TRUE)
sd_range <- sd(electric_range, na.rm = TRUE)

# Histogram and normal distribution curve
ggplot(df_filtered, aes(x = Electric.Range)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#5BD4D0", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean_range, sd = sd_range), color = "red", size = 1) +
  geom_vline(xintercept = 200, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Electric Vehicle Range Distribution",
       x = "Electric Range (miles)",
       y = "Density") +
  theme_minimal()


# Summary statistics of data
summary(df_filtered$Electric.Range)

# QQ Plot
qqnorm(df_filtered$Electric.Range)
qqline(df_filtered$Electric.Range, col = "red")

# Kolmogorov-Smirnov normality test
ks_test <- ks.test(df_filtered$Electric.Range, "pnorm", mean = mean_range, sd = sd_range)
ks_test

# Histogram and density plot
hist(df_filtered$Electric.Range, breaks = 30, probability = TRUE, col = "lightblue", 
     main = "Electric Vehicle Range Distribution", xlab = "Electric Range (miles)")
lines(density(df_filtered$Electric.Range), col = "red", lwd = 2)
curve(dnorm(x, mean = mean_range, sd = sd_range), col = "blue", lwd = 2, add = TRUE)
abline(v = 200, col = "darkblue", lwd = 2, lty = 2)



#Data of the electric range column does not look correct, so I applied some tests.
#According to the results of the Kolmogorov-Smirnov (KS) test, electric vehicle range data does 
#not comply with the normal distribution. The D value was calculated to be 0.20068, which represents 
#the difference between distributions, and a p-value much smaller than 0.05 provides strong evidence 
#that the data does not follow a normal distribution. This may indicate that electric vehicle range data
#is not normally distributed and there may be anomalies in the data.
#I checked some of the data that appeared to be anomalies in the data set and researched it on the 
#Internet and I have determined that the data is correct.


# Calculate brand distribution
brand_distribution <- df %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Total number of vehicles
total_vehicles <- sum(brand_distribution$count)

# Calculate %s and group other brands
brand_distribution <- brand_distribution %>%
  mutate(percent = (count / total_vehicles) * 100) %>%
  mutate(brand_group = if_else(row_number() <= 20, as.character(Make), "Other"))

# Calculate the total number of vehicles of other brands
other_count <- sum(brand_distribution$count[brand_distribution$brand_group == "Other"])
other_percent <- (other_count / total_vehicles) * 100

brand_distribution <- brand_distribution %>%
  filter(brand_group != "Other") %>%
  bind_rows(data.frame(Make = "Other", count = other_count, percent = other_percent, brand_group = "Other"))


# Plotting Electric Vehicle Population By Model (Top 20)
ggplot(model_counts, aes(x = fct_reorder(Model, n), y = n, fill = Model)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  coord_flip() +
  labs(title = "Electric Vehicle Population By Model (Top 20)",
       x = "Model",
       y = "Frequency",
       fill = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        legend.position = "none")

# Filter only Tesla branded vehicles
tesla_vehicles <- df %>%
  filter(Make == "TESLA")

# Calculate Model distribution of Tesla branded vehicles
tesla_model_distribution <- tesla_vehicles %>%
  group_by(Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

model_counts <- df %>% 
  count(Model) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 20)  

# Plot
ggplot(model_counts, aes(x = reorder(Model, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#5BD4D0") +
  labs(title = "Model Usage Frequency",
       x = "Model",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total veichles
total_tesla_vehicles <- sum(tesla_model_distribution$count)

# calculate %s
tesla_model_distribution <- tesla_model_distribution %>%
  mutate(percent = (count / total_tesla_vehicles) * 100)

# Plot: Tesla Electric Vehicle Model Distribution
ggplot(tesla_model_distribution, aes(x = fct_reorder(Model, -count), y = percent, fill = Model)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5, size = 3) +
  coord_flip() +
  labs(title = "Tesla Electric Vehicle Model Distribution",
       x = "Model",
       y = "Percentage (%)",
       fill = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8))



# County distribution
county_distribution <- df %>%
  group_by(County) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# top 10 countries
top_counties <- county_distribution[1:10, ]

# Calculate the total of other countries
other_county_count <- sum(county_distribution$count) - sum(top_counties$count)

# Update data by adding "Other" category
top_counties <- bind_rows(top_counties, data.frame(County = "Other", count = other_county_count))

# total county vehicles
total_county_vehicles <- sum(top_counties$count)

# Calculate percentages
top_counties <- top_counties %>%
  mutate(percent = (count / total_county_vehicles) * 100)

# bar chart: Electric Vehicle Population by County
ggplot(top_counties, aes(x = fct_reorder(County, -count), y = count, fill = County)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5, size = 3) +
  coord_flip() +
  labs(title = "Electric Vehicle Population by County",
       x = "County",
       y = "Count",
       fill = "County") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        legend.position = "none")

# Calculating city distribution
city_distribution <- df %>%
  group_by(City) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Top 10 cities
top_cities <- city_distribution[1:10, ]

# Calculate total for other cities
other_city_count <- sum(city_distribution$count) - sum(top_cities$count)

# Update data by adding "Other" category
top_cities <- bind_rows(top_cities, data.frame(City = "Other", count = other_city_count))

# total city vehicles
total_city_vehicles <- sum(top_cities$count)

# Calculate percentages
top_cities <- top_cities %>%
  mutate(percent = (count / total_city_vehicles) * 100)

# Drawing a bar chart: Electric Vehicle Population by City
ggplot(top_cities, aes(x = fct_reorder(City, -count), y = count, fill = City)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5, size = 3) +
  coord_flip() +
  labs(title = "Electric Vehicle Population by City",
       x = "City",
       y = "Count",
       fill = "City") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        legend.position = "none")



# Calculate distribution of model years
model_year_distribution <- df %>%
  group_by(Model.Year) %>%
  summarise(count = n()) %>%
  arrange(Model.Year)

# Bar chart: Electric Vehicle Population by Model Year
ggplot(model_year_distribution, aes(x = as.factor(Model.Year), y = count)) +
  geom_col(fill = "#5BD4D0", width = 0.7) +
  geom_text(aes(label = count), vjust = -0.3, size = 3, color = "black") +
  labs(title = "Electric Vehicle Population by Model Year",
       x = "Model Year",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))


#It didn't seem right that it was a 1997 model electric vehicle. 


# Finding the model of vehicles produced in 1997
df %>%
filter(Model.Year == 1997) %>%
select(Model)


#The Chevrolet S-10 Electric was an American electric-powered vehicle 
#built by Chevrolet. It was introduced in 1997, becoming the world's 
#first electric pickup truck from the original manufacturer
#So the data is accurate


# calculate the correlation between model year and electric range
cor_model_year <- cor(df_filtered$Model.Year, df_filtered$Electric.Range, use = "complete.obs")
cat("Model Yılı ile Elektrik Menzili Arasındaki Korelasyon:", cor_model_year, "\n")


# filter hybrid vehicles
df_non_hybrid_clean <- df_non_hybrid[df_non_hybrid$Electric.Range > 0 & df_non_hybrid$Electric.Range < 400, ]

#Relationship Between Model Year and Electric Range
ggplot(df_non_hybrid, aes(x = Model.Year, y = Electric.Range)) +
  geom_point(alpha = 0.5, color = "#5BD4D0") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Model Year vs. Electric Range (Excluding Hybrids)",
       x = "Model Year",
       y = "Electric Range (miles)") +
  ylim(0, 350)



