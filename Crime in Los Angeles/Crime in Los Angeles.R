#installing packages and libraries
install.packages("dplyr")
install.packages("kableExtra")
install.packages("DT")
install.packages("gt")

library(dplyr)
library(ggplot2)
library(kableExtra)
library(DT)
library(gt)
library(lubridate)


Sys.setlocale("LC_ALL", "en_US.UTF-8")


#easier name
df <- Crime_Data_from_2020_to_Present


# types of crimes and list the most common ones
crime_counts <- df %>%
  group_by(Crm.Cd.Desc) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# top 20 crime type
head(crime_counts, 20)


top_20_crimes <- df %>%
  count(Crm.Cd.Desc, sort = TRUE) %>%
  slice_head(n = 20)

# Extract names and counts
crime_names <- top_20_crimes$Crm.Cd.Desc
crime_counts <- top_20_crimes$n



# Data preperation for the plot
crime_data <- data.frame(
  crime_names = crime_names,
  crime_counts = crime_counts
)


# Plot
ggplot(crime_data, aes(x = crime_counts, y = reorder(crime_names, crime_counts))) +
  geom_bar(stat = "identity", fill = "#40BCBC") +
  theme_minimal(base_size = 12) +  
  labs(
    title = "Most Common Types of Crime",
    x = "Count",
    y = ""
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12)
  )



# converting the DATE.OCC column to Date format
df$DATE.OCC <- as.Date(df$DATE.OCC, format = "%m/%d/%Y")

# Getting years
df$Year <- format(df$DATE.OCC, "%Y")

#calculating how many crimes are reported each year
annual_crime_counts <- df %>%
  filter(Year != "2024") %>%
  count(Year)

#Display results
print(annual_crime_counts)

annual_crime_counts %>%
  kbl(caption = "Annual Crime Counts (excluding 2024)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, background = "#5BD4D0", color = "white") %>%
  row_spec(1:nrow(annual_crime_counts), background = "#fff")

# Histogram of crimes are reported each year

colnames(annual_crime_counts) <- c("Year", "Crime Count")

annual_crime_counts <- df %>%
  filter(Year != "2024") %>%
  count(Year)

ggplot(annual_crime_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "#5BD4D0") +
  theme_minimal(base_size = 12) +   
  labs(
    title = "Annual Crime Counts (excluding 2024)",
    x = "Year",
    y = "Crime Count"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 10), 
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12)  
  )


#the total number of crimes in the regions
area_crime_counts <- df %>%
  count(AREA.NAME, sort = TRUE)

print(area_crime_counts)

area_crime_counts %>%
  gt() %>%
  tab_header(
    title = "Total Crime Counts by Area",
    subtitle = "Sorted by the highest crime counts"
  ) %>%
  cols_label(
    AREA.NAME = "Area Name",
    n = "Crime Count"
  ) %>%
  fmt_number(
    columns = vars(n),
    decimals = 0
  ) %>%
  data_color(
    columns = vars(n),
    colors = scales::col_numeric(
      palette = c("#E0F7FA", "#5BD4D0", "#00796B"),
      domain = NULL
    )
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  )


# Convert DATE.OCC column to Date format
df$DATE.OCC <- as.Date(df$DATE.OCC, format = "%m/%d/%Y")

# Extract weekday names
df$Weekday <- weekdays(df$DATE.OCC)

# Calculate daily crime counts
daily_crime_counts <- df %>%
  count(Weekday)

# Arrange weekdays in correct order
daily_crime_counts$Weekday <- factor(daily_crime_counts$Weekday, 
                                     levels = c("Monday", "Tuesday", "Wednesday", 
                                                "Thursday", "Friday", "Saturday", "Sunday"))

# Visualize the results
ggplot(daily_crime_counts, aes(x = Weekday, y = n)) +
  geom_bar(stat = "identity", fill = "#5BD4D0") + 
  labs(title = "Daily Crime Counts", x = "Day", y = "Number of Crimes") +
  theme(panel.background = element_rect(fill = "#fff")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Creating a time series chart
ggplot(daily_crime_counts, aes(x = Weekday, y = n, group = 1)) +
  geom_line(color = "#5BD4D0") +  
  geom_point(color = "#5BD4D0") +  
  labs(title = "Daily Crime Counts", x = "Day", y = "Number of Crimes") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  




# Age distribution of victims
age_distribution_filtered <- age_distribution[age_distribution > 1]


#age distribution histogram
ggplot(df[df$Vict.Age > 0, ], aes(x = Vict.Age)) +
  geom_histogram(binwidth = 5, fill = "#5BD4D0", color = "white") +
  labs(title = "Age Distribution of Victims", x = "Age", y = "Count") +
  theme_minimal()



#Removing useless data from vict.sex column
df_filtered <- df[df$Vict.Sex %in% c("M", "F"), ]

#Creating the gender distribution table
sex_distribution <- table(df_filtered$Vict.Sex)

#Gender Distribution of Victims
print("Gender distribution of victims:")
print(sex_distribution)

color_palette <- c("#C8F0F1", "#5BD4D0")

ggplot(data = data.frame(sex = names(sex_distribution), count = as.numeric(sex_distribution)), aes(x = sex, y = count, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Sex", y = "Number of Victim", title = "Gender Distribution of Victims") +
  scale_fill_manual(values = color_palette) + 
  theme_minimal()


# Converting dates to POSIXct format
df$DATE.OCC <- as.POSIXct(df$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")

# Getting Years
df$Year <- format(df$DATE.OCC, "%Y")

# Removing 2024
df <- df[df$Year != "2024", ]

# Calculating the number of crimes on a monthly basis
df$Month <- format(df$DATE.OCC, "%m")
crime_by_month <- table(df$Month)

# Calculating the number of crimes separately on a monthly basis
crime_counts <- sapply(1:12, function(m) {
  sum(df$Month == sprintf("%02d", m))
})

crime_counts


# Creating month names
months <- month.abb 
months <- factor(months, levels = month.abb)  

# Plotting the graph of Crime Statistics by Month

ggplot(data = data.frame(month = months, count = crime_counts), aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "#5BD4D0") +
  labs(x = "Month", y = "Number of Crimes", title = "Crime Statistics by Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Creating a time series chart

ggplot(data = data.frame(month = months, count = crime_counts), aes(x = month, y = count, group = 1)) +
  geom_line(color = "#5BD4D0") +
  geom_point(color = "#5BD4D0", size = 3) +
  labs(x = "Month", y = "Number of Crimes", title = "Crime Statistics by Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

