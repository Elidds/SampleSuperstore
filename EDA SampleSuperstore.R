# Tittle: Sample Superstore Data exploration
# Install and load packages
install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               janitor,
               rstatix,
               flextable,
               ggplot2,
               dplyr,
               corrplot,
               tidyr,
               readxl)
# Load the dataset
library(readxl)
Super_store <- read_excel("Super_store.xlsx")
View(Super_store)

## Initial Data exploration
# View the first few rows
head(Super_store)

# Summary statistics
summary(Super_store)
print(summary(Super_store))

# Data structure
str(Super_store)

## Data Cleaning
# Check for missing values
colSums(is.na(Super_store))

# Remove duplicates
Super_store <- Super_store[!duplicated(Super_store), ]

## Univariate Analysis
# Distribution of Sales
ggplot(Super_store, aes(Sales)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Distribution of Sales")

# Distribution of Profit
ggplot(Super_store, aes(Profit)) + 
  geom_histogram(bins = 30, fill = "green", color = "black") +
  ggtitle("Distribution of Profit")

# Summary by Category
sales_summary<-Super_store %>% 
  group_by(Category) %>% 
  summarise(Total_Sales = sum(Sales), Total_Profit = sum(Profit)) %>%
  arrange(desc(Total_Sales))
print(sales_summary)

## Bivariate Analysis

# Sales vs. Profit
ggplot(Super_store, aes(x = Sales, y = Profit)) + 
  geom_point(alpha = 0.5, color = "red") + 
  ggtitle("Sales vs. Profit")

# Sales and Profit by Category
ggplot(Super_store, aes(x = Category, y = Sales, fill = Category)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Sales by Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Super_store, aes(x = Category, y = Profit, fill = Category)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Profit by Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Geographical Analysis
# Sales by State
Super_store %>% 
  group_by(State) %>% 
  summarise(Total_Sales = sum(Sales), Total_Profit = sum(Profit)) %>%
  arrange(desc(Total_Sales)) %>%
  ggplot(aes(x = reorder(State, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  ggtitle("Total Sales by State")

# Profit by Region
ggplot(Super_store, aes(x = Region, y = Profit, fill = Region)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Profit by Region")

## Correlation Analysis
# Correlation matrix
numeric_cols <- Super_store %>% select(Sales, Quantity, Discount, Profit)
cor_matrix <- cor(numeric_cols)

# Plot correlation matrix
corrplot(cor_matrix, method = "circle")

## Find Insights
# Identify top categories
Super_store %>% 
  group_by(Category) %>% 
  summarise(Total_Sales = sum(Sales), Total_Profit = sum(Profit)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)

Super_store %>% 
  group_by(`Sub-Category`) %>% 
  summarise(Total_Sales = sum(Sales), Total_Profit = sum(Profit)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)

## Save Findings
# Save summary statistics to CSV
write.csv(summary(Super_store), "summary_statistics.csv")





