# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Load the dataset
df <- read_csv("C:\\A4\\pizza_data.csv", col_types = cols(
  brand = col_character(),
  price = col_character(),
  weight = col_character(),
  crust = col_character(),
  cheese = col_character(),
  size = col_character(),
  toppings = col_character(),
  spicy = col_character(),
  ranking = col_double()
))


# Convert categorical columns to factors
df <- df %>%
  mutate(across(c(brand, price, weight, crust, cheese, size, toppings, spicy), as.factor))

# Fit a linear regression model
model <- lm(ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy, data = df)
summary(model)


# Extract coefficients
coefficients <- coef(model)

# Define conjoint attributes
conjoint_attributes <- c('brand', 'price', 'weight', 'crust', 'cheese', 'size', 'toppings', 'spicy')

# Initialize lists for part-worths and importance
part_worth <- list()
part_worth_range <- numeric(length(conjoint_attributes))
important_levels <- list()


# Calculate part-worth utilities and attribute importance
for (attribute in conjoint_attributes) {
  levels <- levels(df[[attribute]])
  n_levels <- length(levels)
  
  part_worth[[attribute]] <- numeric(n_levels)
  
  for (i in seq_along(levels)) {
    level <- levels[i]
    coef_name <- paste(attribute, level, sep = "")
    part_worth[[attribute]][i] <- coefficients[coef_name]
  }
  
  # Normalize part-worths
  part_worth[[attribute]] <- part_worth[[attribute]] - mean(part_worth[[attribute]])
  
  # Calculate range of part-worths
  part_worth_range[attribute] <- max(part_worth[[attribute]]) - min(part_worth[[attribute]])
  
  # Determine most important level
  important_levels[[attribute]] <- levels[which.max(part_worth[[attribute]])]
  
}  

# Calculate relative importance of each attribute
total_range <- sum(part_worth_range)
attribute_importance <- 100 * part_worth_range / total_range


# Print results
print("Relative Importance of Attributes:")
print(attribute_importance)


print("Part-worth Utilities:")
print(part_worth)

print("Most Preferred Levels:")
print(important_levels)


# Plot relative importance of attributes
importance_df <- data.frame(attribute = names(attribute_importance), importance = attribute_importance)
ggplot(importance_df, aes(x = attribute, y = importance)) +
  geom_bar(stat = "identity") +
  ggtitle("Relative Importance of Attributes") +
  xlab("Attributes") +
  ylab("Importance (%)")


# Calculate utility scores for each profile
df <- df %>%
  rowwise() %>%
  mutate(utility = sum(
    part_worth$brand[brand],
    part_worth$price[price],
    part_worth$weight[weight],
    part_worth$crust[crust],
    part_worth$cheese[cheese],
    part_worth$size[size],
    part_worth$toppings[toppings],
    part_worth$spicy[spicy]
  ))


# Determine profile with highest utility score
max_utility_profile <- df[which.max(df$utility), ]

print("Profile with Highest Utility Score:")
print(max_utility_profile)


# Determine preferred levels for each attribute
preferred_levels <- sapply(conjoint_attributes, function(attr) {
  levels <- levels(df[[attr]])
  part_worth_values <- sapply(levels, function(level) {
    part_worth[[attr]][which(levels == level)]
  })
  levels[which.max(part_worth_values)]
})

print("Preferred Levels for Each Attribute:")
print(preferred_levels)