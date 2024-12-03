#===============================================================================
# STEP 1: SUBSET DATA THEN CREATE PlayerCountry COLUMN
#===============================================================================

# Load required library
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(car)
library(randomForest)
library(xgboost)

# Load the dataset
data <- read.csv('mps.csv')

# Subset data so that it has rows where df$StatsName has values in the list below
selected_stats <- c(
  "Assists",
  "Attempts Accuracy",
  "Blocks",
  "Clearances",
  "Corners",
  "Distance covered (Km)",
  "Dribbling",
  "Fouls committed",
  "Free kicks on goal ",
  "Goals",
  "Handball",
  "Offsides",
  "Passes accuracy",
  "Player average speed",
  "Punches",
  "Red cards",
  "Saves",
  "Sprints",
  "Tackles",
  "Throw-in",
  "Yellow cards"
)

# Subset the data where StatsName is in the selected list
data <- data %>% filter(StatsName %in% selected_stats)


# Now determine the country associated with each PlayerID in all matches
# A player could play as part of either the HomeTeam or AwayTeam, so we need to gather both

# Create new dataframes where each player is associated with the teams they played for (Home or Away)
home_teams <- data %>% select(PlayerID, HomeTeamName) %>% rename(PlayerCountry = HomeTeamName)
away_teams <- data %>% select(PlayerID, AwayTeamName) %>% rename(PlayerCountry = AwayTeamName)

# Combine the two dataframes (home and away teams) into a single one
all_player_countries <- bind_rows(home_teams, away_teams)

# Step 2: Group by PlayerID and find the most frequent country (mode)
player_country <- all_player_countries %>%
  group_by(PlayerID) %>%
  summarise(PlayerCountry = names(sort(table(PlayerCountry), decreasing = TRUE))[1])

# Step 3: Merge this most frequent country back into the original dataframe
data <- data %>%
  left_join(player_country, by = "PlayerID")

#===============================================================================
# STEP 2: CLEAN DATA
#===============================================================================

#  Ensure that data$MatchID is stored as a character vector
data$MatchID <- as.character(data$MatchID)

# Coerce Value column to be numeric

data$Value <- as.numeric(data$Value)
data$Value[is.na(data$Value)] <- NaN

# Check for missing values
colSums(is.na(data))

#===============================================================================
# STEP 3: CREATE A WIDE DATAFRAME
#===============================================================================

# Group by MatchID, PlayerCountry, and StatsName and then summarize by summing the Value
grouped_data <- data %>%
  group_by(MatchID, PlayerCountry, StatsName) %>%
  summarize(Sum_Value = sum(Value, na.rm = TRUE), .groups = 'drop')

# Pivot the dataset to have each unique StatsName as a column
df <- grouped_data %>%
  pivot_wider(names_from = StatsName, values_from = Sum_Value, values_fill = list(Sum_Value = 0))

# Rearrange columns so that 'Goals' is the last column
df <- df[c(setdiff(names(df), "Goals"), "Goals")]

# Replace all spaces in column names with underscores and convert to lowercase
colnames(df) <- tolower(gsub(" ", "_", colnames(df)))

#Check new column names
colnames(df)

#===============================================================================
# STEP 4: CREATE VISUALIZATIONS
#===============================================================================
# Count frequencies of each number of goals scored
bar <- df %>%
  count(goals) %>%  # count the occurrences of each unique value in Goals
  arrange(goals)    # sort by Goals

# Create the bar plot
ggplot(bar, aes(x = goals, y = n)) +
  geom_bar(stat = "identity", fill = "#10727a") +   # Bar chart with specified color
  theme_minimal() +                                  # Minimal theme for clean look
  labs(x = "Goals", y = "Count", title = "Goals Distribution") +
  theme(legend.position = "none",                   # Remove legend
        plot.title = element_text(hjust = 0.5))      # Center the title



# Correlation Matrix
# Select only numeric columns from the dataframe
df_numeric <- df[sapply(df, is.numeric)]

# Step 2: Compute the correlation matrix for the numeric columns
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Round the correlation matrix to 2 decimal places
cor_matrix <- round(cor_matrix, 2)

# Step 3: Get upper triangle of the correlation matrix (for visualization purposes)
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}


# Get upper triangle of the correlation matrix
upper_tri <- get_upper_tri(cor_matrix)

# Step 4: Melt the correlation matrix into a long format for ggplot2
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Step 5: Create the heatmap using ggplot2
heatmap_plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") +  # Tile borders
  scale_fill_gradient2(low = "red", high = "green", mid = "white",  # Color scale
                       midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),  # Angle and size of axis text
        axis.text.y = element_text(size = 10),  # Size of y-axis labels
        axis.title.x = element_blank(), axis.title.y = element_blank(),  # Remove axis titles
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.border = element_blank(),  # Remove panel border
        panel.background = element_blank(),  # Remove panel background
        axis.ticks = element_blank()) +  # Remove axis ticks
  coord_fixed()  # Equal aspect ratio to ensure proper visualization of the matrix

# Step 6: Add correlation coefficients on the heatmap
heatmap_with_labels <- heatmap_plot + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +  # Add text labels for correlation values
  theme(legend.justification = c(1, 0), 
        legend.position.inside = c(0.6, 0.7),  # Updated argument for legend position
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))

# Step 7: Print the heatmap
print(heatmap_with_labels)


#===============================================================================
# STEP 5: CREATE AND EVALUATE PREDICTIVE MODELS
#===============================================================================

# Set X (features) and y (target) variables
X <- df_numeric[, setdiff(names(df_numeric), "goals")]  # All columns except 'goals'
y <- df_numeric$goals  # 'goals' as the response variable

# 2. Scale the X variables
X_scaled <- scale(X)

# 3. Split the data into training and testing sets
set.seed(57)  # For reproducibility
train_index <- sample(1:nrow(df_numeric), 0.80 * nrow(df_numeric))  # 80% training, 20% testing
X_train <- X_scaled[train_index, ]
y_train <- y[train_index]
X_test <- X_scaled[-train_index, ]
y_test <- y[-train_index]

# MODEL 1: MULTIPLE LINEAR REGRESSION

# 4. Perform MLR (Multiple Linear Regression) on the training set
mlr_model <- lm(y_train ~ ., data = data.frame(X_train))

# 5. Evaluate the MLR using the testing set
y_pred <- predict(mlr_model, newdata = data.frame(X_test))

# Calculate R-Squared
rss <- sum((y_test - y_pred)^2)  # Residual Sum of Squares
tss <- sum((y_test - mean(y_test))^2)  # Total Sum of Squares
r_squared <- 1 - (rss / tss)  # R-Squared

# Calculate Mean Squared Error (MSE)
mlr_mse <- mean((y_test - y_pred)^2)

# Calculate Root Mean Squared Error (RMSE)
mlr_rmse <- sqrt(mlr_mse)

# Print evaluation metrics
print(paste("R-Squared on the testing set:", round(r_squared, 4)))
print(paste("Mean Squared Error (MSE) on the testing set:", round(mlr_mse, 4)))
print(paste("Root Mean Squared Error (RMSE) on the testing set:", round(mlr_rmse, 4)))


# MODEL 2: RANDOM FOREST

# 4. Perform Random Forest on the training set
rf_model <- randomForest(x = X_train, y = y_train)

# 5. Evaluate the Random Forest model using the testing set
y_pred <- predict(rf_model, newdata = X_test)

# Calculate R-Squared
rss <- sum((y_test - y_pred)^2)  # Residual Sum of Squares
tss <- sum((y_test - mean(y_test))^2)  # Total Sum of Squares
r_squared <- 1 - (rss / tss)  # R-Squared

# Calculate Mean Squared Error (MSE)
rf_mse <- mean((y_test - y_pred)^2)

# Calculate Root Mean Squared Error (RMSE)
rf_rmse <- sqrt(rf_mse)

# Print evaluation metrics
print(paste("R-Squared on the testing set:", round(r_squared, 4)))
print(paste("Mean Squared Error (MSE) on the testing set:", round(rf_mse, 4)))
print(paste("Root Mean Squared Error (RMSE) on the testing set:", round(rf_rmse, 4)))

# MODEL 3: XG BOOST

# 4. Perform XGBoost on the training set
# Convert data into DMatrix format for XGBoost
train_data <- xgb.DMatrix(data = X_train, label = y_train)
test_data <- xgb.DMatrix(data = X_test, label = y_test)

# Train the XGBoost model
xgb_model <- xgboost(data = train_data, nrounds = 100, objective = "reg:squarederror")

# 5. Evaluate the XGBoost model using the testing set
y_pred <- predict(xgb_model, newdata = test_data)

# Calculate R-Squared
rss <- sum((y_test - y_pred)^2)  # Residual Sum of Squares
tss <- sum((y_test - mean(y_test))^2)  # Total Sum of Squares
r_squared <- 1 - (rss / tss)  # R-Squared

# Calculate Mean Squared Error (MSE)
xgb_mse <- mean((y_test - y_pred)^2)

# Calculate Root Mean Squared Error (RMSE)
xgb_rmse <- sqrt(xgb_mse)

# Print evaluation metrics
print(paste("R-Squared on the testing set:", round(r_squared, 4)))
print(paste("Mean Squared Error (MSE) on the testing set:", round(xgb_mse, 4)))
print(paste("Root Mean Squared Error (RMSE) on the testing set:", round(xgb_rmse, 4)))

# MODEL 1: REDONE TO REPORT MOST INFLUENTIAL MATCH STATISTICS

# 1. Set X (features) and y (target) variables using the entire dataset
X <- df_numeric[, setdiff(names(df_numeric), "goals")]  # All columns except 'goals'
y <- df_numeric$goals  # 'goals' as the response variable

# 2. Perform MLR (Multiple Linear Regression) with intercept using the entire dataset
mlr_model_full <- lm(y ~ ., data = data.frame(X))  # lm() automatically includes the intercept

# 3. Summary of the MLR model (includes coefficients, R-squared, p-values, etc.)
summary(mlr_model_full)