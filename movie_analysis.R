library(ggplot2)

movies  <- read.csv("tmdb_5000_movies.csv")
credits <- read.csv("tmdb_5000_credits.csv")

str(movies)
str(credits)

# Clean Movies Dataset

# Check missing values
colSums(is.na(movies))

# Remove missing values
movies_clean <- na.omit(movies)

# Remove rows where budget or revenue is 0
movies_clean <- movies_clean[movies_clean$budget > 0 &
                               movies_clean$revenue > 0, ]

# Select important columns only
movies_clean <- movies_clean[, c("title", "budget", "revenue",
                                 "vote_average", "vote_count", "runtime")]

# Clean Credits Dataset

# Check missing values
colSums(is.na(credits))

# Remove missing values
credits_clean <- na.omit(credits)

# Select useful columns
credits_clean <- credits_clean[, c("title", "cast", "crew")]

# Outlier Removal using IQR Method

remove_outliers <- function(x) {
  Q1      <- quantile(x, 0.25)
  Q3      <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  lower   <- Q1 - 1.5 * IQR_val
  upper   <- Q3 + 1.5 * IQR_val
  return(x >= lower & x <= upper)
}

movies_clean <- movies_clean[
  remove_outliers(movies_clean$budget) &
    remove_outliers(movies_clean$revenue), ]

# Merge Both Datasets

final_data <- merge(movies_clean, credits_clean, by = "title")

# Check final dataset
dim(final_data)
head(final_data)
summary(final_data)

# Convert to Millions for Readable Graphs 

final_data$budget_m  <- final_data$budget  / 1e6
final_data$revenue_m <- final_data$revenue / 1e6

# Exploratory Data Analysis

# --- Graph 1: Budget vs Revenue ---
ggplot(final_data, aes(x = budget_m, y = revenue_m)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.8) +
  geom_smooth(method = "lm", color = "red",
              se = TRUE, linewidth = 1.2) +
  scale_x_continuous(labels = function(x) paste0("$", x, "M")) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
  labs(
    title    = "Budget vs Box Office Revenue",
    subtitle = "Higher budget films generally earn more, but with wide variation",
    x        = "Production Budget (in Millions USD)",
    y        = "Box Office Revenue (in Millions USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "gray40", size = 11),
    panel.grid.minor = element_blank()
  )

# --- Graph 2: IMDb Rating vs Revenue ---
ggplot(final_data, aes(x = vote_average, y = revenue_m)) +
  geom_point(alpha = 0.35, color = "darkgreen", size = 1.8) +
  geom_smooth(method = "lm", color = "red",
              se = TRUE, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
  labs(
    title    = "IMDb Rating vs Box Office Revenue",
    subtitle = "Rating has only a weak effect on how much a movie earns",
    x        = "IMDb Rating (out of 10)",
    y        = "Box Office Revenue (in Millions USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "gray40", size = 11),
    panel.grid.minor = element_blank()
  )

# --- Graph 3: Vote Count vs Revenue ---
ggplot(final_data, aes(x = vote_count, y = revenue_m)) +
  geom_point(alpha = 0.35, color = "tomato", size = 1.8) +
  geom_smooth(method = "lm", color = "darkblue",
              se = TRUE, linewidth = 1.2) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
  labs(
    title    = "Vote Count vs Box Office Revenue",
    subtitle = "More audience votes = more revenue - strongest relationship found",
    x        = "Number of IMDb Votes",
    y        = "Box Office Revenue (in Millions USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "gray40", size = 11),
    panel.grid.minor = element_blank()
  )

# --- Correlation Matrix ---
cor_data   <- final_data[, c("budget", "revenue", "vote_average",
                             "vote_count", "runtime")]
cor_matrix <- cor(cor_data)
print(cor_matrix)

# Create Target Variable (Hit / Flop)

final_data$success <- ifelse(
  final_data$revenue > final_data$budget * 2, "Hit", "Flop"
)

final_data$success <- as.factor(final_data$success)

# Check distribution
table(final_data$success)

# Train-Test Split (70% - 30%)

set.seed(123)
train_index <- sample(1:nrow(final_data), 0.7 * nrow(final_data))
train <- final_data[train_index, ]
test  <- final_data[-train_index, ]

# Linear Regression Model

model_lm <- lm(revenue ~ budget + vote_average + vote_count + runtime,
               data = train)

# Model summary
summary(model_lm)

# Predictions on test set
pred_lm <- predict(model_lm, test)

# Evaluation
residuals_lm <- test$revenue - pred_lm
rmse         <- sqrt(mean(residuals_lm^2))
cat("RMSE      :", rmse, "\n")
cat("R-squared :", summary(model_lm)$r.squared, "\n")

# --- Graph 4: Residuals vs Fitted Values ---
plot_data <- data.frame(
  fitted    = pred_lm      / 1e6,
  residuals = residuals_lm / 1e6
)

ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.8) +
  geom_hline(yintercept = 0, color = "red",
             linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", color = "orange",
              se = FALSE, linewidth = 1) +
  scale_x_continuous(labels = function(x) paste0("$", x, "M")) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
  labs(
    title    = "Residuals vs Fitted Values",
    subtitle = "Points should be randomly scattered around the red line for a good model",
    x        = "Fitted (Predicted) Revenue (in Millions USD)",
    y        = "Residuals (Actual - Predicted)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "gray40", size = 11),
    panel.grid.minor = element_blank()
  )

# Logistic Regression Model

# Keep only Hit and Flop for binary classification
train_bin <- train[train$success != "Average", ]
test_bin  <- test[test$success   != "Average", ]
train_bin$success <- droplevels(train_bin$success)
test_bin$success  <- droplevels(test_bin$success)

# Build model
model_log <- glm(success ~ budget + vote_average + vote_count + runtime,
                 data   = train_bin,
                 family = "binomial")

summary(model_log)

# Predictions
pred_prob <- predict(model_log, test_bin, type = "response")
pred_log  <- ifelse(pred_prob > 0.5, "Hit", "Flop")

# Logistic Regression Evaluation

# Confusion Matrix
conf_matrix <- table(Predicted = pred_log,
                     Actual    = test_bin$success)
print(conf_matrix)

# Accuracy
accuracy <- sum(pred_log == test_bin$success) / nrow(test_bin)
cat("Logistic Regression Accuracy:", round(accuracy * 100, 2), "%\n")