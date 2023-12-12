# Load libraries
library(ggplot2)
library(dplyr)
library(leaps)
library(car)
# Load the data
# Assuming Diamond dataset is already loaded

# Convert categorical variables to factors
Diamond$Colour <- as.factor(Diamond$Colour)
Diamond$Clarity <- as.factor(Diamond$Clarity)
Diamond$Cut <- as.factor(Diamond$Cut)
Diamond$Certification <- as.factor(Diamond$Certification)
Diamond$Polish <- as.factor(Diamond$Polish)
Diamond$Symmetry <- as.factor(Diamond$Symmetry)
Diamond$Wholesaler <- as.factor(Diamond$Wholesaler)

# Create a transformed dataset
Diamond1 <- Diamond %>%
  mutate(Carat_squared = Carat^2, Carat_cube = Carat^3,
         Log_Price = log(Price))
log_price <- log(Diamond1$Price)

# Plot the histogram
hist(log_price, main = "Histogram of Log-Transformed Price", 
     xlab = "Log-Transformed Price", col = "blue", border = "black")
hist(Diamond1$Price, main = "Histogram of Price", 
     xlab = "Price", col = "blue", border = "black")
# Fit linear regression model with additional transformations
fit <- lm(Log_Price ~ Wholesaler + Carat + Colour + Clarity + Cut +
            Certification + Polish + Symmetry + Carat_squared + Carat_cube, data = Diamond1)

fit1 <- lm(Log_Price ~ Wholesaler + Carat + Colour + Clarity + 
            Certification  +  Carat_cube , data = Diamond1)

fit3 <- lm(Price ~ Wholesaler + Carat + Colour + Clarity + 
             Certification  +  Carat_cube , data = Diamond1)
fit4 <- lm(Log_Price ~ Carat + Colour + Clarity
               , data = Diamond1)
# View model summary
summary(fit)
summary(fit1)
summary(fit4)
vif(fit1)
vif(fit4)
# Make predictions
predictions <- exp(predict(fit4, newdata = Diamond1))

# Transform actual Log_Price to original scale
actual_values <- (Diamond1$Price)

# Create a scatterplot
plot(actual_values, predictions, main = "Model Price vs Actual Price", 
     xlab = "Actual Price", ylab = "Predicted Price", col = "blue")

abline(a = 0, b = 1, col = "red", lty = 2)

# Feature selection using best subset
Best_Subset <- regsubsets(Price ~ Wholesaler + Carat + Colour + Clarity + Cut +
                            Certification + Polish + Symmetry + Carat_squared + Carat_cube,
                          data = Diamond1,
                          nbest = 1,
                          nvmax = NULL,
                          force.in = NULL,
                          force.out = NULL,
                          method = "exhaustive")



# Extract the best subset models
summary_best = summary(Best_Subset)
summary_best
max_r2adj_index <- which.min(summary_best$adjr2)
print(max_r2adj_index)
adjusted_r_squared <- summary_best$adjr2[1]
print(adjusted_r_squared)
summary_best[["adjr2"]]
plot(summary_best[["adjr2"]])
# Extract the predictors for the chosen model
chosen_predictors <- coef(Best_Subset, id = max_r2adj_index)

# Print the model with the maximum adjusted R-squared
print(chosen_predictors)


plot(summary_best$bic)
plot(Best_Subset, scale="bic",col = c("blue", "green", "orange", "red"))

plot(best_subset_models$cp)
plot(Best_Subset, scale="Cp")

plot(best_subset_models$adjr2)
plot(Best_Subset, scale="adjr2")

