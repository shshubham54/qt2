# case prep
# Import data set of Professor Proposes
# install.packages("mltools")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("car")

library(readxl)
library(mltools)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(MASS)
library(car)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)



#-----------------------IMPORTING DATA -------
raw_data <- read_excel("C:/Users/shubh/OneDrive/Desktop/QT/Test/W06586-XLS-ENG.xls")
#-----------------------IMPORTING DATA -------

#-----------------------CHECK FOR MISSINGDATA AND DATA CLEANSING-------
#Considering the relevant columns
raw_data <- raw_data[-c(0,1, 2), ]

# Set the first row as column names
colnames(raw_data) <- raw_data[1, ]

# Remove the first row
raw_data <- raw_data[-1, ]

# Find negative numbers No negative numbers observed in our Price Column
negative_numbers_price <- raw_data[raw_data$Price < 0, ]

# Find negative numbers No negative numbers observed in our Carat Column
negative_numbers_carat <- raw_data[raw_data$Carat < 0, ]

names(raw_data)

summary(raw_data)
#View(raw_data)
# This will return the type of each column in the dataframe
sapply(raw_data, typeof)  

# Check for missing data in each column of raw_data
missing_data <- sapply(raw_data, function(x) sum(is.na(x)))

# Print the result
print(missing_data)

print("No missing data found")

#-----------------------CHECK FOR MISSINGDATA AND DATA CLEANSING-------

#--------EDA STARTS FROM HERE

#-----------------------SCATTERPLOT TO UNDERSTAND THE EFFECT OF CARAT ON PRICE FOR DIFFERENT WHOLESLALERS-----------

# Plot with color according to Wholesaler
plot(raw_data$Carat, raw_data$Price, 
     main = " Carat vs. Price Acc to clusters of Wholesaler",  
     col = ifelse(raw_data$Wholesaler == 1, "blue", ifelse(raw_data$Wholesaler == 2, "red", "green")), 
     pch = 19,
     xlab="Carat Size",
     ylab="Price")

# Add a legend
legend("topright", # place legend at top right corner of the plot
       legend = levels(as.factor(raw_data$Wholesaler)), # labels for legend
       fill = c("blue", "red", "green"), # colors for legend
       title = "Wholesaler") # title of the legend


#-----------------------SCATTERPLOT TO UNDERSTAND THE EFFECT OF CARAT ON PRICE FOR DIFFERENT WHOLESLALERS-----------

#------Violin plot of Carat Vs Wholesale----
summary_data <- raw_data %>%
  group_by(Wholesaler) %>%
  summarise(min_value = min(Carat), max_value = max(Carat))

table_plot <- summary_data %>%
  gt() %>%
  tab_spanner(label = "Carat", columns = c(min_value, max_value))

table_plot

ggplot(
  data = raw_data,
  mapping = aes(x = as.numeric(Carat), y =as.factor(Wholesaler), fill =as.factor(Wholesaler))) +
  geom_violin(width = 1.5)+
  labs(title = "Violin plot of Carat Vs Wholesaler",  x = "Carat", y = "Prices", color = "Species")+
  theme_bw()+
  scale_fill_brewer(palette="Set1")
coord_flip()
#------Violin plot of Carat Vs Wholesale----

#------Filtering raw_data Data according to wholesaler-----
library(dplyr)
Wholesaler_1_data <- raw_data %>% filter(Wholesaler == 1)
Wholesaler_2_data <- raw_data %>% filter(Wholesaler == 2)
Wholesaler_3_data <- raw_data %>% filter(Wholesaler == 3)
#------Filtering raw_data Data according to wholesaler-----

#--------Start with ML Modelling

#-----------------------SELECING A BASIC MULTIPLE LINEAR REGRESSION MODEL--------

# Fit your initial model
initial_model <- lm(as.numeric(Price) ~ as.numeric(Carat) + factor(Colour) + factor(Clarity) + factor(Cut) + factor(Certification) + factor(Polish) + factor(Symmetry) + factor(Wholesaler), data = raw_data)

# Print the summary of the final model
print(summary(initial_model))



# Extract the coefficient matrix
R_sq_value = summary(initial_model)$adj.r.squared
print(R_sq_value)
#Plotting original vs predicted value of Price
plot(raw_data$Price, initial_model$fitted.values, col = "red", pch = 19, cex.lab = 1.5, xlab = "Actual Price", ylab = "Predicted Price", main = "Actual Price vs Predicted Price")

# Extract the coefficient matrix
coef_matrix <- coef(initial_model)

abline(a = 0, b = 1, col = "black", lty = 5)
#-----------------------SELECING A BASIC BEST MULTIPLE LINEAR REGRESSION MODEL--------

#-----------------------ANOVA TEST-------
print(paste("Adjusted R Square Value of Original Model: ", R_sq_value))
anova(initial_model)

#As seen from above results removing features Polish and Symmetry did not much significance as compared to other features
#hence we can remove these features 

after_anova_model <- lm(as.numeric(Price) ~ as.numeric(Carat) + factor(Colour) + factor(Clarity) + factor(Cut) + factor(Certification)   + factor(Wholesaler), data = raw_data)

summary(after_anova_model)$adj.r.squared
anova(after_anova_model)


#As seen from above results removing features Polish and Symmetry did not result in much depreciation in Adjusted Rsqaure Value
#We can use this model going further 


#-----------------------ANOVA TEST-------

#-----------------------TEST FOR MULTICOLLINEARITY-----------------
#High Variance Inflation Factor (VIF) values indicate multicollinearity, which means that some predictor variables in your model are highly correlated with each other
#Here The Carat and Wholesaler variables have high VIF values (7.27 and 3.54 respectively), indicating they have strong multicollinearity with other predictor variables in your model.

vif(lm(as.numeric(Price) ~ as.numeric(Carat) + factor(Colour) + factor(Clarity) + factor(Cut) + factor(Certification)   + factor(Wholesaler), data = raw_data)
)

#-----------------------TEST FOR MULTICOLLINEARITY-------

#-----------------------AIC ANALYSIS OF COMPLETE MODEL-------
# Perform stepwise model selection
final_model <- stepAIC(after_anova_model, direction = "both")
summary(final_model)$adj.r.squared
#The Min AIC value is achieved when we remove CUT variable, hence we will try to understand the impact of removing these features on Adjusted sq
#-----------------------AIC ANALYSIS OF COMPLETE MODEL-------

#-----------------------SIMPLE LINEAR REGRESSION CUT VS PRICE-------

Test_model_cut=lm(Price ~ factor(Cut), data = raw_data)
summary(Test_model_cut)$adj.r.squared
plot(raw_data$Price, Test_model_cut$fitted.values, col = "red", pch = 19, cex.lab = 1.5, xlab = "Actual Price", ylab = "Predicted Price CLARITY", main = "Actual Price vs Predicted Price")


# As We can see that the Adjusted R-squared value of ml model Polish vs Price :  0.1090375 which is very low compared to actual model which is 0.985
# and the 2nd Lowest AIC value is of CUT, hence let us try to make a model without cut and see its Rsq Value

# Fit your initial model
model_iter1 <- lm(as.numeric(Price) ~ as.numeric(Carat) + factor(Colour) + factor(Clarity) + factor(Certification) + factor(Wholesaler), data = raw_data)
summary(model_iter1)$adj.r.squared

coef(model_iter1)

#As we can see that Adjusted Rsq Value is 0.9841846 which is lesser than the previous model
#hence we will not remove the CUT feature from model as removing the the CUT feature is increading error in the model
#-----------------------SIMPLE LINEAR REGRESSION CUT VS PRICE-------
###########################################################################################
#-----------------------IMPROVING THE MODEL BY CLUBBING MULTIPLE FEATURES DATA FORMATION-------

summary(final_model)$adj.r.squared
summary(final_model)
# Upon analysis of the summary data we find out that a few features of the model have similar coefficient value
# therefore we will now club these values 

# F=G->FG       | Colour
# VS1=VS2->VS12 | Clarity
# I=V=X->IVX    | Cut
new_data=raw_data

new_data$Colour <- ifelse(new_data$Colour %in% c("F", "G"), "FG", new_data$Colour)

new_data$Clarity <- ifelse(new_data$Clarity  %in% c("VS1", "VS2"), "VS12", new_data$Clarity)

new_data$Cut <- ifelse(new_data$Cut %in% c("I", "V", "X"), "IVX", new_data$Cut)

#-----------------------IMPROVING THE MODEL BY CLUBBING MULTIPLE FEATURES DATA FORMATION-------


#-----------------------NEW-CHECK FOR MISSINGDATA AND DATA CLEANSING-------
# Check for missing data in each column of new_data
missing_data_new <- sapply(new_data, function(x) sum(is.na(x)))

# Print the result
print(missing_data_new)

print("No missing data found")




#-----------------------NEW-CHECK FOR MISSINGDATA AND DATA CLEANSING-------

#--------EDA STARTS


#--------Start with ML Modelling

#-----------------------NEW-SELECING A BASIC MULTIPLE LINEAR REGRESSION MODEL--------

# Fit your initial model
initial_model_new <- lm(as.numeric(Price) ~ as.numeric(Carat) + factor(Colour) + factor(Clarity) + factor(Cut) + factor(Certification) + factor(Polish) + factor(Symmetry) + factor(Wholesaler), data = new_data)

# Print the summary of the final model
print(summary(initial_model_new))



# Extract the coefficient matrix
R_sq_value_new = summary(initial_model_new)$adj.r.squared

#Plotting original vs predicted value of Price
plot(new_data$Price, initial_model_new$fitted.values, col = "red", pch = 19, cex.lab = 1.5, xlab = "Actual Price", ylab = "Predicted Price", main = "Actual Price vs Predicted Price")

# Extract the coefficient matrix
coef_matrix <- coef(initial_model_new)

#-----------------------NEW-SELECING A BASIC BEST MULTIPLE LINEAR REGRESSION MODEL--------

#-----------------------NEW-ANOVA TEST-------
print(paste("Adjusted R Square Value of Initial Model: ", R_sq_value_new))
anova(initial_model_new)

#As seen from above results removing features Polish and Symmetry did not much significance as compared to other features
#hence we can remove these features 

after_anova_model_new <- lm(as.numeric(Price) ~ as.numeric(Carat) + factor(Colour) + factor(Clarity) + factor(Cut) + factor(Certification)   + factor(Wholesaler), data = new_data)

summary(after_anova_model_new)$adj.adj.r.squared
anova(after_anova_model_new)


#As seen from above results removing features Polish and Symmetry did not result in much depreciation in Adjusted Rsqaure Value
#We can use this model going further 


#-----------------------NEW-ANOVA TEST-------

#-----------------------NEW-TEST FOR MULTICOLLINEARITY-----------------
#High Variance Inflation Factor (VIF) values indicate multicollinearity, which means that some predictor variables in your model are highly correlated with each other
#Here The Carat and Wholesaler variables have high VIF values (7.27 and 3.54 respectively), indicating they have strong multicollinearity with other predictor variables in your model.

vif(after_anova_model_new)

#-----------------------NEW-TEST FOR MULTICOLLINEARITY-------

#-----------------------NEW-AIC ANALYSIS OF COMPLETE MODEL-------
# Perform stepwise model selection
final_model_new <- stepAIC(after_anova_model, direction = "both")
summary(final_model_new)$adj.r.squared
#The Min AIC value is achieved when we remove CUT variable, hence we will try to understand the impact of removing these features on Adjusted sq
#-----------------------NEW-AIC ANALYSIS OF COMPLETE MODEL-------

#-----------------------NEW-SIMPLE LINEAR REGRESSION CUT VS PRICE-------

Test_model_cut_new=lm(Price ~ factor(Cut), data = new_data)
summary(Test_model_cut_new)$adj.r.squared
plot(new_data$Price, Test_model_cut_new$fitted.values, col = "red", pch = 19, cex.lab = 1.5, xlab = "Actual Price", ylab = "Predicted Price CLARITY", main = "Actual Price vs Predicted Price")


# As We can see that the Adjusted R-squared value of ml model Polish vs Price :  0.1090375 which is very low compared to actual model which is 0.985
# and the 2nd Lowest AIC value is of CUT, hence let us try to make a model without cut and see its Rsq Value

# Fit your initial model
model_iter1_new <- lm(as.numeric(Price) ~ as.numeric(Carat) + factor(Colour) + factor(Clarity) + factor(Certification) + factor(Wholesaler), data = new_data)
summary(model_iter1_new)$adj.r.squared

coef(model_iter1_new)

#As we can see that Adjusted Rsq Value is 0.9841846 which is lesser than the previous model
#hence we will not remove the CUT feature from model as removing the the CUT feature is increading error in the model

#-----------------------NEW-SIMPLE LINEAR REGRESSION CUT VS PRICE-------
