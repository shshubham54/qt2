# case prep
# Import data set of Professor Proposes
# install.packages("mltools")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("tidyr")
library(readxl)
library(mltools)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)

raw_data <- read_excel("C:/Users/shubh/OneDrive/Desktop/QT/Test/W06586-XLS-ENG.xls")
names(raw_data)

#Considering the relevant columns
raw_data <- raw_data[-c(0,1, 2), ]

# Set the first row as column names
colnames(raw_data) <- raw_data[1, ]

# Remove the first row
raw_data <- raw_data[-1, ]
# Convert 'Carat' to numeric in the original data frame
raw_data$Carat <- as.numeric(as.character(raw_data$Carat))
raw_data$Wholesaler <- as.numeric(as.character(raw_data$Wholesaler))


names(raw_data)
summary(raw_data)
#View(raw_data)
# This will return the type of each column in the dataframe
temp=data.frame(raw_data)
sapply(raw_data, typeof)  
typeof(temp)

#-----------------------CHECK FOR MISSINGDATA AND DATA CLEANSING-------
# Check for missing data in each column of raw_data
missing_data <- sapply(raw_data, function(x) sum(is.na(x)))

# Print the result
print(missing_data)

print("No missing data found")




#-----------------------CHECK FOR MISSINGDATA AND DATA CLEANSING-------


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

#-----------------------GGPLOT TO UNDERSTAND THE EFFECT OF VARIOUS CATEGORICAL VARIABLES ON PRICE--------

ggplot(
  data = temp,
  mapping = aes(x = Cut, y=Price, color=Cut)
) +
  geom_point()


ggplot(
  data = raw_data,
  mapping = aes(x = Clarity, y=Price, color=Clarity)
) +
  geom_point()

ggplot(
  data = raw_data,
  mapping = aes(x = Polish, y=Price, color=Polish)
) +
  geom_point()

ggplot(
  data = raw_data,
  mapping = aes(x = Clarity, y=Price)
) +
  geom_boxplot(fill="steelblue")



#-----------------------GGPLOT TO UNDERSTAND THE EFFECT OF VARIOUS CATEGORICAL VARIABLES ON PRICE--------












Model_all = lm(Price ~ Carat + factor(Colour) + factor(Clarity) + factor(Cut) + factor(Certification) + factor(Polish) + factor(Symmetry) + Wholesaler, data = raw_data)
View(Model_all)

#Plotting original vs predicted value of Price
plot(raw_data$Price, Model_all$fitted.values, col = "red", pch = 19, cex.lab = 1.5, xlab = "Actual Price", ylab = "Predicted Price", main = "Actual Price vs Predicted Price")

summary(Model_all)
Model_all.StdRes = rstandard(Model_all)
abline(Model_all, col = "red", lwd=1.5)
anova(Model_all)

#Ignore this section
# # Define new observation
# Define new observation
# Profdata <- data.frame(
#   Carat = as.double(0.9),
#   Colour = factor("J", levels = levels(as.factor(raw_data$Colour))),
#   Clarity = factor("SI2", levels = levels(raw_data$Clarity)),
#   Cut = factor("V", levels = levels(raw_data$Cut)),
#   Certification = factor("GIA", levels = levels(raw_data$Certification)),
#   Polish = factor("B", levels = levels(raw_data$Polish)),
#   Symmetry = factor("V", levels = levels(raw_data$Symmetry)),
#   Wholesaler = as.numeric(2)
# )

# Profdata2 <- data.frame(Carat = as.double(0.9), Colour=factor("J"), Clarity = factor("SI2"), Cut=factor("V"), Certification= factor("GIA") , Polish = factor("B"), Symmetry = factor("V"), Wholesaler = as.numeric(2))
# Profdata3 <- data.frame(Carat = as.double(0.9), Colour=factor("J"), Clarity = "SI2", Cut="V", Certification= "GIA" , Polish = "B", Symmetry = "V", Wholesaler = as.numeric(2))
# 
# View(Profdata)
# View(Profdata2)
# View(Profdata3)
# # Use the model to predict the response value for the new observation
# 
# predict(Model_all, newdata = Profdata)
# predict(Model_all, newdata = Profdata2)
# predict(Model_all, newdata = Profdata3)
# 
# # Print the levels of the 'Colour' column
# print(levels(raw_data$Colour))
# # Print the levels of the 'Clarity' column
# print(levels(raw_data$Clarity))
