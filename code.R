################################# 
# You can use this template to draft the script for your Assessment 2.
# More clarification and related resources can be found at
# https://d2l.deakin.edu.au/d2l/le/content/1193535/viewContent/6065224/View
#################################

install.packages("corrplot")
install.packages("moments")
install.packages("lpSolve")
install.packages("lpSolveAPI")
library(tidyverse)
library(corrplot)
library(ggplot2)
library(moments)


##################################
#Question 1 - Understand the Data
##################################

data.energy <- as.matrix(read.table("RedWine.txt"))

set.seed(219254708)

data.subset <- data.energy[sample(1:1599, 440), c(1:6)]

data.variable.names <- c("citric_acid", "chlorides", "total_sulfur_dioxide", "pH", "alcohol", "quality")

# Create 5 scatterplots function (for each X variable against the variable of interest Y)

colnames(data.subset) <- data.variable.names
wine_quality_data <- as.data.frame(data.subset)


plot(wine_quality_data$citric_acid,
     wine_quality_data$quality, main = "How Citric Acid affects the quality of wine?",
     xlab = "citric_acid", ylab = "quality",
     pch = 19, frame = FALSE)
cor(wine_quality_data$citric_acid, wine_quality_data$quality)

plot(wine_quality_data$chlorides,
     wine_quality_data$quality, main = "How chlorides affects the quality of wine?",
     xlab = "chlorides", ylab = "quality",
     pch = 19, frame = FALSE)
cor(wine_quality_data$chlorides, wine_quality_data$quality)

plot(wine_quality_data$total_sulfur_dioxide,
     wine_quality_data$quality, main = "How total sulfur dioxide affects the quality of wine?",
     xlab = "sulfur dioxide", ylab = "quality",
     pch = 19, frame = FALSE)
cor(wine_quality_data$total_sulfur_dioxide, wine_quality_data$quality)

plot(wine_quality_data$pH,
     wine_quality_data$quality, main = "How pH affects the quality of wine?",
     xlab = "pH", ylab = "quality",
     pch = 19, frame = FALSE)
cor(wine_quality_data$pH, wine_quality_data$quality)

plot(wine_quality_data$alcohol,
     wine_quality_data$quality, main = "How alcohol affects the quality of wine?",
     xlab = "alcohol", ylab = "quality",
     pch = 19, frame = FALSE)
cor(wine_quality_data$alcohol, wine_quality_data$quality)

numericVars <- which(sapply(wine_quality_data, is.numeric))
numericVarNames <- names(numericVars)
cat("There are", length(numericVars), "numeric variables")

all_numVar <- wine_quality_data[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs")

#Sort on decreasing correlations with quality
cor_sorted <- as.matrix(sort(cor_numVar[,"quality"], decreasing = TRUE))

#Selecting high correlations 
Cor_High <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0)))
cor_numVar <- cor_numVar[Cor_High, Cor_High]

corrplot.mixed(cor_numVar, tl.col = "black", tl.pos = "lt")

# Create 6 histograms for each X variable and Y
ggplot(wine_quality_data, aes(x = citric_acid)) + geom_histogram(fill = "blue", bins=40) + 
  ggtitle("Histogram for citric acid") #right skewed
ggplot(wine_quality_data, aes(x = chlorides)) + geom_histogram(fill = "blue",  bins=40) +
  ggtitle("Histogram for chlorides") #right skewed
ggplot(wine_quality_data, aes(x = total_sulfur_dioxide)) + geom_histogram(fill = "blue", bins=40) +
  ggtitle("Histogram for total sulfur dioxide") #right skewed
ggplot(wine_quality_data, aes(x = pH)) + geom_histogram(fill = "blue", bins=40) +
  ggtitle("Histogram for pH")
ggplot(wine_quality_data, aes(x = alcohol)) + geom_histogram(fill = "blue", bins=40) +
  ggtitle("Histogram for alcohol")
ggplot(wine_quality_data, aes(x = quality)) + geom_histogram(fill = "blue", bins=40) +
  ggtitle("Histogram for quality")



################################
#Question 2 - Transform the Data
################################


I <- c("citric_acid", "chlorides", "total_sulfur_dioxide", "alcohol", "quality") # Choose any four X variables and Y

variables_to_transform <- data.subset[,I]
variables_to_transform_df <- data.frame(variables_to_transform)

# for each variable, you need to figure out a good data transformation method, 
# such as Polynomial, log and negation transformation. The k-S test and Skewness 
# calculation may be helpful to select the transformation method

# minmax normalisation
minmax <- function(x){
  print(paste("min ", min(x)))
  print(paste("max ", max(x)))
  return((x - min(x))/(max(x)-min(x)))
}

# z-score standardisation and scaling to unit interval
unit.z <- function(x){
  print(paste("sd ", sd(x)))
  return(0.15*((x-mean(x))/sd(x)) + 0.5)
}

x1 <- variables_to_transform_df$citric_acid
ks.test(x1, "pnorm")
set.seed(219254708)
test_normal_data <- rchisq(100, 1)
qqnorm(x1)
qqline(test_normal_data, col = 'red', lwd = 2, lty = 2)


x2 <- variables_to_transform_df$chlorides
ks.test(x2, "pnorm")
set.seed(219254708)
test_normal_data <- rchisq(100, 1)
qqnorm(x2)
qqline(test_normal_data, col = 'red', lwd = 2, lty = 2)


x3 <- variables_to_transform_df$total_sulfur_dioxide
ks.test(x3, "pnorm")
set.seed(219254708)
test_normal_data <- rchisq(100, 1)
qqnorm(x3)
qqline(test_normal_data, col = 'red', lwd = 2, lty = 2)


x4 <- variables_to_transform_df$alcohol
ks.test(x4, "pnorm")
set.seed(219254708)
test_normal_data <- rchisq(100, 1)
qqnorm(x4)
qqline(test_normal_data, col = 'red', lwd = 2, lty = 2)

skewness(x1)
kurtosis(x1)

skewness(x2)
kurtosis(x2)

skewness(x3)
kurtosis(x3)

skewness(x4)
kurtosis(x4)


hist(minmax(x1), breaks=40, main="Transformed data for Citric Acid")
hist(log(x2), breaks=40, main="Transformed data for Chlorides") 
hist(log(x3), breaks=40, main="Transformed data for Total Sulfur Dioxide")
hist(unit.z(x4), breaks=40, main="Transformed data for Alcohol")

qqnorm(minmax(x1), pch = 1, frame = FALSE, main="QQ Plot for Citric Acid")
qqline(minmax(x1), col = "steelblue", lwd = 2)

qqnorm(log(x2), pch = 1, frame = FALSE, main="QQ Plot for Chlorides")
qqline(log(x2), col = "steelblue", lwd = 2)

qqnorm(log(x3), pch = 1, frame = FALSE, main="QQ Plot for Total Sulfur Dioxide")
qqline(log(x3), col = "steelblue", lwd = 2)

qqnorm(unit.z(x4), pch = 1, frame = FALSE, main="QQ Plot for Alcohol")
qqline(unit.z(x4), col = "steelblue", lwd = 2)

data.transformed <- data.frame(citric_acid=c(minmax(x1)),
                               chlorides = c(log(x2)),
                               total_sulfur_dioxide = c(log(x3)),
                               alcohol = c(unit.z(x4)),
                               quality = c(variables_to_transform[,5])
)


# Save this transformed data to a text file
write.table(data.transformed, "transformed.txt") 


##########################################
#Question 3 - Build models and investigate
##########################################


source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("transformed.txt"))  # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(data.transformed_copy)

# Get weights for Power Mean p=2 with fit.QAM()
fit.QAM(data.transformed_copy, g=QM)

# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(data.transformed_copy)



#######################################
#Question 4 - Use Model for Prediction
#######################################

new_input <- c(1, 0.075, 41, 3.53, 9.3) 

new_input_to_transform <- new_input[c(1,2,3,5)] # choose the same four X variables as in Q2 

# transforming the four variables in the same way as in question 2 
# minmax normalisation fit with min as 0 and max as 0.78
minmax.transformed <- function(x){
  return((x - 0)/(0.78-0))
}

# z-score standardisation and scaling to unit interval
# unit.z normalisation fit with mean sd as 1.07683683784681
unit.z.transformed <- function(x){
  return(0.15*((x-mean(x))/1.07683683784681) + 0.5)
}

transformed_input <- c(minmax.transformed(new_input_to_transform[1]),
                       log(new_input_to_transform[2]),
                       log(new_input_to_transform[3]),
                       unit.z.transformed(new_input_to_transform[4]))


weights <- c(0,0,0.999999999999996,0)
# applying the transformed variables to the best model selected from Q3 for Y prediction
prediction <- OWA(transformed_input, weights)


# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
#As no transformation for Prediction varibale Y
print(round(prediction))

 

#############################################################################################
# References 
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################

# You must cite all the datasets and packages you used for this assessment. 
# https://www.r-bloggers.com/2013/06/measures-of-skewness-and-kurtosis/
# https://www.statology.org/test-for-normality-in-r/