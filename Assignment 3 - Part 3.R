# Load the proper libraries and set the right working directory
setwd("D:/Documenten/Artificial Intelligence Master/Semester 1/Experimentation in Psychology and Linguistics/Assignment 3")
library(readr)
library(tidyverse)
library(pwr)
# Load in the dataset
results_df <- read_delim("Word similarity.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

# Creating two subset dataframes using the select function and starts_with function 
# One subset for wordpairs with High Edit-distance(HED) and one for Low Edit-distance(LED)
HED_DF <- results_df %>% select(starts_with("HED"))
LED_DF <- results_df %>% select(starts_with("LED"))

# Compute the mean answer per parcipant/row (using rowMeans) so we can use these to test our hypothesis
HED_DF$Rowmeans <- rowMeans(HED_DF)
LED_DF$Rowmeans <- rowMeans(LED_DF)

# Test both vectors of mean scores if they are normally distributed
# First by looking at the descriptive statistics and then using a shapiro-wilks test
summary(HED_DF$Rowmeans)
summary(LED_DF$Rowmeans)
hist(HED_DF$Rowmeans)
hist(LED_DF$Rowmeans)
shapiro.test(HED_DF$Rowmeans)
shapiro.test(LED_DF$Rowmeans)

# Test if there is a statistical difference between the mean responses of respondents on LED pairs versus HED pairs
t.test(HED_DF$Rowmeans, LED_DF$Rowmeans)
plot(HED_DF$Rowmeans, LED_DF$Rowmeans)

# Because their is no statistical difference we need to do a power analysis
M1 <- mean(HED_DF$Rowmeans)
M2 <- mean(LED_DF$Rowmeans)
S1 <- sd(HED_DF$Rowmeans)
S2 <- sd(LED_DF$Rowmeans)
d <- (M1 - M2)/sqrt(((S1^2) + (S2^2))/2)

# Below wer find out how many participants we need to get to get sufficient power and significance level in our experiment
pwr.t.test(n = NULL  , d = d , sig.level =0.05 , power = 0.8 , type = "two.sample")

