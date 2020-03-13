# Load the proper libraries and set the right working directory
setwd("D:/Documenten/Artificial Intelligence Master/Semester 1/Experimentation in Psychology and Linguistics/Assignment 3")
library(readr)
library(tidyverse)
library(pwr)
library(stringr)
library(ggplot2)
library(ggpubr)

# Load in the dataset using read_csv
results_df <- read_csv('results_08032020.csv', skip = 1)
wordpairs_df <-
  read_delim("similarity_data_secexp.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)
# Only select the responses
results_df <- results_df[2:22, 19:78]
write_csv(results_df, 'results.csv')
# Give the right name to the colnames
colnames(results_df) <-
  lapply(colnames(results_df),
         word,
         start = -3,
         end = -1)
# And add which group they belong to
wordpairs_df$wordpairs <-
  paste(wordpairs_df$word1, wordpairs_df$word2, sep = " vs. ")
cn_df <-
  data.frame(wordpairs = colnames(results_df),
             stringsAsFactors = FALSE)
wordpairs_df <-
  merge(
    y = wordpairs_df,
    x = cn_df ,
    all.x = TRUE,
    by = 'wordpairs',
    sort = FALSE
  )
wordpairs_df$colnames <-
  paste(wordpairs_df$condition, wordpairs_df$wordpairs, sep = " ")
colnames(results_df) <- wordpairs_df$colnames

# Only get the first token every column and changes the types to numeric
results_df <-
  as.data.frame(apply(
    results_df,
    MARGIN = c(1, 2),
    FUN = word,
    start = 1
  ))
results_df <-
  as.data.frame(apply(results_df, MARGIN = c(1, 2), as.numeric))
results_df
# Make three datasets according to groups
low_df <- results_df %>% select(starts_with('low'))
high_df <- results_df %>% select(starts_with('high'))
fill_df <- results_df %>% select(starts_with('fill'))


# Compute the mean answer per parcipant/row (using rowMeans) so we can use these to test our hypothesis
high_df$Rowmeans <- rowMeans(high_df)
low_df$Rowmeans <- rowMeans(low_df)
fill_df$Rowmeans <- rowMeans(fill_df)
groups <- c(rep("high", 21), rep("low", 21), rep("fill", 21))
rowmeans <- c(high_df$Rowmeans, low_df$Rowmeans, fill_df$Rowmeans)
plot_df <- data.frame(groups, rowmeans)
plot_df <- subset(plot_df, groups != "fill")

# Create a boxplot to analyse the difference in answers between the groups
p <-
  ggplot(plot_df, aes(x = groups, y = rowmeans, fill = groups)) +
  geom_boxplot() +
  ggtitle("Average answers of the participants per group") + 
  ylab("Average similarity score") + 
  ylim(0,10) + 
  scale_fill_brewer(palette = "Blues")
p

# Get the descriptive statistics
summary(high_df$Rowmeans)
sd(high_df$Rowmeans)
summary(low_df$Rowmeans)
sd(low_df$Rowmeans)
ggplot(plot_df, aes(x = rowmeans)) + 
  geom_histogram(bin_width = 1, color = "black", fill = "white") +
  facet_grid(groups ~ .)
ggqqplot(high_df$Rowmeans)
ggqqplot(low_df$Rowmeans)
hist(low_df$Rowmeans)

shapiro.test(high_df$Rowmeans)
shapiro.test(low_df$Rowmeans)
var.test(rowmeans ~ groups, data = plot_df)

# Test if there is a statistical difference between the mean responses of respondents on LED pairs versus HED pairs
t.test(high_df$Rowmeans, low_df$Rowmeans, var.equal = TRUE)

# Because their is no statistical difference we need to do a power analysis
M1 <- mean(high_df$Rowmeans)
M2 <- mean(low_df$Rowmeans)
S1 <- sd(high_df$Rowmeans)
S2 <- sd(low_df$Rowmeans)
d <- (M1 - M2)/sqrt(((S1^2) + (S2^2))/2)

# Below wer find out how many participants we need to get to get sufficient power and significance level in our experiment
pwr.t.test(n = 21  , d = NULL , sig.level =0.05 , power = 0.8 , type = "two.sample")
