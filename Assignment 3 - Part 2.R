# Load the proper libraries and set the right working directory
setwd("D:/Documenten/Artificial Intelligence Master/Semester 1/Experimentation in Psychology and Linguistics/Assignment 3")
library(readr)
library(tidyverse)
library(lme4)
# Load the proper dataset using a read_delimiter function and set it to tab separations
news_df_all <- read_delim("newsurprisal.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)

# Change the types of the next from character(string) to factor(categorical)
news_df_all$artpres <- as.factor(news_df_all$artpres)
news_df_all$noun <- as.factor(news_df_all$noun)
news_df_all$verb <- as.factor(news_df_all$verb)

# Create the models that are asked for in the assignment
o <- glmer(artpres ~ (1|noun), data = news_df_all, family = binomial)
s <- glmer(artpres ~ (1|noun) + (1|verb), data = news_df_all, family = binomial)
r <- glmer(artpres ~ surprisal + (1|noun) + (1|verb), data = news_df_all, family = binomial)

# Inspect the models using the summary functions
summary(o)
summary(s)
summary(r)

# Analyse the different models using the anova functions
anova(o,s,r)
