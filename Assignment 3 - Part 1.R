setwd("D:/Documenten/Artificial Intelligence Master/Semester 1/Experimentation in Psychology and Linguistics/Assignment 3")
library(readr)
library(tidyverse)
df <- read_csv("results-survey516897.csv")
df1 <- subset(df, `lastpage. Last page` > 59)
df1 <- subset(df1, `submitdate. Date submitted` > 2015)
df1$`id. Response ID`
r7 = t(df1[8:67])
hist(r7)
summary(r7)
sd(r7)
shapiro.test(r7) # if significant the data is not normally distributed

for (val in c(1:30))
{
  cat('Respondent',df1$`id. Response ID`[val])
  r = t(df1[val, 8:67])
  print(summary(r))
  print(sd(r))
  print(shapiro.test(r))
}

r32 <- df1 %>% subset(`id. Response ID` == "32")
r32 <- t(r32[,8:67])
wilcox.test(x = r7, y = r32, paired = F)
plot(r7, r32)
abline(lm( r7 ~ r32))


cor(x = r7, y = r32, method = "pearson")
cor(x = r7, y = r32, method = "spearman")

i1 = df1$`n08[A1]. How similar are these words in meaning? [daughter vs. kid]`
i2 = df1$`n25[SQ001]. How similar are these words in meaning? [noise vs. rattle]`
plot(i1, i2)
abline(lm(i1 ~ i2))

cor(x = i1, y=i2, method='pearson')

rsp1 = df1$`n08Time. Question time: n08`
rsp2 = df1$`n25Time. Question time: n25`
plot(rsp1, rsp2)
abline(lm(rsp1 ~ rsp2))
cor(x = i1, y=i2, method='pearson')

iA1 <- df1[8:24]
iA1$rowmeans <- rowMeans(iA1)
iSQ00 <- df1[25:67]
iSQ00$rowmeans <- rowMeans(iSQ00)

plot(iA1$rowmeans, iSQ00$rowmeans)
abline(lm(iA1$rowmeans ~ iSQ00$rowmeans))
cor(x = iA1$rowmeans, y=iSQ00$rowmeans, method='pearson')
