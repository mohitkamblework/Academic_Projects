cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Week - 2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Chi-Square testing
# Section 11-1
# 6. Blood Types

#     Expected  |  Observed
# A       20%   |     12
# B       28%   |     8
# 0       36%   |     24  
# AB      16%   |     6  

# Ho: A=0.2, B=0.28, O=0.36, AB=0.16
# H1: The distribution is not the same as the null hypothesis.

# Setting the significance level
alpha <- 0.10

# Creating a vector for values
observed <- c(12,8,24,6)

# Creating a vector for probabilities
p <- c(0.2, 0.28, 0.36, 0.16)

# Running the Chi-Square Test
result <- chisq.test(x = observed, p = p)

# Making a decision by comparing the p-value
ifelse(result$p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

###############################################################################################################

# Section 11-1
# 8. On-Time Performance by Airlines

#       Expected  |  Observed
# OT      70.8%   |     125
# NASD    8.2%    |     10
# AAL     9%      |     25  
# O       12%     |     40  

# Ho: OT=0.708, NASD=0.082, AAL=0.09, 0=0.12
# H1: The distribution is not the same as the null hypothesis.

# Setting the significance level
alpha <- 0.05

# Creating a vector for values
observed <- c(125,10,25,40)

# Creating a vector for probabilities
p <- c(0.708, 0.082, 0.09, 0.12)

# Running the Chi-Square Test
result <- chisq.test(x = observed, p = p)

# Making a decision by comparing the p-value
ifelse(result$p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

###############################################################################################################

# Section 11-2
# 8. Ethnicity and Movie Admissions


#     Caucasian   Hispanic    African American    Other
# 2013    724         335             174           107
# 2014    370         292             152           140

# Ho: Admissions are independent to ethnicity
# H1: Admissions are dependent to ethnicity


# Setting the significance level
alpha <- 0.05

# Creating a vector for each row
r1 <- c(724, 335, 174, 107)
r2 <- c(370, 292, 152, 140)

# Stating the number of rows for matrix
rows = 2

# Creating a matrix from the rows
mtrx <- matrix(c(r1,r2), nrow = rows, byrow = TRUE)

# Naming the rows and columns
rownames(mtrx) = c("2013", "2014")
colnames(mtrx) = c("Caucasian", "Hispanic", "African American", "Other")

# Viewing the matrix
mtrx

# Running the Chi-Square Test
result <- chisq.test(mtrx)

summary(result)

result$p.value

# Making a decision by comparing the p-value
ifelse(result$p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

###############################################################################################################

# Section 11-2
# 10. Women in the Military

#               Officers    Enlisted
# Army          10,791      62,491
# Navy          7,816       42,750
# Marine Corps  932         9,525
# Air Force     11,819      54,344


# Ho: There is no relationship between rank and branch of the Armed Forces (Chi-square = 0)
# H1: There is a relationship between rank and branch of the Armed Forces (Chi-square != 0)

# Setting the significance level
alpha <- 0.05

# Creating a vector for each row
r1 <- c(10791, 62491)
r2 <- c(7816, 42750)
r3 <- c(932, 9525)
r4 <- c(11819, 54344)

# Stating the number of rows for matrix
rows = 4

# Creating a matrix from the rows
mtrx <- matrix(c(r1,r2,r3,r4), nrow = rows, byrow = TRUE)

# Naming the rows and columns
rownames(mtrx) = c("Army", "Navy", "Marine Corps", "Air Force")
colnames(mtrx) = c("Officers", "Enlisted")

# Viewing the matrix
mtrx

# Running the Chi-Square Test
result <- chisq.test(mtrx)

# Making a decision by comparing the p-value
ifelse(result$p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

###############################################################################################################

# Section 12-1
# 8. Sodium Contents of Foods

# Condiments  Cereals Desserts
#   270         260     100
#   130         220     180
#   230         290     250
#   180         290     250
#   80          200     300
#   70          320     360
#   200         140     300
#                       160

# Ho: u1=u2=u3
# H1: At least one mean is different form others.

# Setting the significance level
alpha <- 0.05

# Creating a dataframe for condiments
condiments <- data.frame('sodium' = c(270,130,230,180,80,70,200), 'food' = rep('condiments', 7), stringsAsFactors = FALSE)

# Creating a dataframe for cereals
cereals <- data.frame('sodium' = c(260,220,290,290,200,320,140), 'food' = rep('cereals', 7), stringsAsFactors = FALSE)

# Creating a dataframe for desserts
desserts <- data.frame('sodium' = c(100,180,250,250,300,360,300,160), 'food' = rep('desserts', 8), stringsAsFactors = FALSE)

# Combining the dataframes into one
sodium <- rbind(condiments, cereals, desserts)
sodium$food <- as.factor(sodium$food)

# Creating a contingency table
ct <- table(sodium$food, cut(sodium$sodium, breaks = c(0, 150, 250, 400)))

# Running the Chi-Square test
result <- chisq.test(ct)

# Extracting the summary
summary(result)

# Extracting the p-value
p_value <- result$p.value

# Making a decision by comparing the p-value for Chi-Square Test
ifelse(p_value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

###############################################################################################################

# ANOVA Test
# Section 12-2
# 10. Sales for Leading Companies

# Cereals     Chocolate Candy   Coffee
#   578             311           261
#   320             106           185
#   264             109           302
#   249             125           689
#   237             173     

# Ho: u1=u2=u3
# H1: Atleast one mean is different form others.

# Setting the significance level
alpha <- 0.01

# Creating a dataframe for cereal
cereal <- data.frame('sales' = c(578,320,264,249,237), 'food' = rep('cereal', 5), stringsAsFactors = FALSE)

# Creating a dataframe for chocolate candy
ch_candy <- data.frame('sales' = c(311,106,109,125,173), 'food' = rep('ch_candy', 5), stringsAsFactors = FALSE)

# Creating a dataframe for coffee
coffee <- data.frame('sales' = c(261,185,302,689), 'food' = rep('coffee', 4), stringsAsFactors = FALSE)

# Combining the dataframes into one
sales <- rbind(cereal, ch_candy, coffee)
sales$food <- as.factor(sales$food)

# Running the ANOVA test
anova <- aov(sales ~ food, data = sales)

# Extracting the summary
summary(anova)

# Extracting the p-value
a.summary <- summary(anova)
p.value <- a.summary[[1]][[1,"Pr(>F)"]]


# Making a decision by comparing the p-value
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Seeing the difference
TukeyHSD(anova)

###############################################################################################################

# Section 12-2
# 12. Per-Pupil Expenditures

# Eastern third   Middle third   Western third
#   4946            6149          5282
#   5953            7451          8605
#   6202            6000          6528
#   7243            6479          6911
#   6113

# Ho: u1=u2=u3
# H1: Atleast one mean is different form others.

# Setting the significance level
alpha <- 0.05

# Creating a dataframe for eastern third
et <- data.frame('exp' = c(4946,5953,6202,7242, 6113), 'sec' = rep('et', 5), stringsAsFactors = FALSE)

# Creating a dataframe for chocolate candy
mt <- data.frame('exp' = c(6149,7451,6000,6479), 'sec' = rep('mt', 4), stringsAsFactors = FALSE)

# Creating a dataframe for coffee
wt <- data.frame('exp' = c(5282,8605,6528,6911), 'sec' = rep('wt', 4), stringsAsFactors = FALSE)

# Combining the dataframes into one
exp <- rbind(et, mt, wt)
exp$sec <- as.factor(exp$sec)

# Running the ANOVA test
anova <- aov(exp ~ sec, data = exp)

# Extracting the summary
summary(anova)

# Extracting the p-value
a.summary <- summary(anova)
p.value <- a.summary[[1]][[1,"Pr(>F)"]]


# Making a decision by comparing the p-value
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Seeing the difference
TukeyHSD(anova)


###############################################################################################################

# Two-way ANOVA Test
# Section 12-3
# 10. Increasing Plant Growth

#                Grow-light 1    Grow-light 2
# Plant food A   9.2, 9.4, 8.9   8.5, 9.2, 8.9
# Plant food B   7.1, 7.2, 8.5   5.5, 5.8, 7.6

# Null-Hypothesis:
# Ho: There is no interaction between plant food and grow light
# H1: There is no difference in mean growth w.r.t. grow light
# H2: There is no difference in mean growth w.r.t. plant food

# Alternative-Hypothesis:
# Ha:  There is a significant interaction between plant food and grow light
# Ha1: There is a difference in mean growth w.r.t. grow light
# Ha2: There is a difference in mean growth w.r.t. plant food


# Setting the significance level
alpha <- 0.05

# Setting-up the data for growth light
gl <- factor(rep(c("Grow-light 1", "Grow-light 2"), each = 6))

# Setting-up the data for plant food
pf <- factor(rep(c("Plant food A", "Plant food B"), times = 6))

# Setting-up the data for growth
gro <- c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9, 7.1, 7.2, 8.5, 5.5, 5.8, 7.6)

# Creating a dataframe
plnt <- data.frame(gl, pf, gro)

# Running the Two-way ANOVA test
tw_anova <- aov(gro ~ gl*pf, data = plnt)

# Extracting the summary
summary(tw_anova)

# Extracting the p-value
a.summary <- summary(tw_anova)
p.value <- a.summary[[1]][[1,"Pr(>F)"]]

# Making a decision by comparing the p-value
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Seeing the difference
TukeyHSD(tw_anova)

# Result: There is a difference in mean growth w.r.t. grow light

###############################################################################################################
# Baseball Dataset

bb <- read.csv("D:/NEU STUDY/2nd Quarter/Intermediate Analytics (ALY 6015)/Week 2/baseball.csv")

names(bb)
head(bb)
View(bb)


library(dplyr)
library(tidyverse)

# Extract decade from year
bb$Decade <- bb$Year - (bb$Year %% 10)


# Create a wins table by summing the wins by decade
wins <- bb %>%
  group_by(Decade) %>%
  summarize(wins = sum(W)) %>%
  as.tibble()

# Extracting summary
summary(bb_clear)

# Correlation matrix
cor_matrix <- round(cor(bb[, c("RS", "RA", "W", "OBP", "SLG", "BA", "G")]),2)

library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)

# Histogram of RS
hist(bb$RS, main="Histogram of RS", xlab="RS", col="seagreen", border="black")


# Chi_square Test (Goodness-of-fit)

# Ho: There is no significance difference in the no. of wins by decade
# H1: There is a significance difference in the no. of wins by decade

# Setting the significance level
alpha <- 0.05

# Finding the critical value from the standard normal distribution
cv <- qnorm(1 - alpha/2)

# Running Chi-Square Test
result <- chisq.test(wins)

# Extracting the summary
summary(result)

# Extracting the p-value
p_value <- result$p.value

# Making a decision by comparing the p-value for Chi-Square Test
ifelse(p_value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

###############################################################################################################

# Crop Dataset
crop <- read.csv("D:/NEU STUDY/2nd Quarter/Intermediate Analytics (ALY 6015)/Week 2/crop_data.csv")

names(crop)
head(crop)
View(crop)


# Two-way ANOVA Test


# Null-Hypothesis:
# Ho: There is no interaction between fertilizer and plant density
# H1: There is no difference in mean yield with respect to fertilizer
# H2: There is no difference in mean yield with respect to plant density

# Alternative-Hypothesis:
# Ha: There is a significant interaction between fertilizer and plant density
# Ha1: There is a difference in mean yield with respect to fertilizer
# Ha2: There is a difference in mean yield with respect to plant density

# Setting the significance level
alpha <- 0.05

# Converting the variables density, fertilizer and block to R factors
crop$density <- as.factor(crop$density)
crop$fertilizer <- as.factor(crop$fertilizer)
crop$block <- as.factor(crop$block)


# Running the Two-way ANOVA test
tw_anova_crop <- aov(yield ~ fertilizer*density, data = crop)

# Extracting the summary
summary(tw_anova_crop)

# Extracting the p-value
a.summary <- summary(tw_anova_crop)
p.value <- a.summary[[1]][[1,"Pr(>F)"]]

# Making a decision by comparing the p-value
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Seeing the difference
TukeyHSD(tw_anova_crop)







