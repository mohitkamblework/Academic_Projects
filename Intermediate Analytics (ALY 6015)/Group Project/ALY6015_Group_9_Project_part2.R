cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session




# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Group Project<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Part 2

library(dplyr)
library(tidyverse)


boston <- read.csv("D:/NEU STUDY/2nd Quarter/Intermediate Analytics (ALY 6015)/Group Project/fy19fullpropassess.csv")

# Data Overview
str(boston)
summary(boston)


# 1) 
# Subset of street name BEACON
subset_beacon <- subset(boston, ST_NAME == "BEACON")
str(subset_beacon)
summary(subset_beacon)

# Subset of year 1990
subset_1990 <- subset(boston, YR_BUILT == 1990)
str(subset_1990)
summary(subset_1990)

# Subset of land used cd
subset_cd <- subset(boston, LU == "CD")
str(subset_cd)
summary(subset_cd)


# 2)
# New Ratio variable of AV_LAND/AV_TOTAL
boston$landtotal_ratio <- round((boston$AV_LAND / boston$AV_TOTAL), 2)

# New Ratio variable of AV_BLDG/AV_TOTAL
boston$bldgtotal_ratio <- round((boston$AV_BLDG / boston$AV_TOTAL), 2)


names(boston)
view(boston)


# 3) didn't merge the cpi, population data with the boston checkbook dataset

# 4)

# >>>>>>>>>>>>>>>>>Corelation<<<<<<<<<<<<
library(corrplot)

# Replace missing values with the calculated mean
mean_yr_remod <- mean(boston$YR_REMOD, na.rm = TRUE)
boston$YR_REMOD[is.na(boston$YR_REMOD)] <- mean_yr_remod

selected_columns <- boston[c("AV_LAND",	"AV_BLDG",	"AV_TOTAL", "GROSS_TAX")]

# correlation matrix
cor_matrix <- cor(selected_columns)


# correlation plot
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.9, tl.col = "black", addCoef.col = "orange")

# Av_total has a positive corelation with AV_land
# Av_total has a strong positive corelation with AV_bldg
# Gross_tax has a weak positive corelation with AV_bldg
# Gross_tax has a similar corelation with AV_bldg and AV_total


# >>>>>>>>>>>>>>>>>>>>>Regression Table<<<<<<<<<<<<<<<<<<<

# Predicting AV_TOTAL based on LAND_SF and GROSS_AREA
mod1 <- lm(AV_TOTAL ~ LAND_SF + GROSS_AREA, data = boston)

summary(mod1)

install.packages("stargazer")
library(stargazer)

stargazer(mod1, title = " Multiple Linear Regression Results", type = "text")

mod1$p.value


# >>>>>>>>>>>>>>>>>>>>>ANOVA<<<<<<<<<<<<<<<<<<<<<
# Comparing the mean AV_TOTAL across different levels of a categorical variable 'LU'
anova_model <- aov(AV_TOTAL ~ LU, data = dataset)

mod2 <- aov(AV_TOTAL ~ LU, data = boston)

summary(mod2)

mod2$p.value



# Chi-square Test

contgncy_table <- table(boston$PTYPE, boston$OWN_OCC)

mod3 <- chisq.test(contgncy_table)

summary(mod3)

mod3$p.value

















