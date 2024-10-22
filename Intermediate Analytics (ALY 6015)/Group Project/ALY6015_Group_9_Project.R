cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session




# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Group Project<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

boston <- read.csv("D:/NEU STUDY/2nd Quarter/Intermediate Analytics (ALY 6015)/Group Project/fy19fullpropassess.csv")

# Data Overview
str(boston)
summary(boston)

# Univariate Analysis
hist(boston$AV_TOTAL, main = "Distribution of AV_TOTAL", xlab = "Total Assessed Value")
barplot(table(boston$OWN_OCC), main = "Distribution of OWN_OCC", xlab = "Owner Occupied (Y/N)")

# Descriptive Statistics for All Sample
summary(boston[c("AV_TOTAL", "GROSS_AREA", "YR_BUILT", "R_BDRMS")])

# Descriptive Statistics by Property Type (LU)
for (property_type in unique(boston$LU)) {
  subset_data <- subset(boston, LU == property_type)
  print(paste("Descriptive Statistics for", property_type))
  print(summary(subset_data[c("AV_TOTAL", "GROSS_AREA", "YR_BUILT", "R_BDRMS")]))
}


# Descriptive Statistics by Bedroom Count
for (bedroom_count in unique(boston$R_BDRMS)) {
  subset_data <- subset(boston, R_BDRMS == bedroom_count)
  print(paste("Descriptive Statistics for", bedroom_count, "Bedrooms"))
  print(summary(subset_data[c("AV_TOTAL", "GROSS_AREA", "YR_BUILT")]))
}

# Analysis
# Chi-Square Test
chi_square_result <- chisq.test(table(boston$LU, boston$OWN_OCC))
print(chi_square_result)

# ANOVA Test
anova_result <- aov(AV_TOTAL ~ LU, data = boston)
print(summary(anova_result))

# ANOVA Test for Sub-Groups
for (bedroom_count in unique(boston$R_BDRMS)) {
  subset_data <- subset(boston, R_BDRMS == bedroom_count)
  
  # Check if the subset has at least two levels for LU
  if (length(unique(subset_data$LU)) >= 2) {
    anova_result <- aov(AV_TOTAL ~ LU, data = subset_data)
    print(paste("ANOVA Results for", bedroom_count, "Bedrooms:"))
    print(summary(anova_result))
  } else {
    print(paste("Insufficient levels for ANOVA in subset with", bedroom_count, "Bedrooms."))
  }
}

