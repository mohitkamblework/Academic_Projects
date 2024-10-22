cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Week - 1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Q1)
ames <- read.csv("D:/NEU STUDY/2nd Quarter/Intermediate Analytics (ALY 6015)/Week 1/AmesHousing.csv")

# Q2)
head(ames)
names(ames)
View(ames)
summary(ames)
str(ames)


# Q3)
library(dplyr)

# Imputing missing values with mean
ames <- ames %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Q4)
# Continuous numeric variables of interest
contd_vars <- ames[c("SalePrice", "Lot.Frontage", "Lot.Area", "Pool.Area", "Gr.Liv.Area",
                     "Wood.Deck.SF", "Open.Porch.SF", "Enclosed.Porch")]

cm <- cor(contd_vars)

# Q5)
library(corrplot)
corrplot(cm, method = "color", type = "upper", tl.cex = 0.9, tl.col = "black",
         addCoef.col = "black", title = "Correlation plot")  



# Q6)
library(ggplot2)

ggplot(ames, aes(x = SalePrice, y = Gr.Liv.Area, color = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot: Sale Price vs Ground Living Area",
       x = "Sale Price",
       y = "Ground Living Area")

ggplot(ames, aes(x = SalePrice, y = Pool.Area, color = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot: Sale Price  vs Pool Area",
       x = "Sale Price",
       y = "Pool Area")

ggplot(ames, aes(x = SalePrice, y = Lot.Frontage, color = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot: Sale Price vs Lot Frontage",
       x = "Sale Price",
       y = "Lot Frontage")



# Q7)
reg_mod <- lm(SalePrice ~ BsmtFin.SF.1 + BsmtFin.SF.2 + Bsmt.Unf.SF, data = ames)

# Q8)
summary(reg_mod)


# Q9)
install.packages("viridis")
library(viridis)

col_palette <- viridis(1)
par(mfrow = c(2, 2))
plot(reg_mod, col = col_palette)

# Q10)
install.packages("car")
library(car)

vif(reg_mod)

# Q11)
ot <- outlierTest(reg_mod)


# Q12)
row_del <- c(1499,2181,1768,1761,2451,434,2446,2331,2667,2333)
ames <- ames[-row_del, ]
reg_mod <- lm(SalePrice ~ BsmtFin.SF.1 + BsmtFin.SF.2 + Bsmt.Unf.SF, data = ames)
summary(reg_mod)


# Q13)
install.packages('leaps')
library(leaps)
mod_sub = regsubsets(SalePrice ~ BsmtFin.SF.1 + BsmtFin.SF.2 + Bsmt.Unf.SF, data = ames,
                           nbest = 3)
summary(mod_sub)






































