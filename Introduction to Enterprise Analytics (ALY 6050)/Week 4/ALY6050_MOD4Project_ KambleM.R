cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Week - 4<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Module Four Project
# A Prescriptive Model for Strategic Decision-making, An Inventory Management Decision Model

# Part - 1
library(dplyr)
library(ggplot2)

# Data
annualdemand <- 19000
unitcost <- 123
holdper <- 0.135
holdcost <- unitcost*holdper
ordercost <- 159

# EOQ
eoq <- round(sqrt((2*annualdemand*ordercost)/holdcost),0)


# 1.5 times order quantity
nextorder <- 1.5*eoq


nfo <- round(annualdemand/nextorder,0)


ordering_cost <- ordercost * nfo


holdingcost <- eoq * holdcost


total <- ordering_cost + holdingcost


r_results <- data.frame(
  EOQ = eoq,
  Next_Order = nextorder,
  Number_of_Orders = nfo,
  Ordering_Cost = ordering_cost,
  Holding_Cost = holdingcost,
  Total_Cost = total
)


# Simulation of Inventory, 1.5 Times Inventory to obtain Minimum Total Cost

inventory <- seq(100,10000,by=20)


inventory_new <- inventory*1.5


totalcost <- (ordercost*(annualdemand/inventory_new))+(holdcost*inventory)

cost <- data.frame(inventory, inventory_new, totalcost)

head(cost)

which.min(cost$totalcost)
cost$inventory[13]  



# Plot of Total Cost Vs Inventory
ggplot(cost, aes(x = inventory, y = totalcost)) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Total Cost vs. Inventory",
    x = "Inventory",
    y = "Total Cost") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# PART - 2

#Given Data 
min <- 15000
max <- 23000
mode <- 19000

set.seed(2024)
rand <- runif(3000)
rand <- round(rand,2)


options(scipen = 3000)

K <- (mode-min)/(max-min)

M <- (max-min)*(mode-min)


N <- (max-min)*(max-mode)


x <- min+sqrt(rand*M)

y <- max-sqrt((1-rand)*N)


annualdemand_sim <- round(ifelse(x<=K,x,y),0)
summary(annualdemand_sim)
round(sd(annualdemand_sim),0)

# Inventory Level
inventory_sim <- round(sqrt((2*annualdemand_sim*ordercost)/holdcost),0)
summary(inventory_sim)

inventory_new_sim <- 1.5 * inventory_sim

order_sim <- round(annualdemand_sim/inventory_sim,0)
summary(order_sim)

ordercost_sim <- (ordercost*annualdemand_sim)/inventory_new_sim

holdcost_sim <- inventory_sim*holdcost

totalcost_sim <- ordercost_sim + holdcost_sim

min(totalcost_sim)
mean(totalcost_sim)

simulated_data <- data.frame(inventory_sim,order_sim,totalcost_sim)

# expected minimum total cost
t.test(simulated_data$totalcost_sim,conf.level = 0.9)

# expected order quantity
t.test(simulated_data$inventory_sim,conf.level = 0.9)

# expected annual number of orders
t.test(simulated_data$order_sim, conf.level = 0.9)


































