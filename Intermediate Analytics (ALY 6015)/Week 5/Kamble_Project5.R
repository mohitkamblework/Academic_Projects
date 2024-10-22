cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Week - 5<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



# Section 13-2
# 6 - Game Attendance

#Stating the Hypothesis
#H0: median = 3000
#H1: median =! 3000

# median
median = 3000

#Significance level
alpha = 0.05

fig <- c(6210,3150,2700,3012,4875,3540,6127,2581,2642,2573,2792,2800,2500,3700,6030,
             5437,2758,3490,2851,2720)

guide <- fig-median

#positive
p <- length(guide[guide>0])

#negative
n <- length(guide[guide<0])

result<- binom.test(x= c(p,n), alternative = "two.sided")

result$p.value

ifelse(result$p.value>alpha,"Failed to reject the null hypothesis","Rejected the null hypothesis")

# 10 - Lottery Ticket Sales

#Stating the hypothesis
#H0: Sells 200 lottery t/day
#H1: sells < 200 lottery t/day

#Significance level
alpha=0.05

sample <- 40
db_200 <- 15

result <- wilcox.test(x=sample, y=db_200, alternative = "two.sided", correct = FALSE)

test_value <- sum(1:db_200)

# Performing Wilcoxon rank sum test
result <- wilcox.test(x = rep(200, sample), y = c(rep(200, sample - db_200), seq(1, db_200)), exact = FALSE, correct = TRUE)

result$p.value

ifelse(result$p.value>alpha,"Failed to reject the null hypothesis","Rejected the null hypothesis")



# Section 13-3
# 4 - Lengths of Prison Sentences

#Stating the hypothesis
#H0: No difference in sentence received
#H1: Difference in sentence received
alpha=0.05

m <- c(8,12,6,14,22,27,32,24,26,19,15,13)
f <- c(7,5,2,3,21,26,30,9,4,17,23,12,11,16)

result <- wilcox.test(x=m, y=f)

result$p.value

if (result$p.value < alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}



# 8 - Winning Baseball Games

#Stating the Hypothesis
#H0: Difference between wins
#H1: No difference between wins

alpha=0.05

nl <- c(89,96,88,101,90,91,92,96,108,100,95)
al <- c(108,86,91,97,100,102,95,104,95,89,88,101)

result <- wilcox.test(x= nl, y= al)

result$p.value

if (result$p.value > alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}


# Section 13-4
# 5)
#Stating the Hypothesis

#H0: The two groups have the same median.
#H1: The two groups do not have the same median.

alpha = 0.01

res5 <- kruskal.test(list(ws = c(13), n = c(15)))

res5$p.value

if (res5$p.value > alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}

# 6)
#Stating the Hypothesis

#H0: Second group = median as or Median < the first group median.
#H1: Second group median > first group median.

alpha = 0.025

res6 <- kruskal.test(list(ws = c(32), n = c(28)))

res6$p.value

if (res6$p.value > alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}

# 7)
#Stating the Hypothesis

#H0: Second group = median as or median < first group median.

#H1: Second group median > first group median.

alpha = 0.05

res7 <- kruskal.test(list(ws = c(65), n = c(20)))

res7$p.value

if (res7$p.value > alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}


# 8)
#Stating the Hypothesis

#H0: Both the groups have = median.
#H1: Both the groups different median.

alpha=0.10

res8 <- kruskal.test(list(ws = c(22), n = c(14)))

res8$p.value

if (res8$p.value > alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}


# Section 13-5
# 2 - Mathematics Literacy Scores

#Stating the Hypothesis

#H0: No difference in means
#H1: Difference in means

alpha = 0.05

wh <- data.frame( scores =c(527, 406, 474, 381, 411), group = rep("wh",5))
eu <- data.frame( scores = c(520, 510, 513, 548, 496), group = rep("eu",5))
ea <- data.frame( scores = c(523, 547, 547, 391, 549) , group = rep("ea",5))

data <- rbind(wh,eu,ea)

result <- kruskal.test(scores ~ group, data = data)

result$p.value

if (result$p.value < alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}


# Section 13-6
# Subway and Commuter p Passengers

#Stating the Hypothesis

#H0: No correlation
#H1: There is a correlation

alpha = 0.05

c <- c(1,2,3,4,5,6)
s <- c(845, 494, 425, 313, 108, 41)
r <- c(39, 291, 142, 103, 33, 38)

data <-data.frame(c=c, s=s, r=r)

result <- cor.test(x= data$s, y= data$r, method="spearman")

if (result$p.value < alpha) {
  cat("\nRejected the null hypothesis")
} else {
  cat("\nFailed to reject the null hypothesis")
}

# Section 14-3
# 16 - Prizes in Caramel Corn Boxes

sim_exp <- function() {
  prizes <- c("Prize A", "Prize B", "Prize C", "Prize D") 
  boxes <- c() 
  num_boxes <- 0 
  while(length(unique(boxes)) < length(prizes)) {
    prize <- sample(prizes, 1)
    boxes <- c(boxes, prize)
    num_boxes <- num_boxes + 1  
  }
  return(num_boxes)  
}

numtr <- 40  

results <- replicate(numtr, sim_exp())

avg_b <- mean(results)


# 18 - Lottery Winner

sim_exp <- function() {
  l <- c("b", "i", "g")
  t <- c()  
  while(TRUE) {
    letter <- sample(l, 1, prob = c(0.6, 0.3, 0.1), replace = TRUE)
    t <- c(t, letter)
    if (length(t) >= 3 && paste(t[(length(t)-2):length(t)], collapse = "") == "big") {
      break 
    }
  }
  return(length(t))  
}


numtr <- 30  

results <- replicate(numtr, sim_exp())

avg_t <- mean(results)


