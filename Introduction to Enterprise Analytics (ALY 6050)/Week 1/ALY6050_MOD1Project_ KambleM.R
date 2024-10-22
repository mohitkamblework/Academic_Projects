cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Week - 1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Module One Project
# Project: Analysis of a Betting Strategy in Sports

# Part - 1
# i)
# Defining the probabilities
p_celtics_home = 0.60  # Probability Celtics win at home
p_warriors_home = 0.58  # Probability Warriors win at home

# Defining the win/loss values
win_amount = 150
loss_amount = -160

# A function to calculate probability of winning a game
win_prob <- function(home_team, p_home) {
  if (home_team == "Celtics") {
    return(p_home)
  } else {
    return(1 - p_home)
  }
}

# Calculating the probabilities for each scenario:

# Celtics win first game (Warriors home)
celtics_win_1 = win_prob("Warriors", p_warriors_home) * p_celtics_home

# Warriors win first game (Warriors home)
warriors_win_1 = (1 - win_prob("Warriors", p_warriors_home)) * 1

# Celtics win the series in two games (considering only Celtics win first game)
celtics_win_series_2g = celtics_win_1

# Warriors win first game, Celtics win second game (Warriors home, Celtics home)
warriors_win_1_celtics_win_2 = (1 - win_prob("Warriors", p_warriors_home)) * win_prob("Celtics", p_celtics_home)

# Celtics win the series in three games (considering only Warriors win first game and Celtics win second game)
celtics_win_series_3g = warriors_win_1_celtics_win_2 * win_prob("Warriors", p_warriors_home)

# Combined probabilities for Celtics winning the series
celtics_win_series = celtics_win_series_2g + celtics_win_series_3g

# The probability
cat("Probability that Celtics win the series:", celtics_win_series)


# ii)
# Calculating the net win for each scenario
net_win_2g = 2 * win_amount  # Win both games
net_win_3g = win_amount - loss_amount  # Win 2nd and 3rd game

# Calculating the probability for each net win scenario
prob_2g = celtics_win_series_2g
prob_3g = celtics_win_series_3g

# Calculating the expected net win (weighted average)
expected_win <- (prob_2g * net_win_2g) + (prob_3g * net_win_3g)

# Calculating the squared deviations from the mean
sq_dev_2g <- (net_win_2g - expected_win)^2 * prob_2g
sq_dev_3g <- (net_win_3g - expected_win)^2 * prob_3g

# Calculating the variance (sum of squared deviations)
variance <- sq_dev_2g + sq_dev_3g

# Calculating the standard deviation (square root of variance)
standard_deviation <- sqrt(variance)

# Results
cat("Expected net win:", expected_win, "\n")
cat("Standard deviation of net win:", standard_deviation)



# iii)
# Simulating the net win scenarios (5,000 samples)
set.seed(123)  # Setting random number seed for reproducibility
simulated_wins <- sample(c(net_win_2g, net_win_3g), size = 5000, replace = TRUE, prob = c(prob_2g, prob_3g))

# Calculating the estimated expected net win using the sample mean
estimated_expected_win <- mean(simulated_wins)

# Calculating the confidence interval (95%)
alpha <- 0.05  # Significance level (1 - confidence level)
confidence_level <- 1 - alpha
z_value <- qnorm(1 - alpha/2)  # Quantile of standard normal distribution

# Calculating confidence interval bounds
lower_bound <- estimated_expected_win - z_value * sd(simulated_wins)
upper_bound <- estimated_expected_win + z_value * sd(simulated_wins)

# Results
cat("Estimated expected net win (simulation):", estimated_expected_win, "\n")
cat("95% confidence interval:", lower_bound, "-", upper_bound, "\n")

# Checking if the E(X) is within the confidence interval
if (expected_win >= lower_bound & expected_win <= upper_bound) {
  cat("Yes, the E(X) is within the 95% confidence interval.")
} else {
  cat("No, the E(X) is not within the 95% confidence interval.")
}



# iv)
library(base)


# Creating frequency table for simulated net win (Y)
eps <- .Machine$double.eps  # Using the machine epsilon to avoid empty bins
breaks <- c(-Inf, net_win_2g - eps, net_win_3g + eps, Inf)  
simulated_data_counts <- table(cut(simulated_wins, breaks = breaks, labels = FALSE))

# Expected counts for each bin (based on original probabilities)
expected_counts <- c(prob_2g * 5000, prob_3g * 5000)

# Performing the Chi-square goodness of fit test
chisq_test <- chisq.test(x = simulated_data_counts)

# Results
cat("Chi-square test statistic:", chisq_test$statistic, "\n")
cat("p-value:", chisq_test$p.value, "\n")

# Interpreting the results
if (chisq_test$p.value > 0.05) {
  cat("We fail to reject the null hypothesis that the simulated data (Y) follows the expected distribution (X).")
} else {
  cat("We reject the null hypothesis that the simulated data (Y) follows the expected distribution (X).")
}



# v)
# Mentioned in report**



# Part - 2
# i)
# Updating the probabilities for the new scenario
p_celtics_home_part2 = 0.60  # Probability Celtics win at home in Part 2
p_warriors_home_part2 = 0.58  # Probability Warriors win at home in Part 2

# Calculating probabilities for each scenario in Part 2:

# Celtics win first game (Celtics home)
celtics_win_1_part2 = win_prob("Celtics", p_celtics_home_part2) * (1 - p_warriors_home_part2)

# Warriors win first game (Celtics home)
warriors_win_1_part2 = (1 - win_prob("Celtics", p_celtics_home_part2)) * 1

# Celtics win the series in two games (considering only Celtics win first game)
celtics_win_series_2g_part2 = celtics_win_1_part2

# Warriors win first game, Celtics win second game (Celtics home, Warriors home)
warriors_win_1_celtics_win_2_part2 = (1 - win_prob("Celtics", p_celtics_home_part2)) * win_prob("Warriors", p_warriors_home_part2)

# Celtics win the series in three games (considering only Warriors win first game and Celtics win second game)
celtics_win_series_3g_part2 = warriors_win_1_celtics_win_2_part2 * win_prob("Celtics", p_celtics_home_part2)

# Combined probabilities for Celtics winning the series in Part 2
celtics_win_series_part2 = celtics_win_series_2g_part2 + celtics_win_series_3g_part2

# The probability
cat("Probability that Celtics win the series in Part 2:", celtics_win_series_part2, "\n")


# ii)
# Calculating the net win for each scenario in Part 2
net_win_2g_part2 = win_amount - loss_amount  # Win both games in Part 2
net_win_3g_part2 = 2 * win_amount  # Win 1st and 3rd game in Part 2

# Calculating the probability for each net win scenario in Part 2
prob_2g_part2 = celtics_win_series_2g_part2
prob_3g_part2 = celtics_win_series_3g_part2

# Calculating the expected net win (weighted average) for Part 2
expected_win_part2 <- (prob_2g_part2 * net_win_2g_part2) + (prob_3g_part2 * net_win_3g_part2)

# Calculating the squared deviations from the mean for Part 2
sq_dev_2g_part2 <- (net_win_2g_part2 - expected_win_part2)^2 * prob_2g_part2
sq_dev_3g_part2 <- (net_win_3g_part2 - expected_win_part2)^2 * prob_3g_part2

# Calculating the variance (sum of squared deviations) for Part 2
variance_part2 <- sq_dev_2g_part2 + sq_dev_3g_part2

# Calculating the standard deviation (square root of variance) for Part 2
standard_deviation_part2 <- sqrt(variance_part2)

# Results for Part 2
cat("Expected net win in Part 2:", expected_win_part2, "\n")
cat("Standard deviation of net win in Part 2:", standard_deviation_part2, "\n")


# iii)
# Simulating the net win scenarios for Part 2 (5,000 samples)
simulated_wins_part2 <- sample(c(net_win_2g_part2, net_win_3g_part2), size = 5000, replace = TRUE, prob = c(prob_2g_part2, prob_3g_part2))

# Calculating the estimated expected net win using the sample mean for Part 2
estimated_expected_win_part2 <- mean(simulated_wins_part2)

# Calculating the confidence interval (95%) for Part 2
lower_bound_part2 <- estimated_expected_win_part2 - z_value * sd(simulated_wins_part2)
upper_bound_part2 <- estimated_expected_win_part2 + z_value * sd(simulated_wins_part2)

# Results for Part 2
cat("Estimated expected net win in Part 2:", estimated_expected_win_part2, "\n")
cat("95% confidence interval in Part 2:", lower_bound_part2, "-", upper_bound_part2, "\n")

# Checking if the E(X) is within the confidence interval for Part 2
if (expected_win_part2 >= lower_bound_part2 & expected_win_part2 <= upper_bound_part2) {
  cat("Yes, the E(X) in Part 2 is within the 95% confidence interval.")
} else {
  cat("No, the E(X) in Part 2 is not within the 95% confidence interval.")
}


# iv)
# Creating frequency table for simulated net win (Y) in Part 2
simulated_data_counts_part2 <- table(cut(simulated_wins_part2, breaks = breaks, labels = FALSE))

# Expected counts for each bin (based on original probabilities) in Part 2
expected_counts_part2 <- c(prob_2g_part2 * 5000, prob_3g_part2 * 5000)

# Performing the Chi-square goodness of fit test for Part 2
chisq_test_part2 <- chisq.test(x = simulated_data_counts_part2)

# Results for Part 2
cat("Chi-square test statistic in Part 2:", chisq_test_part2$statistic, "\n")
cat("p-value in Part 2:", chisq_test_part2$p.value, "\n")

# Interpreting the results for Part 2
if (chisq_test_part2$p.value > 0.05) {
  cat("We fail to reject the null hypothesis that the simulated data (Y) in Part 2 follows the expected distribution (X).")
} else {
  cat("We reject the null hypothesis that the simulated data (Y) in Part 2 follows the expected distribution (X).")
}




# v)
# Mentioned in report**



# Part - 3
# i)
# Updating the probabilities for the new scenario (best of five series)
p_celtics_home_part3 = 0.60  # Probability Celtics win at home in Part 3
p_warriors_home_part3 = 0.58  # Probability Warriors win at home in Part 3

# A function to calculate probability of winning a game in best of five series
win_prob_best_of_five <- function(home_team, p_home) {
  if (home_team == "Celtics") {
    return(p_home)
  } else {
    return(1 - p_home)
  }
}

# Calculating the probabilities for each scenario in best of five series:

# Celtics win first game (Celtics home)
celtics_win_1_part3 = win_prob_best_of_five("Celtics", p_celtics_home_part3)

# Warriors win first game (Celtics home)
warriors_win_1_part3 = (1 - win_prob_best_of_five("Celtics", p_celtics_home_part3))

# Celtics win second game (Warriors home)
celtics_win_2_part3 = win_prob_best_of_five("Warriors", p_warriors_home_part3)

# Warriors win second game (Warriors home)
warriors_win_2_part3 = (1 - win_prob_best_of_five("Warriors", p_warriors_home_part3))

# Celtics win third game (Celtics home)
celtics_win_3_part3 = win_prob_best_of_five("Celtics", p_celtics_home_part3)

# Combined probabilities for Celtics winning the best of five series
celtics_win_series_part3 = celtics_win_1_part3 * warriors_win_2_part3 * celtics_win_3_part3

# The probability
cat("Probability that Celtics win the best of five series in Part 3:", celtics_win_series_part3, "\n")


# ii)
# Calculating the net win for each scenario in best of five series
net_win_3g_part3 = 3 * win_amount  # Win 1st, 3rd, and 5th game in Part 3

# Calculating the probability for each net win scenario in best of five series
prob_3g_part3 = celtics_win_series_part3

# Calculating the expected net win (weighted average) for best of five series
expected_win_part3 <- prob_3g_part3 * net_win_3g_part3

# Calculating the squared deviations from the mean for best of five series
sq_dev_3g_part3 <- (net_win_3g_part3 - expected_win_part3)^2 * prob_3g_part3

# Calculating the variance (sum of squared deviations) for best of five series
variance_part3 <- sq_dev_3g_part3

# Calculating the standard deviation (square root of variance) for best of five series
standard_deviation_part3 <- sqrt(variance_part3)

# Results for best of five series
cat("Expected net win in Part 3 (Best of Five):", expected_win_part3, "\n")
cat("Standard deviation of net win in Part 3 (Best of Five):", standard_deviation_part3, "\n")



# iii)
# Simulating the net win scenarios for Part 3 (5,000 samples)
simulated_wins_part3 <- sample(c(net_win_3g_part3, 0), size = 5000, replace = TRUE, prob = c(prob_3g_part3, 1 - prob_3g_part3))

# Calculating the estimated expected net win using the sample mean for Part 3
estimated_expected_win_part3 <- mean(simulated_wins_part3)

# Calculating the confidence interval (95%) for Part 3
lower_bound_part3 <- estimated_expected_win_part3 - z_value * sd(simulated_wins_part3)
upper_bound_part3 <- estimated_expected_win_part3 + z_value * sd(simulated_wins_part3)

# Results for Part 3
cat("Estimated expected net win in Part 3 (Best of Five):", estimated_expected_win_part3, "\n")
cat("95% confidence interval in Part 3 (Best of Five):", lower_bound_part3, "-", upper_bound_part3, "\n")

# Checking if the E(X) is within the confidence interval for Part 3
if (expected_win_part3 >= lower_bound_part3 & expected_win_part3 <= upper_bound_part3) {
  cat("Yes, the E(X) in Part 3 (Best of Five) is within the 95% confidence interval.")
} else {
  cat("No, the E(X) in Part 3 (Best of Five) is not within the 95% confidence interval.")
}



# iv)
# Creating frequency table for simulated net win (Y) in Part 3
simulated_data_counts_part3 <- table(cut(simulated_wins_part3, breaks = breaks, labels = FALSE))

# Expected counts for each bin (based on original probabilities) in Part 3
expected_counts_part3 <- c(prob_3g_part3 * 5000)

# Performing the Chi-square goodness of fit test for Part 3
chisq_test_part3 <- chisq.test(x = simulated_data_counts_part3)

# Results for Part 3
cat("Chi-square test statistic in Part 3 (Best of Five):", chisq_test_part3$statistic, "\n")
cat("p-value in Part 3 (Best of Five):", chisq_test_part3$p.value, "\n")

# Interpreting the results for Part 3
if (chisq_test_part3$p.value > 0.05) {
  cat("We fail to reject the null hypothesis that the simulated data (Y) in Part 3 (Best of Five) follows the expected distribution (X).")
} else {
  cat("We reject the null hypothesis that the simulated data (Y) in Part 3 (Best of Five) follows the expected distribution (X).")
}


# v)
# Creating a table for the net win results in Part 3
table_part3 <- table(simulated_wins_part3)

# Calculating the observed probabilities from the simulated data in Part 3
observed_probs_part3 <- table_part3 / sum(table_part3)

# Calculating the expected probabilities based on the original probabilities in Part 3
expected_probs_part3 <- c(prob_3g_part3, 1 - prob_3g_part3)

# Creating a data frame for comparison
comparison_df_part3 <- data.frame(
  Net_Win = c("Win", "No Win"),
  Observed_Probability = observed_probs_part3,
  Expected_Probability = expected_probs_part3
)

# The comparison data frame
cat("\nComparison of Observed and Expected Probabilities in Part 3 (Best of Five):\n")
print(comparison_df_part3)



# Part - 4
# i)
# Updating the probabilities for the new scenario (best of seven series)
p_celtics_home_part4 = 0.60  # Probability Celtics win at home in Part 4
p_warriors_home_part4 = 0.58  # Probability Warriors win at home in Part 4

# A function to calculate probability of winning a game in best of seven series
win_prob_best_of_seven <- function(home_team, p_home) {
  if (home_team == "Celtics") {
    return(p_home)
  } else {
    return(1 - p_home)
  }
}

# Calculating the probabilities for each scenario in best of seven series:

# Celtics win first two games (Celtics home)
celtics_win_2_part4 = win_prob_best_of_seven("Celtics", p_celtics_home_part4) ^ 2

# Warriors win first two games (Celtics home)
warriors_win_2_part4 = (1 - win_prob_best_of_seven("Celtics", p_celtics_home_part4)) ^ 2

# Celtics win the series in four games (considering only Celtics win first two games)
celtics_win_series_4g_part4 = celtics_win_2_part4

# Warriors win first two games, Celtics win third game (Celtics home, Warriors home)
warriors_win_2_celtics_win_1_part4 = (1 - win_prob_best_of_seven("Celtics", p_celtics_home_part4)) ^ 2 * win_prob_best_of_seven("Celtics", p_celtics_home_part4)

# Celtics win the series in five games (considering only Warriors win first two games and Celtics win third game)
celtics_win_series_5g_part4 = warriors_win_2_celtics_win_1_part4

# Combined probabilities for Celtics winning the series in Part 4
celtics_win_series_part4 = celtics_win_series_4g_part4 + celtics_win_series_5g_part4

# The probability
cat("Probability that Celtics win the series in Part 4:", celtics_win_series_part4, "\n")


# ii)
# Calculating the net win for each scenario in best of seven series
net_win_4g_part4 = 4 * win_amount  # Win first, second, sixth, and seventh game in Part 4

# Calculating the probability for each net win scenario in best of seven series
prob_4g_part4 = celtics_win_series_4g_part4
prob_5g_part4 = celtics_win_series_5g_part4

# Calculating the expected net win (weighted average) for best of seven series
expected_win_part4 <- (prob_4g_part4 * net_win_4g_part4) + (prob_5g_part4 * (net_win_4g_part4 - win_amount))

# Calculating the squared deviations from the mean for best of seven series
sq_dev_4g_part4 <- (net_win_4g_part4 - expected_win_part4)^2 * prob_4g_part4
sq_dev_5g_part4 <- ((net_win_4g_part4 - win_amount) - expected_win_part4)^2 * prob_5g_part4

# Calculating the variance (sum of squared deviations) for best of seven series
variance_part4 <- sq_dev_4g_part4 + sq_dev_5g_part4

# Calculating the standard deviation (square root of variance) for best of seven series
standard_deviation_part4 <- sqrt(variance_part4)

# Results for best of seven series
cat("Expected net win in Part 4 (Best of Seven):", expected_win_part4, "\n")
cat("Standard deviation of net win in Part 4 (Best of Seven):", standard_deviation_part4, "\n")


# iii)
# Simulating the net win scenarios for Part 4 (5,000 samples)
simulated_wins_part4 <- sample(c(net_win_4g_part4, net_win_4g_part4 - win_amount, 0), size = 5000, replace = TRUE, prob = c(prob_4g_part4, prob_5g_part4, 1 - (prob_4g_part4 + prob_5g_part4)))

# Calculating the estimated expected net win using the sample mean for Part 4
estimated_expected_win_part4 <- mean(simulated_wins_part4)

# Calculating the confidence interval (95%) for Part 4
lower_bound_part4 <- estimated_expected_win_part4 - z_value * sd(simulated_wins_part4)
upper_bound_part4 <- estimated_expected_win_part4 + z_value * sd(simulated_wins_part4)

# Results for Part 4
cat("Estimated expected net win in Part 4 (Best of Seven):", estimated_expected_win_part4, "\n")
cat("95% confidence interval in Part 4 (Best of Seven):", lower_bound_part4, "-", upper_bound_part4, "\n")

# Checking if the E(X) is within the confidence interval for Part 4
if (expected_win_part4 >= lower_bound_part4 & expected_win_part4 <= upper_bound_part4) {
  cat("Yes, the E(X) in Part 4 (Best of Seven) is within the 95% confidence interval.")
} else {
  cat("No, the E(X) in Part 4 (Best of Seven) is not within the 95% confidence interval.")
}


# iv)
# Creating frequency table for simulated net win (Y) in Part 4
simulated_data_counts_part4 <- table(cut(simulated_wins_part4, breaks = breaks, labels = FALSE))

# Expected probabilities for each bin (based on original probabilities) in Part 4
expected_probs_part4 <- c(prob_4g_part4, prob_5g_part4, (1 - (prob_4g_part4 + prob_5g_part4)))

# Performing the Chi-square goodness of fit test for Part 4
chisq_test_part4 <- chisq.test(x = simulated_data_counts_part4)

# Results for Part 4
cat("Chi-square test statistic in Part 4 (Best of Seven):", chisq_test_part4$statistic, "\n")
cat("p-value in Part 4 (Best of Seven):", chisq_test_part4$p.value, "\n")

# Interpreting the results for Part 4
if (chisq_test_part4$p.value > 0.05) {
  cat("We fail to reject the null hypothesis that the simulated data (Y) in Part 4 (Best of Seven) follows the expected distribution (X).")
} else {
  cat("We reject the null hypothesis that the simulated data (Y) in Part 4 (Best of Seven) follows the expected distribution (X).")
}



# v)
# Creating a data frame for comparison of observed and expected probabilities in Part 4
comparison_df_part4 <- data.frame(
  Net_Win = c("Win", "No Win"),
  Observed_Probability.simulated_wins_part4 = c(
    sum(simulated_wins_part4 >= net_win_4g_part4),
    sum(simulated_wins_part4 < net_win_4g_part4)
  ),
  Observed_Probability.Freq = c(
    sum(simulated_wins_part4 >= net_win_4g_part4) / length(simulated_wins_part4),
    sum(simulated_wins_part4 < net_win_4g_part4) / length(simulated_wins_part4)
  ),
  Expected_Probability = c(prob_4g_part4, prob_5g_part4)
)

# The comparison data frame for Part 4
print(comparison_df_part4)

# Interpreting the final results for Part 4
if (chisq_test_part4$p.value > 0.05) {
  cat("The betting strategy appears to be favorable.\n")
} else {
  cat("The betting strategy may not be favorable. Further analysis is recommended.\n")
}
