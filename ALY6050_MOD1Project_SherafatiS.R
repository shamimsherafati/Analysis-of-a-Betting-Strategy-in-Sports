# Part 1: Boston-New York-Boston
# Define probabilities
p_rs_win_game <- 0.6  #p_rs_win_game = probability of Red Sox wins the game
p_nyy_win_game <- 0.57 #p_nyy_win_game = probability of Yankees wins the game

# Probability that Red Sox wins the series
p_red_sox_series_win <- p_rs_win_game^2 + 2 * p_rs_win_game * (1 - p_rs_win_game) * p_nyy_win_game
p_red_sox_series_win

# Probability that Yankees wins the series
p_yankees_series_win <- (1 - p_rs_win_game)^2 + 2 * p_rs_win_game * (1 - p_rs_win_game) * (1 - p_nyy_win_game)
p_yankees_series_win

# Print results
cat("The probability that the Red Sox will win the series is", round(p_red_sox_series_win, 3), "\n")
cat("The probability that the Yankees will win the series is", round(p_yankees_series_win, 3), "\n")


p_rs_win_bo = 0.6  # probability of Red Sox wins the game in boston
p_rs_win_ny = 1 - 0.57   # probability of Red Sox wins the game in new york
p_y_win_bo = 1 - 0.6    #probability of Yankees wins the game in boston
p_y_win_ny = 0.57    #probability of Yankees wins the game in newyork

r_win = p_rs_win_bo * p_rs_win_ny + p_rs_win_bo * p_y_win_ny * p_rs_win_bo + p_y_win_bo * p_rs_win_ny * p_rs_win_bo

r_win

p_rs_win_series <- 0.43
p_nyy_win_series <- (1 - p_rs_win_series)^2 + 2 * p_rs_win_series * (1 - p_rs_win_series) * (1 - p_nyy_win_game)

# Probability distribution of net win
net_win_dist <- c(-1040, 500, 1040)
#the values -1040, 500, and 1040 represent the minimum, median, and maximum possible net wins for this scenario.

# Probability of net win

#Red Sox win 2-0: p_rs_win_series^2
#Red Sox win 2-1: 2 * p_rs_win_series * p_nyy_win_series
#Red Sox lose 0-2 or 1-2: p_nyy_win_series^2

p_net_win <- c(p_nyy_win_series^2, 2 * p_rs_win_series * p_nyy_win_series, p_rs_win_series^2)
p_net_win 
#p_net_win variable contains the probabilities of each net win

# Mean of net win
mean_net_win <- sum(net_win_dist * p_net_win)

# Standard deviation of net win
sd_net_win <- sqrt(sum((net_win_dist - mean_net_win)^2 * p_net_win))

# Display results
cat("Probability distribution of net win:", net_win_dist, "\n")
cat("Probability of net win:", p_net_win, "\n")
cat("Mean of net win:", mean_net_win, "\n")
cat("Standard deviation of net win:", sd_net_win, "\n")

set.seed(123) # set the seed for reproducibility
Y <- rbinom(n = 10000, size = 1, prob = p_net_win) * net_win_dist
estimated_mean <- mean(Y)
estimated_mean
t.test(Y, conf.level = 0.95)


# Estimating expected net win using Y values and a 95% confidence interval
n <- length(Y)
sample_mean <- mean(Y)
sample_sd <- sd(Y)
margin_of_error <- qt(0.95, n - 1) * sample_sd / sqrt(n)
confidence_interval <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)
cat("95% confidence interval for expected net win:", confidence_interval, "\n")

# Constructing a frequency distribution for Y and performing the Chi-squared goodness of fit test
observed_freq <- table(Y)
observed_freq

probabilities <- c(0.36, 0.14742, 0.1368, 0.2451)

expected_freq <- probabilities * length(Y)
expected_freq

probs_rescaled <- probabilities / sum(probabilities)
probs_rescaled

chisq_test_result <- chisq.test(observed_freq, p = probs_rescaled)
cat("Chi-squared goodness of fit test result:", "\n")
print(chisq_test_result)

set.seed(123) # set seed for reproducibility
n_sim <- 10000 # number of simulations

# probability of winning for each team
p_rs_win_game <- 0.6
p_nyy_win_game <- 0.57

# generate random values for Y
Y <- rbinom(n_sim, 2, p_rs_win_game) * 500 - rbinom(n_sim, 2, 1-p_nyy_win_game) * 520

# construct frequency distribution
freq_table <- table(Y)
freq_table


# Expected frequency distribution of X values
expected_freq <- p_net_win * 10000
expected_freq

# Observed frequency distribution of Y values
observed_freq <- table(Y)
observed_freq

# Probability distribution of net win (X)
x <- c(-2, -1, 0, 1, 2)
p <- c(0.064, 0.272, 0.424, 0.216, 0.024)

# Expected value of net win
expected_value <- sum(x * p)
expected_value


# Simulated random values of net win (X)
set.seed(123) # set seed for reproducibility
sim_x <- sample(x, size = 10000, replace = TRUE, prob = p)

# Net win for each bet
net_win <- sim_x * 1 - 1

# Total net win
total_net_win <- sum(net_win)
total_net_win


# probability of Red Sox winning each game
p_rs_win_game <- 0.6

# probability of Yankees winning each game
p_nyy_win_game <- 0.57

#Red Sox win in two games
p_rs_win_series <- p_rs_win_game^2      # "p_rs_win_game"  represent the probabilities of the Red Sox winning
p_rs_win_series

#Red Sox win in three games
p_rs_win_series <- p_rs_win_series + (3 * p_rs_win_game * (1 - p_rs_win_game) * p_nyy_win_game)  #"p_nyy_win_game" represent the Yankees winning
p_rs_win_series

#Red Sox win in three games (alternative order)
p_rs_win_series <- p_rs_win_series - (3 * p_nyy_win_game * (1 - p_nyy_win_game) * p_rs_win_game)
p_rs_win_series

cat("Probability that Red Sox win the series:", p_rs_win_series, "\n")

#Probability distribution for net win X

probabilities <- c(0.16, 0.36, 0.36, 0.12)
x_values <- c(1500, -520, 500, 500)

expected_value <- sum(x_values * probabilities)
expected_value

variance <- sum((x_values - expected_value)^2 * probabilities)
variance

sd <- sqrt(variance)
sd

cat("Expected net win:", expected_value, "\n")
cat("Standard deviation of net win:", sd, "\n")

#Create 10,000 random values for X and calculate Y

set.seed(123) # Set seed for reproducibility
random_X <- sample(x_values, size = 10000, replace = TRUE, prob = probabilities)
Y <- 10 * random_X

#Calculate expected net win from Y and construct a 95% confidence interval

expected_Y <- mean(Y)
se <- sd(Y) / sqrt(length(Y))
lower_ci <- expected_Y - qt(0.975, df = length(Y) - 1) * se
upper_ci <- expected_Y + qt(0.975, df = length(Y) - 1) * se

cat("Expected net win from Y:", expected_Y, "\n")
cat("95% Confidence interval:", lower_ci, upper_ci, "\n")
cat("Does the confidence interval contain E(X)?", lower_ci <= expected_value & upper_ci >= expected_value, "\n")

#Construct frequency distribution for Y
freq_table <- table(Y)

# observed frequencies
observed_freq <- c(1187, 3659, 3327, 5983)

# calculate expected frequencies
p_values <- c(0.36, 0.14742, 0.1368, 0.2451)
n <- sum(observed_freq)
expected_freq <- p_values * n
expected_freq <- expected_freq / sum(expected_freq) # normalize frequencies

# perform chi-squared goodness of fit test
freq_table <- cbind(observed_freq, expected_freq)
chisq.test(x = freq_table)

#Chi-squared goodness of fit test
chisq_test <- chisq.test(x = freq_table, p = expected_freq)
expected_freq <- probabilities * length(Y)

cat("Chi-squared test for goodness of fit:\n")
print(chisq_test)

cat("\nSummary:\n")
cat("The probability that the Red Sox win the series is", p_rs_win_series, "\n")
cat("The expected net win is $", expected_value, "with a standard deviation of $", sd, "\n")
cat("The 95% confidence interval for the expected net win from Y is ($", lower_ci, ", $", upper_ci, ")\n")
#cat("The Chi-squared test for goodness of fit has a p-value of", chisq_test$p.value, "\n")
cat("Based on these observations, the betting strategy is not favorable as the expected net win is negative.")

#define some variables
p_bos_win_home <- 0.6  #p_bos_win_home: the probability that the Red Sox win at home
p_yan_win_home <- 0.57  #p_yan_win_home: the probability that the Yankees win at home
p_bos_win_away <- 1 - p_yan_win_home  #p_bos_win_away: the probability that the Red Sox win away
p_yan_win_away <- 1 - p_bos_win_home  #p_yan_win_away: the probability that the Yankees win away
bet_win <- 500  #bet_win: the amount won if the Red Sox win
bet_loss <- -520  #bet_loss: the amount lost if the Red Sox lose

# Define the probability matrix
P <- matrix(c(p_bos_win_home*p_yan_win_away, (1-p_bos_win_home)*(1-p_yan_win_away), p_yan_win_home*(1-p_bos_win_away), (1-p_yan_win_home)*p_bos_win_away, p_bos_win_home*(1-p_yan_win_away), (1-p_bos_win_home)*p_yan_win_away, (1-p_yan_win_home)*p_bos_win_away, p_yan_win_home*(1-p_bos_win_home), p_bos_win_home*p_yan_win_away), nrow=3, byrow=TRUE)

# Define the state vector
v <- c(1, 0, 0)

# Calculate the probability of winning the series
v %*% P %*% P %*% P %*% c(0, 0, 1)


outcomes <- c(3*bet_win, bet_win + bet_loss + bet_win, bet_loss + bet_win + bet_win, bet_win + bet_win + bet_loss, bet_win + bet_loss*2, bet_loss + bet_win*2, bet_win*2 + bet_loss, bet_loss*2 + bet_win, bet_loss*3)

outcomes

# Define the probability matrix
P <- matrix(c(p_bos_win_home*p_yan_win_away, (1-p_bos_win_home)*(1-p)))
p


## NA
