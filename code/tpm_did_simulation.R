# Copilot prompt:
#In R, get the intercepts and slopes from a two-part model difference-in-differences analysis with pre-post (ind_post, 0/1) and use those to create a data-generating function to simulate a dataset. The outcome variable for part 1 of the tpm is ind_gt0, the outcome variable for part 2 of the tpm is TOTEXPYY. The treatment variable is ind_tx. Predictor variables are ind_female (0, 1), HISPANX (0,1), INSCOV (0,1), and PCS42 (continuous). The person ID for random effect is DUPERSID


# Load necessary libraries
library(lme4)
library(twopartm)

# Assume data is in a data frame called df

# Part 1: Binary outcome model (ind_gt0)
part1_model <- glmer(
	ind_gt0 ~ ind_post * ind_tx + ind_female + HISPANX + INSCOV + PCS42 + (1 | DUPERSID), data = fyc, family = binomial)

# Part 2: Continuous outcome model (TOTEXPYY), conditional on ind_gt0 == 1
part2_model <- lmer(log(TOTEXPYY) ~ ind_post * ind_tx + ind_female + HISPANX + INSCOV + PCS42 + (1 | DUPERSID), data = subset(fyc, ind_gt0 == 1))

# Extract coefficients
coef_part1 <- fixef(part1_model)
coef_part2 <- fixef(part2_model)

# Data-generating function
simulate_data <- function(n, coef1, coef2) {
  DUPERSID <- 1:n
  ind_female <- rbinom(n, 1, 0.5)
  HISPANX <- rbinom(n, 1, 0.3)
  INSCOV <- rbinom(n, 1, 0.7)
  PCS42 <- rnorm(n, mean = 50, sd = 10)
  ind_tx <- rbinom(n, 1, 0.5)
  ind_post <- rbinom(n, 1, 0.5)

  # Linear predictor for part 1
  lp1 <- coef1["(Intercept)"] +
         coef1["ind_post"] * ind_post +
         coef1["ind_tx"] * ind_tx +
         coef1["ind_post:ind_tx"] * ind_post * ind_tx +
         coef1["ind_female"] * ind_female +
         coef1["HISPANX"] * HISPANX +
         coef1["INSCOV"] * INSCOV +
         coef1["PCS42"] * PCS42

  prob_gt0 <- plogis(lp1)
  ind_gt0 <- rbinom(n, 1, prob_gt0)

  # Linear predictor for part 2
  lp2 <- coef2["(Intercept)"] +
         coef2["ind_post"] * ind_post +
         coef2["ind_tx"] * ind_tx +
         coef2["ind_post:ind_tx"] * ind_post * ind_tx +
         coef2["ind_female"] * ind_female +
         coef2["HISPANX"] * HISPANX +
         coef2["INSCOV"] * INSCOV +
         coef2["PCS42"] * PCS42

  log_TOTEXPYY <- lp2 + rnorm(n, 0, 1)
  TOTEXPYY <- ifelse(ind_gt0 == 1, exp(log_TOTEXPYY), 0)

  data.frame(DUPERSID, ind_female, HISPANX, INSCOV, PCS42, ind_tx, ind_post, ind_gt0, TOTEXPYY)
}

# Example usage
# simulated_df <- simulate_data(1000, coef_part1, coef_part2)
