# Copilot prompt:
#In R, get the intercepts and slopes from a two-part model difference-in-differences analysis with pre-post (ind_post, 0/1) and use those to create a data-generating function to simulate a dataset. The outcome variable for part 1 of the tpm is ind_gt0, the outcome variable for part 2 of the tpm is TOTEXPYY. The treatment variable is ind_tx. Predictor variables are ind_female (0, 1), HISPANX (0,1), INSCOV (0,1), and PCS42 (continuous). The person ID for random intercept is DUPERSID: many DUPERSIDs have two records - one for pre and one for post.

# Load necessary libraries
pacman::p_load(
	dplyr,
	lme4,
	twopartm,
	lmerTest
)

sim_conditions <- expand.grid(
	n = c(1000, 2000, 3000),
	magnitude = c(1, 2, 3),         # effect size for DiD
	p_treated = c(0.01, 0.05, 0.10) # proportion of treated units
)

fyc <- read.csv(here::here("data_processed/fyc_processed.csv"))
fyc$PCS42_scaled <- scale(fyc$PCS42)

# Looking for high variance inflation factors
library(car)
vif(lm(ind_gt0 ~ ind_post * ind_tx + ind_female + HISPANX + PCS42, data = fyc))
# none seem to be an issue. Try scaling PCS42


# look for complete or quasi-complete separation
pacman::p_load(detectseparation)
fyc_sep <- glm(
	ind_gt0 ~ ind_post * ind_tx + ind_female + HISPANX + PCS42,
	data = fyc,
	family = binomial("logit"), method = "detect_separation")
fyc_sep # none are infinite
summary(update(fyc_sep, method = "glm.fit"))

# Part 1: Binary outcome model (ind_gt0)
part1_model <- glmer(
	ind_gt0 ~ ind_post + ind_tx + ind_female + HISPANX  + PCS42_scaled + INSCOV + (1 | DUPERSID), data = fyc, family = binomial,
	control=glmerControl(optimizer="bobyqa"))

summary(part1_model)
		# m0: with ind_post*ind_tx, model convergence issues. INSCOV might be too correlated with HISPANX.
	  # m1: Same
	  # m2: Tried without INSCOV but ind_post*ind_tx and scaled PCS42. No warning
	  # with optimizer, got same info results generally, still showing interaction not significant
	  # tried without interaction again but with optimizer and INSCOV

# Part 2: Continuous outcome model (TOTEXPYY), conditional on ind_gt0 == 1
part2_model <- lmerTest::lmer(log(TOTEXPYY) ~ ind_post + ind_tx + ind_female + HISPANX + INSCOV + PCS42_scaled + (1 | DUPERSID), data = subset(fyc, ind_gt0 == 1))

summary(part2_model)

# Extract coefficients
coef_part1 <- fixef(part1_model)
coef_part2 <- fixef(part2_model)
sigma_part2 <- sigma(part2_model) # residual Stddev (sigma)

# Data-generating function
simulate_data <- function(n, coef1, coef2, sigma_part2) {
  DUPERSID <- 1:n
  ind_female <- rbinom(n, 1, 0.5)
  HISPANX <- rbinom(n, 1, 0.3)
  INSCOV <- rbinom(n, 1, 0.7)
  PCS42 <- rnorm(n, mean = 47.35, sd = 19)
  ind_tx <- rbinom(n, 1, 0.5)
  ind_post <- rbinom(n, 1, 0.4)

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

  log_TOTEXPYY <- lp2 + rnorm(n, 0, sigma_part2)
  TOTEXPYY <- ifelse(ind_gt0 == 1, exp(log_TOTEXPYY), 0)

  data.frame(DUPERSID, ind_female, HISPANX, INSCOV, PCS42, ind_tx, ind_post, ind_gt0, TOTEXPYY)
}

sim_df <- simulate_data(1000, coef_part1, coef_part2, sigma_part2)


## VALIDATION
fyc$pred_prob_gt0 <- predict(part1_model, type="response")
fyc$pred_log_totexp <- NA
fyc$pred_log_totexp[fyc$ind_gt0==1] <- predict(part2_model)
fyc$pred_totexp <- ifelse(fyc$ind_gt0 == 1, exp(fyc$pred_log_totexp), 0)

library(pROC)
roc(fyc$ind_gt0, fyc$pred_prob_gt0)
# AUC: 0.9281 (outstanding). Ability of model to distinguish between positive and zero outcomes. An AUC of 0.85 means that 85% of the time, the model correctly ranks a randomly chosen positive case higher than a randomly chosen zero case.

# Only for positive outcomes: MSE (avg squared diff between predicted and actual values / the lower the better. A lower MSE means your predictions are closer to the actual values. Sensitive to outliers bc it squares the errors. Can compare it to the variance of the actual data to see how much error your model introduces.)
# 2. Calculate MSE for continuous outcome
# Filter to non-missing values
valid_rows <- !is.na(fyc$TOTEXPYY) & !is.na(fyc$pred_totexp)
mse <- mean((fyc$TOTEXPYY[valid_rows] - fyc$pred_totexp[valid_rows])^2)
print(paste("MSE:", mse))

# Plot observed vs predicted
png("observed_vs_predicted.png")
ggplot(fyc[valid_rows, ], aes(x = TOTEXPYY, y = pred_totexp)) +
	geom_point(alpha = 0.4) +
	geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
	labs(title = paste("Observed vs Predicted TOTEXPYY\nMSE =", round(mse, 2)),
			 x = "Observed TOTEXPYY", y = "Predicted TOTEXPYY") +
	theme_minimal()
dev.off()


### ATTEMPT 2 asking for repeated subjects
library(MASS)  # for mvrnorm

simulate_data <- function(n, coef1, coef2, sigma_part2, sd1 = 0.5, sd2 = 0.5, rho = 0.3) {
	# Covariance matrix for correlated random effects
	cov_matrix <- matrix(
		c(sd1^2, rho * sd1 * sd2, rho * sd1 * sd2, sd2^2), nrow = 2)

	# Simulate individual-level covariates
	DUPERSID <- 1:n
	ind_female <- rbinom(n, 1, 0.5)
	HISPANX <- rbinom(n, 1, 0.3)
	INSCOV <- rbinom(n, 1, 0.7)
	PCS42 <- rnorm(n, mean = 47.35, sd = 19)
	ind_tx <- rbinom(n, 1, 0.5)

	# Simulate correlated random effects: one for part 1, one for part 2
	rand_effects <- mvrnorm(n, mu = c(0, 0), Sigma = cov_matrix)
	rand1 <- rand_effects[, 1]  # for part 1
	rand2 <- rand_effects[, 2]  # for part 2

	# Create repeated measures (pre and post)
	base_df <- data.frame(DUPERSID, ind_female, HISPANX, INSCOV, PCS42, ind_tx, rand1, rand2)
	sim_df <- base_df[rep(1:n, each = 2), ]
	sim_df$ind_post <- rep(c(0, 1), times = n)

	# Part 1: Binary outcome
	lp1 <- coef1["(Intercept)"] +
		coef1["ind_post"] * sim_df$ind_post +
		coef1["ind_tx"] * sim_df$ind_tx +
		coef1["ind_post:ind_tx"] * sim_df$ind_post * sim_df$ind_tx +
		coef1["ind_female"] * sim_df$ind_female +
		coef1["HISPANX"] * sim_df$HISPANX +
		coef1["INSCOV"] * sim_df$INSCOV +
		coef1["PCS42"] * sim_df$PCS42 +
		sim_df$rand1

	sim_df$ind_gt0 <- rbinom(nrow(sim_df), 1, plogis(lp1))

	# Part 2: Continuous outcome
	lp2 <- coef2["(Intercept)"] +
		coef2["ind_post"] * sim_df$ind_post +
		coef2["ind_tx"] * sim_df$ind_tx +
		coef2["ind_post:ind_tx"] * sim_df$ind_post * sim_df$ind_tx +
		coef2["ind_female"] * sim_df$ind_female +
		coef2["HISPANX"] * sim_df$HISPANX +
		coef2["INSCOV"] * sim_df$INSCOV +
		coef2["PCS42"] * sim_df$PCS42 +
		sim_df$rand2

	log_TOTEXPYY <- lp2 + rnorm(nrow(sim_df), 0, sigma_part2)
	sim_df$TOTEXPYY <- ifelse(sim_df$ind_gt0 == 1, exp(log_TOTEXPYY), 0)

	return(sim_df)
}

sim1 <- simulate_data(n=500)

#####

simulate_tpm_data <- function(n_individuals = 500) {
	set.seed(123)

	# Number of timepoints (pre and post)
	n_timepoints <- 2
	n_total <- n_individuals * n_timepoints

	# Create individual-level variables
	DUPERSID <- rep(1:n_individuals, each = n_timepoints)
	ind_post <- rep(0:1, times = n_individuals)
	ind_tx <- rep(rbinom(n_individuals, 1, 0.5), each = n_timepoints)
	ind_female <- rep(rbinom(n_individuals, 1, 0.5), each = n_timepoints)
	HISPANX <- rep(rbinom(n_individuals, 1, 0.3), each = n_timepoints)
	INSCOV <- rep(rbinom(n_individuals, 1, 0.7), each = n_timepoints)
	PCS42 <- rep(rnorm(n_individuals, mean = 50, sd = 10), each = n_timepoints)

	# Random intercepts for individuals
	random_intercepts <- rep(rnorm(n_individuals, mean = 0, sd = 0.5), each = n_timepoints)

	# Part 1: Logistic model for ind_gt0
	b <- list(intercept = -1.0, post = 0.5, tx = 0.3, interaction = 0.8,
						female = 0.2, hispanx = -0.1, inscov = 0.4, pcs42 = 0.01)

	lin_pred1 <- b$intercept + b$post * ind_post + b$tx * ind_tx +
		b$interaction * ind_post * ind_tx + b$female * ind_female +
		b$hispanx * HISPANX + b$inscov * INSCOV + b$pcs42 * PCS42 +
		random_intercepts

	prob_gt0 <- 1 / (1 + exp(-lin_pred1))
	ind_gt0 <- rbinom(n_total, 1, prob_gt0)

	# Part 2: Linear model for log(TOTEXPYY)
	g <- list(intercept = 7.0, post = 0.2, tx = 0.1, interaction = 0.5,
						female = 0.1, hispanx = -0.05, inscov = 0.3, pcs42 = 0.02)

	lin_pred2 <- g$intercept + g$post * ind_post + g$tx * ind_tx +
		g$interaction * ind_post * ind_tx + g$female * ind_female +
		g$hispanx * HISPANX + g$inscov * INSCOV + g$pcs42 * PCS42 +
		random_intercepts

	residual_error <- rnorm(n_total, mean = 0, sd = 0.3)
	log_TOTEXPYY <- lin_pred2 + residual_error
	TOTEXPYY <- exp(log_TOTEXPYY)
	TOTEXPYY[ind_gt0 == 0] <- 0

	# Return data frame
	data.frame(DUPERSID, ind_post, ind_tx, ind_female, HISPANX,
						 INSCOV, PCS42, ind_gt0, TOTEXPYY)
}

sim2 <- simulate_tpm_data(n_individuals=500)
