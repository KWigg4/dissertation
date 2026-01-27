# Example DiD , CF
# Generalized tpm (Logit + Gamma GLM)

## 1) Simulate toy HCE
set.seed(123)

N <- 2000

df <- data.frame(
	id = 1:N,
	treat = rbinom(N, 1, 0.5),
	post  = rbinom(N, 1, 0.5)
)

df$interaction <- df$treat * df$post

# Probability of any spending
linprob <- -1 + 0.5*df$treat + 0.3*df$post + 1.0*df$interaction
df$any_spend <- rbinom(N, 1, plogis(linprob))

# Positive spending amount (Gamma distributed)
linamt <- 7 + 0.2*df$treat + 0.1*df$post + 0.5*df$interaction
shape <- 2
scale <- exp(linamt) / shape

df$spend <- ifelse(df$any_spend == 1, rgamma(N, shape = shape, scale = scale), 0)


## 2) fit tpm on the whole dataset

m1 <- glm(any_spend ~ treat * post, data = df, family = binomial)

m2 <- glm(log(spend) ~ treat * post, data = df[df$any_spend == 1, ], family = gaussian)

## Computing ATT using counterfactual predictions

## 3) Get treated-post obs
treated_post <- subset(df, treat == 1 & post == 1)

## 4) Predict outcomes under tx, counterfactual
# 4a) Predicted prob of any spending
# Observed
p_obs <- predict(m1, newdata = treated_post, type = "response")
	# these are all the same probabilities because there weren't any other covs

# Counterfactual: flip treat to 0
treated_post_cf <- treated_post
treated_post_cf$treat <- 0
treated_post_cf$interaction <- 0

p_cf <- predict(m1, newdata = treated_post_cf, type = "response")

# 4b) Conditional mean spending (gamma GLM)
# Observed
amt_obs <- exp(predict(m2, newdata = treated_post, type = "response"))

# Counterfactual
amt_cf <- exp(predict(m2, newdata = treated_post_cf, type = "response"))

## 5) Combine parts into expected spending
# E[Y]=P(Y>0)*E[Y|Y>0]
# Belotti formula: yhat_i = phat_i*yhat_i(y_i > 0)
EY_obs <- p_obs * amt_obs
EY_cf  <- p_cf  * amt_cf


## 6) ATT
# Diff between predicted outcome under treatment and predicted counterfactual outcome if treatment had not occurred
ATT <- mean(EY_obs - EY_cf)
ATT
# $1377.625
