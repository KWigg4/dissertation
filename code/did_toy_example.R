# Toy example ATT


# Create a small dataset with treatment, post, and interaction
df <- expand.grid(
	treat = c(0,1),
	post  = c(0,1)
)
df$did <- df$treat * df$post

df
#   treat post did
# 1     0    0   0
# 2     1    0   0
# 3     0    1   0
# 4     1    1   1

# -----------------------------
# Part 1: Participation model
# -----------------------------
# We'll just hard-code toy coefficients for illustration
# logit-style linear predictor
b0 <- -0.85   # baseline intercept
b_treat <- 0.20
b_post  <- 0.10
b_did   <- 0.25

linpred1 <- with(df, b0 + b_treat*treat + b_post*post + b_did*did)
p_hat <- plogis(linpred1)   # predicted probability of positive outcome
df$p_hat <- p_hat

# -----------------------------
# Part 2: Intensity model
# -----------------------------
# GLM-style linear predictor on log scale
g0 <- log(100)   # baseline mean
g_treat <- log(1.2)   # +20% for treat
g_post  <- log(1.1)   # +10% for post
g_did   <- log(1.3)   # +30% for DiD

linpred2 <- with(df, g0 + g_treat*treat + g_post*post + g_did*did)
mu_hat <- exp(linpred2)   # conditional mean outcome
df$mu_hat <- mu_hat

# -----------------------------
# Combine parts
# -----------------------------
df$EY <- df$p_hat * df$mu_hat

df
#   treat post did     p_hat    mu_hat       EY
# 1     0    0   0 0.2994329 100.00000 29.94329
# 2     1    0   0 0.3543437 120.00000 42.52124
# 3     0    1   0 0.3318122 110.00000 36.49934
# 4     1    1   1 0.4234030 160.00000 67.74448

# -----------------------------
# ATT calculation
# -----------------------------
# Treated post with treatment (row 4)
EY_with <- df$EY[df$treat==1 & df$post==1 & df$did==1]

# Treated post counterfactual (row 2 + post effect, but no did)
# We can recompute by setting did=0 for treated post
df_cf <- data.frame(treat=1, post=1, did=0)
linpred1_cf <- b0 + b_treat*1 + b_post*1 + b_did*0
p_cf <- plogis(linpred1_cf)
linpred2_cf <- g0 + g_treat*1 + g_post*1 + g_did*0
mu_cf <- exp(linpred2_cf)
EY_cf <- p_cf * mu_cf

ATT <- EY_with - EY_cf
ATT
# [1] 20.7
