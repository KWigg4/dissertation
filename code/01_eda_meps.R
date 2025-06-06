library(dplyr)

fyc <- readRDS(here::here("data_processed/fyc.RDS"))

# How many in pre/ post
table(fyc$pre_post)

mean(fyc$TOTEXPYY) #1990.039

# Histogram:
hist(fyc$TOTEXPYY, breaks=100)
abline(v=mean(fyc$TOTEXPYY), col='red', lwd=3)
lines(density(fyc$TOTEXPYY), col = 'green', lwd = 3)

# can't barely see anything - do without 50k >
fyc_rm50k <- fyc |> dplyr::filter(TOTEXPYY<50000)
hist(fyc_rm50k$TOTEXPYY, breaks=100)
abline(v=mean(fyc_rm50k$TOTEXPYY), col='red', lwd=3)
lines(density(fyc_rm50k$TOTEXPYY), col = 'green', lwd = 3)

# how many have 0?
fyc |> filter(TOTEXPYY==0) |> nrow() #6481 (of 18841 is .34, or 34%)

fyc |> select(contains(c("TOTEXPYY"))) |> View()

# how many above 50k? Deb has 35 (max is same so we both have highest value)
fyc |>
	filter(TOTEXPYY>=50000) |>
	View() # 41. I have 6 that Deb doesn't have.
# 50k is about 2.74 SD (99.65th percentile)
# (use z_TOTEXPYY)



# I have 942 more than Deb... and 6 more where >50k. Why?
# (Deb got 17899 and I have 18841)
nrow(fyc) #18841
table(fyc$ind_tx) # 0: 9512, 1: 9329
# Their control group N=8983, mine=9512: DELTA: 529 more in mine
# Their treatment group N=8916, mine=9329. DELTA: 413 more in mine
prop.table(table(fyc$MARRYYYX, fyc$ind_tx))

# All variables by tx, summary:
fyc %>% split(.$ind_tx) %>% purrr::map(summary)

# Insurance coverage:
table(fyc$INSCOVYY, fyc$pre_post)

summary(fyc$TOTEXPYY)
fyc |> filter(TOTEXPYY >= 50000) |> select(TOTEXPYY) |> nrow()
# I have 41, Deb had 35

# Distribution / shape of TOTEXPYY:
summary(fyc$TOTEXPYY)


# means (off by $77 for treated, $241 for control) - but these aren't weighted
fyc |>
	group_by(ind_tx) |>
	summarize(
		mean_tot = mean(TOTEXPYY),
		sd_tot = sd(TOTEXPYY),
		min_tot = min(TOTEXPYY),
		max_tot = max(TOTEXPYY))
# physical and mental health scale seem off... Or need different weight? SAQ wt?

summary(fyc$PCS42)

# For demographic variables you have to use weights to get same as T1 in paper

options(survey.lonely.psu = "adjust")
mepsperwt = svydesign(
	id = ~VARPSU,
	strata = ~VARSTR,
	weights = ~PERWTYYF,
	data = fyc,
	nest = TRUE)

# Trying wtih gtsummary
library(gtsummary)
mepsperwt |>
	# don't include totexpyy in here, it changes it totally
	tbl_svysummary(by = ind_tx, percent = "column", include=c(SEX, fct_POVCATYY, CANCERDX, RAREDX, ARTHDX))
# we won't report the n's from this, only the rates like they do in their paper


# Maybe physical/mental health are using saq weights?
mepssaq = svydesign(
	id = ~VARPSU,
	strata = ~VARSTR,
	weights = ~SAQWTYYF,
	data = fyc,
	nest = TRUE)

mepssaq |>
	tbl_svysummary(
		include = c(PCS42, MCS42)) # Deb: 54/51, Mine:56/54


mepssaq |>
	tbl_svysummary(by = ind_tx, include = c(PCS42, MCS42))

prop.table(svytable(~POVCATYY + ind_tx, design=mepsdsgn))
