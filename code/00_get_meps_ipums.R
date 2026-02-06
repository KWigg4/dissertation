# IPUMS extract to see if they have better unique ids:
pacman::p_load(ipumsr, dplyr)

ipums0 <- read.csv(here::here("data_raw_ipums/meps_00002.csv/meps_00002.csv"))
# 248869 obs

# save Raw
saveRDS(
	ipums0,
	here::here("data_raw_ipums/ipums_raw.RDS")
	)

first500 <- ipums0[1:500, ]
write.csv(
	first500,
	here::here("data_raw_ipums/ipums_raw_first500.csv")
)


ipums1 <- ipums0 %>%
	filter(
		# In scope for the full year (based on December 31 flag)
		INSCOP1231 == 3,
		INSCOP == 1,

		# Valid responses for family size and marital status
		!is.na(FAMSIZE),
		!is.na(MARSTAT),
		!MARSTAT %in% c(00, 99),


		# Valid responses for SF12 physical and mental health scales
		!is.na(PCS),
		!is.na(MCS),

		# Age restriction for treatment and control groups
		AGELAST %in% c(23, 24, 25, 27, 28, 29)
	) |>
	mutate(prepost = if_else(YEAR < 2011, "pre", "post"))
# 20355

ipums1_both <- ipums1 |>
	select(MEPSID, prepost) |>
	tidyr::pivot_wider(
		values_from = prepost,
		names_from = prepost)

ipums1_complete_ids <-ipums1 %>%
	distinct(MEPSID, prepost) %>%
	count(MEPSID) %>%
	filter(n == 2) %>%              # must have both pre and post
	pull(MEPSID)
# 765 : way smaller

# 2. Filter to complete cases only
ipums1_complete <- ipums1 %>%
	dplyr::filter(MEPSID %in% ipums1_complete_ids) |>
	group_by(MEPSID) |>
	mutate(n_MEPSID=n()) |>
	ungroup()
# 1530

summary(ipums1_complete$EXPTOT)
