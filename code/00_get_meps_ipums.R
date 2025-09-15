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
	)
# 20355