library(dplyr)

fyc <- readRDS(here::here("data_processed/fyc.RDS")) |>
	mutate(ind_gt0 = ifelse(TOTEXPYY > 0, 1, 0)) |>
	#mutate(log_totexpy = log(TOTEXPYY), na.rm=TRUE) |>
	# collapse the categories of insurance type (private any, public only)
	# 3 means uninsured, and it matched UNINSYY
	mutate(ind_INSCOV = if_else(INSCOVYY ==3, 0, 1)) |>
	# race: 2012 only year they didn't combine 4, 5 for RACEV1X. Combine
	# mutate(
	# 	RACE = case_when(
	# 		RACEV1X %in% c(4,5) ~ 4,
	# 		RACEX %in% c(4,5) ~ 4,
	# 		TRUE ~ coalesce(RACEX, RACEV1X)
	# 	)) |>
	# don't need weights, not going to do weighted sampling
	select(
		, year:DUPERSID
		, AGELAST
		, TOTEXPYY
		, z_TOTEXPYY
		, ind_gt0
		, PANEL
		, ind_INSCOV
		, INSCOVYY
		, INSCOPE   # it has the same values as INSCOPYY
		, MARRYYYX
		, fct_MARRYYYX
		, FAMSZEYR
		, PCS42:MCS42
		, SEX
		, HISPANX
		# , RACE
		# , EDUCYR, HIDEG, EDUYRDG, EDRECODE
		, POVCATYY
		, fct_POVCATYY
		, ARTHDX:RAREDX
	) |>
	mutate(
		ind_post = ifelse(pre_post=="post", 0, 1),
		ind_female = ifelse(SEX==2, 1,0),
		ind_comorb = ifelse(
			(ARTHDX == 1 | ASTHDX==1 | CANCERDX==1 | CHOLDX==1 |
				DIABDX==1|HIBPDX==1 | RAREDX==1), 1, 0),
		zPCS42 = as.vector(scale(PCS42)),
		zMCSr2 = as.vector(scale(PCS42))
		)

write.csv(
	fyc,
	here::here("data_processed/fyc_processed.csv"),
	row.names = FALSE
)
