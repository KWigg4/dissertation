# Get MEPS data

# install the gh version of MEPS:
# install_github("e-mitchell/meps_r_pkg/MEPS")
pacman::p_load(
  devtools,
  foreign,
  readr,
  readxl,
  haven,
  survey,
  MEPS,
  dplyr
)


# dn_2011 = read_MEPS(year = 2011, type = "FYC")
# how to get one full dataset if needed:
# fyc2008_raw_full <- read_MEPS(year=2008, type = "FYC")

# Did we remove where missing? Check with 2008
# fyc2008_raw_full |>
# 	select(PCS42, MCS42, ADGENH42, ADDAYA42, ADCLIM42, ADPALS42, ADPWLM42,
# 				 ADMALS42, ADMWLM42, ADPAIN42, ADCAPE42, ADNRGY42, ADDOWN42, ADSOCA42) |>
# 	View()

filter_valid_indicators <- function(
		df,
		fam_var = "FMRS1231",
		marry_prefix = "MARRY",
		pcs_var = "PCS42",
		mcs_var = "MCS42",
		pov_var = NULL) {
	df %>%
		filter(.data[[fam_var]] > 0) %>%
		filter(if_any(starts_with(marry_prefix), ~ .x > 0)) %>%
		filter(!is.na(.data[[pcs_var]]), .data[[pcs_var]] > 0) %>%
		filter(!is.na(.data[[mcs_var]]), .data[[mcs_var]] > 0) %>%
		{
			if (!is.null(pov_var)) {
				filter(., .data[[pov_var]] > 0)
			} else {
				.
			}
		}
}


# Function for getting raw data and early processing
# fn_raw <- function(year){
#
#   # get raw data, only where age_last is 23-25 or 27-29
#   df1 <- read_MEPS(year={{year}}, type = "FYC") |>
#     filter(
#       between(AGELAST, 23, 25) | between(AGELAST, 27, 29)) |>
#
#       select(
#         DUID, PID, DUPERSID,
#         # total expenditures
#         starts_with("TOTEXP"),
#         PANEL,
#         # get insured, uninsured (DEB does uninsured...)
#         starts_with(c("INSCOV","UNINS")),
#         starts_with(c("INSCOP", "AGELAST", "MARRY", "FAMS", "FMRS1231")),
#         PCS42, MCS42,
#         starts_with("RACE"),
#         SEX,
#         HISPANX,
#         # I think Deb used educyr for 'some college' and HIDEG doesn't have that
#         starts_with("POVCAT"),
#         ARTHDX, ASTHDX, CANCERDX, CHOLDX, DIABDX, HIBPDX,
#         ANGIDX, CHDDX, EMPHDX, MIDX, STRKDX
#       )  |>
#       mutate(year=year) |>
#     	mutate(unique_id = paste0(PANEL, "_", DUPERSID)) |>
#     	relocate(unique_id, .before = year) |>
#       relocate(year, .before = everything())
#
#   # some of the processing that worked for all datasets:
#   df2 <- df1 |>
#     # in whole year:
#   	filter(
#   		INSCOP31 == 1,
#   		INSCOP42 == 1,
#   		INSCOP53 == 1
#   	) |>
#     # for famsize, also thinking negative values are not 'valid' but idk obvi
#     # -- some famsize08 are -1 but others aren't neg, some famsize1231 are -1, but others arent
#     # -- used FMRS1231
#     filter(
#       FMRS1231 > 0
#     ) |>
#     # Physical, Mental health not missing
#     # -- PCS42 and MCS42
#     filter(!is.na(PCS42)) |>
#   	filter(PCS42 > -9) |>
#   	filter(MCS42 > -9) |>
#     filter(!is.na(MCS42)) |>
#   	# Marry status: exclude -9 (not ascertained) and -7 (refused)
#     filter(
#       if_any(starts_with("MARRY"), ~ . > 0))
#
#   # save results
#   saveRDS(df2, here::here(paste0("data_raw/fyc_", {{year}}, "_raw.RDS")))
# }

# Method 2: seeing if this is better:

fn_raw <- function(year){

	df1 <- read_MEPS(year = {{year}}, type = "FYC") |>
		filter(between(AGELAST, 23, 25) | between(AGELAST, 27, 29)) |>

		select(
			DUPERSID,
			starts_with("TOTEXP"),
			PANEL,
			starts_with(c("INSCOV", "UNINS")),
			starts_with(c("INSCOP", "AGELAST", "MARRY", "FAMS", "FMRS1231")),
			PCS42, MCS42,
			starts_with("RACE"),
			SEX,
			HISPANX,
			starts_with("POVCAT"),
			ARTHDX, ASTHDX, CANCERDX, CHOLDX, DIABDX, HIBPDX,
			ANGIDX, CHDDX, EMPHDX, MIDX, STRKDX
		) |>
		mutate(
			year = year,
			unique_id = paste0(PANEL, "_", DUPERSID)
		) |>
		relocate(unique_id, .before = year) |>
		relocate(year, .before = everything())

	df2 <- df1 |>
		filter(INSCOP31 == 1, INSCOP42 == 1, INSCOP53 == 1) |>
		filter_valid_indicators(
			fam_var = "FMRS1231",
			marry_prefix = "MARRY",
			pcs_var = "PCS42",
			mcs_var = "MCS42",
			pov_var = NULL  # or "POVCAT08" if year-specific
		)

	saveRDS(df2, here::here(paste0("data_raw/fyc_", {{year}}, "_raw.RDS")))
}


years <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014)
length_yrs <- vector("list", length(years))

# apply fn_raw across all years in string
for (i in seq_along(years)) {
  length_yrs[[i]] <- fn_raw(years[i])
}

# purrr::map(dats, nrow)

# Create FYC ==============================================================
fyc <- local({

  # Read them all in:
  fyc_files <- list.files(
    here::here("data_raw"),
    pattern=paste0("^", "fyc_"),
    full.names = TRUE)

  dats <- lapply(fyc_files, readRDS)
  names(dats) <- c("fyc08_raw", "fyc09_raw", "fyc10_raw", "fyc11_raw",
                   "fyc12_raw", "fyc13_raw", "fyc14_raw")

  fyc08 <- dats$fyc08_raw |>
    rename(
      INSCOPYY = INSCOP08,
      TOTEXPYY = TOTEXP08,
      FAMSZEYY = FAMSZE08,
      POVCATYY = POVCAT08,
      MARRYYYX = MARRY08X,
      INSCOVYY = INSCOV08,
      UNINSYY  = UNINS08)

  fyc09 <- dats$fyc09_raw |>
    rename(
      INSCOPYY = INSCOP09,
      TOTEXPYY = TOTEXP09,
      FAMSZEYY = FAMSZE09,
      POVCATYY = POVCAT09,
      MARRYYYX = MARRY09X,
      INSCOVYY = INSCOV09,
      UNINSYY  = UNINS09)

  fyc10 <- dats$fyc10_raw |>
    rename(
      INSCOPYY = INSCOP10,
      TOTEXPYY = TOTEXP10,
      FAMSZEYY = FAMSZE10,
      POVCATYY = POVCAT10,
      MARRYYYX = MARRY10X,
      INSCOVYY = INSCOV10,
      UNINSYY  = UNINS10)

  fyc11 <- dats$fyc11_raw |>
    rename(
      INSCOPYY = INSCOP11,
      TOTEXPYY = TOTEXP11,
      FAMSZEYY = FAMSZE11,
      POVCATYY = POVCAT11,
      MARRYYYX = MARRY11X,
      INSCOVYY = INSCOV11,
      UNINSYY  = UNINS11)

  fyc12 <- dats$fyc12_raw |>
    rename(
      INSCOPYY = INSCOP12,
      TOTEXPYY = TOTEXP12,
      FAMSZEYY = FAMSZE12,
      POVCATYY = POVCAT12,
      MARRYYYX = MARRY12X,
      INSCOVYY = INSCOV12,
      UNINSYY  = UNINS12)

  fyc13 <- dats$fyc13_raw |>
    rename(
      INSCOPYY = INSCOP13,
      TOTEXPYY = TOTEXP13,
      FAMSZEYY = FAMSZE13,
      POVCATYY = POVCAT13,
      MARRYYYX = MARRY13X,
      INSCOVYY = INSCOV13,
      UNINSYY  = UNINS13)

  fyc14 <- dats$fyc14_raw |>
    rename(
      INSCOPYY = INSCOP14,
      TOTEXPYY = TOTEXP14,
      FAMSZEYY = FAMSZE14,
      POVCATYY = POVCAT14,
      MARRYYYX = MARRY14X,
      INSCOVYY = INSCOV14,
      UNINSYY  = UNINS14)

  fyc <- bind_rows(fyc08, fyc09, fyc10, fyc11, fyc12, fyc13, fyc14) |>


  	# distinct(unique_id, .keep_all=TRUE) |>
    mutate(pre_post = if_else(year < 2011, "pre", "post")) |>
    relocate(pre_post, .after = year) |>
    mutate(ind_tx = if_else(
      AGELAST <= 25, 1, 0
    )) |>
    mutate(fct_tx = factor(
      ind_tx,
      levels = c("0","1"),
      labels = c("control (25-27)","treatment (23-25)"))) |>
    relocate(ind_tx, .after=pre_post) |>
    relocate(fct_tx, .after=ind_tx) |>
    mutate(
      fct_MARRYYYX = ifelse(
        MARRYYYX == 1, "Married", "Other"),
      fct_MARRYYYX = as.factor(fct_MARRYYYX)
    ) |>
    relocate(fct_MARRYYYX, .after=MARRYYYX) |>
    # create rare disease
    mutate(RAREDX = if_else(
      (ANGIDX==1 | CHDDX ==1 | EMPHDX==1 | MIDX==1 | STRKDX==1), 1, 0
    )) %>%
    mutate(
      across(c(
        HISPANX, ARTHDX, ASTHDX, CANCERDX, CHOLDX, DIABDX, HIBPDX),
      ~if_else(.x==1, 1, 0))) |>
    # can get rid of those 5 now:
    dplyr::select(-c(ANGIDX, CHDDX, EMPHDX, MIDX, STRKDX)) |>
    mutate(fct_POVCATYY = factor(
      POVCATYY,
      levels = c("1","2","3","4","5"),
      labels = c("Poor", "Near poor","Low income", "Middle income", "High income")
    )) |>
  	mutate(z_TOTEXPYY = scale(TOTEXPYY)) |>
  	dplyr::relocate(z_TOTEXPYY, .after = TOTEXPYY) |>
  	mutate(zPCS42 = as.numeric(scale(PCS42))) |>
  	dplyr::relocate(zPCS42, .after = PCS42) |>
  	mutate(zMCS42 = as.numeric(scale(MCS42))) |>
  	dplyr::relocate(zMCS42, .after=MCS42) |>
  	mutate(ind_gt0 = ifelse(TOTEXPYY > 0, 1, 0)) |>
  	relocate(ind_gt0, .before = TOTEXPYY) |>
  	mutate(ind_INSCOV = if_else(INSCOVYY ==3, 0, 1)) |>
  	relocate(ind_INSCOV, .after=INSCOVYY) |>
  	mutate(
  		ind_post = ifelse(pre_post=="post", 0, 1),
  		ind_female = ifelse(SEX==2, 1,0),
  		ind_comorb = ifelse(
  			(ARTHDX == 1 | ASTHDX==1 | CANCERDX==1 | CHOLDX==1 |
  			 	DIABDX==1|HIBPDX==1 | RAREDX==1), 1, 0)
  	) |>
  	dplyr::select(
  		year:DUPERSID
  		, PANEL
  		, unique_id
  		, AGELAST
  		, ind_post
  		, TOTEXPYY
  		, z_TOTEXPYY
  		, ind_gt0
  		, ind_INSCOV
  		, INSCOVYY
  		, INSCOPE   # it has the same values as INSCOPYY
  		, MARRYYYX
  		, fct_MARRYYYX
  		, FAMSZEYR
  		, PCS42, zPCS42, MCS42, zMCS42
  		, SEX
  		, ind_female
  		, HISPANX
  		# , RACE
  		# , EDUCYR, HIDEG, EDUYRDG, EDRECODE
  		, POVCATYY
  		, fct_POVCATYY
  		, ARTHDX:RAREDX
  		, ind_comorb)

  saveRDS(fyc, here::here("data_processed/fyc.RDS"))

  fyc

})


write.csv(
	fyc,
	here::here("data_processed/fyc_processed.csv"),
	row.names = FALSE
)
saveRDS(fyc, here::here("data_processed/fyc_processed.RDS"))


# some people are in post twice? Like 4k are in group >1x
fyc |>
	group_by(DUPERSID, pre_post) |>
	summarize(n=n()) |>
	filter(n>1) |>
	arrange(DUPERSID)







