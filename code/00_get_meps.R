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

# how to get one full dataset if needed:
# fyc2008_raw_full <- read_MEPS(year=2008, type = "FYC")

# Did we remove where missing? Check with 2008
fyc2008_raw_full |>
	select(PCS42, MCS42, ADGENH42, ADDAYA42, ADCLIM42, ADPALS42, ADPWLM42,
				 ADMALS42, ADMWLM42, ADPAIN42, ADCAPE42, ADNRGY42, ADDOWN42, ADSOCA42) |>
	View()


# Function for getting raw data and early processing
fn_raw <- function(year){
  # get raw data, only where age_last is 23-25 or 27-29
  df0 <- read_MEPS(year={{year}}, type = "FYC") |>
    filter(
      between(AGELAST, 23, 25) | between(AGELAST, 27, 29))

  year <- {{year}}

  # 2013 had different variables for education
  if (year < 2013) {
    df1 <- df0 |>
      select(
        DUID, PID, DUPERSID,
        starts_with(c("PERWT","SAQWT")),
        # for weighted sampling:
        VARSTR, VARPSU,
        # total expenditures
        starts_with("TOTEXP"),
        PANEL,
        # get insured, uninsured (DEB does uninsured...)
        starts_with(c("INSCOV","UNINS")),
        starts_with(c("INSCOP", "AGELAST", "MARRY", "FAMS", "FMRS1231")),
        PCS42, MCS42,
        starts_with("RACE"),
        SEX,
        HISPANX,
        # I think Deb used educyr for 'some college' and HIDEG doesn't have that
        EDUCYR, HIDEG,
        starts_with("POVCAT"),
        ARTHDX, ASTHDX, CANCERDX, CHOLDX, DIABDX, HIBPDX,
        ANGIDX, CHDDX, EMPHDX, MIDX, STRKDX
      )  |>
      mutate(year=year) |>
      relocate(year, .before = everything())

} else if (year>=2013){
    df1 <- df0 |>
      select(
        DUID, PID, DUPERSID,
        starts_with(c("PERWT","SAQWT")),
        # the two below from MEPS R pkg as id and strata to get weighted avgs?
        VARSTR, VARPSU,
        starts_with("TOTEXP"),
        PANEL,
        starts_with(c("INSCOV","UNINS")),
        starts_with(c("INSCOP", "AGELAST", "MARRY", "FAMS", "FMRS1231")),
        PCS42, MCS42,
        starts_with("RACE"),
        SEX,
        HISPANX,
        EDUYRDG, EDRECODE,
        starts_with("POVCAT"),
        ARTHDX, ASTHDX, CANCERDX, CHOLDX, DIABDX, HIBPDX,
        ANGIDX, CHDDX, EMPHDX, MIDX, STRKDX) |>
      mutate(year=year) |>
      relocate(year, .before = everything())
}

  # some of the processing that worked for all datasets:
  df2 <- df1 |>
    # in whole year:
    filter(if_all(c(INSCOP31, INSCOP42, INSCOP53), ~.==1)) |>
    # for famsize, also thinking negative values are not 'valid' but idk obvi
    # -- some famsize08 are -1 but others aren't neg, some famsize1231 are -1, but others arent
    # -- used FMRS1231
    filter(
      FMRS1231 > 0
    ) |>
    # Physical, Mental health not missing
    # -- PCS42 and MCS42
    filter(!is.na(PCS42)) |>
  	filter(PCS42 > -9) |>
  	filter(MCS42 > -9) |>
    filter(!is.na(MCS42)) |>
  	# Marry status: exclude -9 (not ascertained) and -7 (refused)
    filter(
      if_any(starts_with("MARRY"), ~ . > 0))

  # save results
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
    # create year variable
    mutate(pre_post = "pre") |>
    relocate(pre_post, .after = year) %>%
    # rename so you can join all dats
    rename(
      INSCOPYY = INSCOP08,
      TOTEXPYY = TOTEXP08,
      FAMSZEYY = FAMSZE08,
      POVCATYY = POVCAT08,
      MARRYYYX = MARRY08X,
      PERWTYYF = PERWT08F,
      SAQWTYYF = SAQWT08F,
      INSCOVYY = INSCOV08,
      UNINSYY  = UNINS08)

  fyc09 <- dats$fyc09_raw |>
    mutate(pre_post = "pre") |>
    relocate(pre_post, .after = year) |>
    rename(
      INSCOPYY = INSCOP09,
      TOTEXPYY = TOTEXP09,
      FAMSZEYY = FAMSZE09,
      POVCATYY = POVCAT09,
      MARRYYYX = MARRY09X,
      PERWTYYF = PERWT09F,
      SAQWTYYF = SAQWT09F,
      INSCOVYY = INSCOV09,
      UNINSYY  = UNINS09)

  fyc10 <- dats$fyc10_raw |>
    mutate(pre_post = "pre") |>
    relocate(pre_post, .after = year) |>
    rename(
      INSCOPYY = INSCOP10,
      TOTEXPYY = TOTEXP10,
      FAMSZEYY = FAMSZE10,
      POVCATYY = POVCAT10,
      MARRYYYX = MARRY10X,
      PERWTYYF = PERWT10F,
      SAQWTYYF = SAQWT10F,
      INSCOVYY = INSCOV10,
      UNINSYY  = UNINS10)

  fyc11 <- dats$fyc11_raw |>
    mutate(pre_post = "post") |>
    relocate(pre_post, .after = year) |>
    rename(
      INSCOPYY = INSCOP11,
      TOTEXPYY = TOTEXP11,
      FAMSZEYY = FAMSZE11,
      POVCATYY = POVCAT11,
      MARRYYYX = MARRY11X,
      PERWTYYF = PERWT11F,
      SAQWTYYF = SAQWT11F,
      INSCOVYY = INSCOV11,
      UNINSYY  = UNINS11)

  fyc12 <- dats$fyc12_raw |>
    mutate(pre_post = "post") |>
    relocate(pre_post, .after = year) |>
    rename(
      INSCOPYY = INSCOP12,
      TOTEXPYY = TOTEXP12,
      FAMSZEYY = FAMSZE12,
      POVCATYY = POVCAT12,
      MARRYYYX = MARRY12X,
      PERWTYYF = PERWT12F,
      SAQWTYYF = SAQWT12F,
      INSCOVYY = INSCOV12,
      UNINSYY  = UNINS12)

  fyc13 <- dats$fyc13_raw |>
    mutate(pre_post = "post") |>
    relocate(pre_post, .after = year) |>
    rename(
      INSCOPYY = INSCOP13,
      TOTEXPYY = TOTEXP13,
      FAMSZEYY = FAMSZE13,
      POVCATYY = POVCAT13,
      MARRYYYX = MARRY13X,
      PERWTYYF = PERWT13F,
      SAQWTYYF = SAQWT13F,
      INSCOVYY = INSCOV13,
      UNINSYY  = UNINS13)

  fyc14 <- dats$fyc14_raw |>
    mutate(pre_post = "post") |>
    relocate(pre_post, .after = year) |>
    rename(
      INSCOPYY = INSCOP14,
      TOTEXPYY = TOTEXP14,
      FAMSZEYY = FAMSZE14,
      POVCATYY = POVCAT14,
      MARRYYYX = MARRY14X,
      PERWTYYF = PERWT14F,
      SAQWTYYF = SAQWT14F,
      INSCOVYY = INSCOV14,
      UNINSYY  = UNINS14)

  fyc <- bind_rows(fyc08, fyc09, fyc10, fyc11, fyc12, fyc13, fyc14) |>
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
    select(-c(ANGIDX, CHDDX, EMPHDX, MIDX, STRKDX)) |>
    mutate(fct_POVCATYY = factor(
      POVCATYY,
      levels = c("1","2","3","4","5"),
      labels = c("Poor", "Near poor","Low income", "Middle income", "High income")
    ))
    # mutate(
    #   EDUC_r = case_when(
    #     (HIDEG==2 | HIDEG==3 | EDRECODE==13) ~ 1,
    #     (between(EDUCYR, 13, 17) | EDRECODE == 14) ~ 2,
    #     (EDUCYR==4 | EDRECODE == 15) ~ 3,
    #     TRUE ~ NA_real_),
    #   EDUC_r_fct = factor(
    #     EDUC_r,
    #     levels = c("1","2","3"),
    #     labels = c("High School education","Some college", "College graduate"))
  #18973 / ncol 52
  # TO DO: Fix education, others = do later

  saveRDS(fyc, here::here("data_processed/fyc.RDS"))

  fyc

})


fyc <- fyc |>
	#-9 means not ascertained (https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H121&varName=PCS42)
	# -1 means NA, but removing all those removes a ton... so it's not that (even though there are 6 in the >50k and that would fix my issue!)
	filter(PCS42>-9) |>
	filter(MCS42>-9)
# weight this by perwtYYf

fyc <- fyc |>
	mutate(z_TOTEXPYY = scale(TOTEXPYY)) |>
	dplyr::relocate(z_TOTEXPYY, .after = TOTEXPYY)

saveRDS(fyc, here::here("data_processed/fyc.RDS"))
write.csv(
	fyc,
	here::here("data_processed/fyc.csv"),
	na = "",
	row.names = FALSE)

