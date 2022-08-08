#load library...

#Get source for summarizing functions:
source("data_preparation/summarize_netcdf_functions.R")


############## ~~ Overall Conditions ~~ ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TDD
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Total_growing_degree_days")
tdd_hist_series <- read_ncdf("tdd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "tdd")
##### Read in climatologies
tdd_hist_clim <- read_ncdf("tdd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "tdd")
tdd_45_clim <- read_ncdf("tdd_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "tdd")
tdd_45_agree <- read_ncdf("tdd_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

tdd_hist <- get_his(tdd_hist_clim, tdd_hist_series)

tdd_fut <- get_fut(tdd_45_clim, tdd_hist_clim, tdd_45_agree, tdd_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TDD Longest Duration
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Warm_season_length")
tddld_hist_series <- read_ncdf("tddspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "tddspell")
##### Read in climatologies
tddld_hist_clim <- read_ncdf("tddspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "tddspell")
tddld_45_clim <- read_ncdf("tddspell_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "tddspell")
tddld_45_agree <- read_ncdf("tddspell_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

tddld_hist <- get_his(tddld_hist_clim, tddld_hist_series)

tddld_fut <- get_fut(tddld_45_clim, tddld_hist_clim, tddld_45_agree, tddld_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#WDD
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Wet_degree_days")
wdd_hist_series <- read_ncdf("wdd-100cm_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "wdd")
##### Read in climatologies
wdd_hist_clim <- read_ncdf("wdd-100cm_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "wdd")
wdd_45_clim <- read_ncdf("wdd-100cm_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "wdd")
wdd_45_agree <- read_ncdf("wdd-100cm_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

wdd_hist <- get_his(wdd_hist_clim, wdd_hist_series)

wdd_fut <- get_fut(wdd_45_clim, wdd_hist_clim, wdd_45_agree, wdd_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DDD
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Dry_degree_days")
ddd_hist_series <- read_ncdf("ddd-100cm_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "ddd")
##### Read in climatologies
ddd_hist_clim <- read_ncdf("ddd-100cm_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "ddd")
ddd_45_clim <- read_ncdf("ddd-100cm_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "ddd")
ddd_45_agree <- read_ncdf("ddd-100cm_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")


ddd_hist <- get_his(ddd_hist_clim, ddd_hist_series)

ddd_fut <- get_fut(ddd_45_clim, ddd_hist_clim, ddd_45_agree, ddd_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SWA
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Available_soil_moisture")
swa_hist_series <- read_ncdf("swa-100cm_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "swa")
##### Read in climatologies
swa_hist_clim <- read_ncdf("swa-100cm_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "swa")
swa_45_clim <- read_ncdf("swa-100cm_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "swa")
swa_45_agree <- read_ncdf("swa-100cm_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")


swa_hist <- get_his(swa_hist_clim, swa_hist_series)

swa_fut <- get_fut(swa_45_clim, swa_hist_clim, swa_45_agree, swa_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CWD
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Climatic_water_deficit")
cwd_hist_series <- read_ncdf("cwd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "cwd")
##### Read in climatologies
cwd_hist_clim <- read_ncdf("cwd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "cwd")
cwd_45_clim <- read_ncdf("cwd_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "cwd")
cwd_45_agree <- read_ncdf("cwd_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

cwd_hist <- get_his(cwd_hist_clim, cwd_hist_series)

cwd_fut <- get_fut(cwd_45_clim, cwd_hist_clim, cwd_45_agree, cwd_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Last Frost
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Timing_of_last_exposure_to_frost_before_warm_season")
last_hist_series <- read_ncdf("lastfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "lastfrost")
##### Read in climatologies
last_hist_clim <- read_ncdf("lastfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "lastfrost")
last_45_clim <- read_ncdf("lastfrost_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "lastfrost")
last_45_agree <- read_ncdf("lastfrost_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

last_hist <- get_his(last_hist_clim, last_hist_series)

last_fut <- get_fut(last_45_clim, last_hist_clim, last_45_agree, last_hist[2,,])

last_hist <- st_crop(last_hist, st_transform(extent_mask, st_crs(last_hist)), crop = FALSE)
last_fut[[1]] <- st_crop(last_fut[[1]], st_transform(extent_mask, st_crs(last_fut[[1]])), crop = FALSE)
last_fut[[2]] <- st_crop(last_fut[[2]], st_transform(extent_mask, st_crs(last_fut[[2]])), crop = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#First Frost
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Timing_of_first_exposure_to_frost_after_warm_season")
first_hist_series <- read_ncdf("firstfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "firstfrost")
##### Read in climatologies
first_hist_clim <- read_ncdf("firstfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "firstfrost")
first_45_clim <- read_ncdf("firstfrost_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "firstfrost")
first_45_agree <- read_ncdf("firstfrost_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

first_hist <- get_his(first_hist_clim, first_hist_series)

first_fut <- get_fut(first_45_clim, first_hist_clim, first_45_agree, first_hist[2,,])

first_hist <- st_crop(first_hist, st_transform(extent_mask, st_crs(first_hist)), crop = FALSE)
first_fut[[1]] <- st_crop(first_fut[[1]], st_transform(extent_mask, st_crs(first_fut[[1]])), crop = FALSE)
first_fut[[2]] <- st_crop(first_fut[[2]], st_transform(extent_mask, st_crs(first_fut[[2]])), crop = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




############## ~~ Seasonal Variability ~~ ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TDD Seasonal Variability
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Seasonal_variability_of_growing_degree_days")
tddseasvar_hist_series <- read_ncdf("tddseasvar_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "tddseasvar")
##### Read in climatologies
tddseasvar_hist_clim <- read_ncdf("tddseasvar_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "tddseasvar")
tddseasvar_45_clim <- read_ncdf("tddseasvar_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "tddseasvar")
tddseasvar_45_agree <- read_ncdf("tddseasvar_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

tddseasvar_hist <- get_his(tddseasvar_hist_clim, tddseasvar_hist_series)

tddseasvar_fut <- get_fut(tddseasvar_45_clim, tddseasvar_hist_clim, tddseasvar_45_agree, tddseasvar_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SWA SEASONAL VARIABILITY
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Seasonal_variability_of_available_soil_moisture")
swaseasvar_hist_series <- read_ncdf("swaseasvar_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "swaseasvar")
##### Read in climatologies
swaseasvar_hist_clim <- read_ncdf("swaseasvar_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "swaseasvar")
swaseasvar_45_clim <- read_ncdf("swaseasvar_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "swaseasvar")
swaseasvar_45_agree <- read_ncdf("swaseasvar_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

swaseasvar_hist <- get_his(swaseasvar_hist_clim, swaseasvar_hist_series)

swaseasvar_fut <- get_fut(swaseasvar_45_clim, swaseasvar_hist_clim, swaseasvar_45_agree, swaseasvar_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CWD Seasonal Variability
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Seasonal_variability_of_climatic_water_deficit")
cwdseasvar_hist_series <- read_ncdf("cwdseasvar_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "cwdseasvar")
##### Read in climatologies
cwdseasvar_hist_clim <- read_ncdf("cwdseasvar_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "cwdseasvar")
cwdseasvar_45_clim <- read_ncdf("cwdseasvar_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "cwdseasvar")
cwdseasvar_45_agree <- read_ncdf("cwdseasvar_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

cwdseasvar_hist <- get_his(cwdseasvar_hist_clim, cwdseasvar_hist_series)

cwdseasvar_fut <- get_fut(cwdseasvar_45_clim, cwdseasvar_hist_clim, cwdseasvar_45_agree, cwdseasvar_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




############## ~~ Seasonal Moisture Timing ~~ ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#WDD SEASONALITY
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Seasonal_timing_of_snowfree_wet_degree_days")
wddseas_hist_series <- read_ncdf("wddseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "wddseasonality")
##### Read in climatologies
wddseas_hist_clim <- read_ncdf("wddseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "wddseasonality")
wddseas_45_clim <- read_ncdf("wddseasonality_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "wddseasonality")
wddseas_45_agree <- read_ncdf("wddseasonality_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

wddseas_hist <- get_his(wddseas_hist_clim, wddseas_hist_series)

wddseas_fut <- get_fut(wddseas_45_clim, wddseas_hist_clim, wddseas_45_agree, wddseas_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SWA SEASONALITY
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Seasonal_timing_of_available_soil_moisture")
swaseas_hist_series <- read_ncdf("swaseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "swaseasonality")
##### Read in climatologies
swaseas_hist_clim <- read_ncdf("swaseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "swaseasonality")
swaseas_45_clim <- read_ncdf("swaseasonality_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "swaseasonality")
swaseas_45_agree <- read_ncdf("swaseasonality_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

swaseas_hist <- get_his(swaseas_hist_clim, swaseas_hist_series)

swaseas_fut <- get_fut(swaseas_45_clim, swaseas_hist_clim, swaseas_45_agree, swaseas_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PPT SEASONALITY
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Precipitation_seasonality")
ppts_hist_series <- read_ncdf("prseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "prseasonality")
##### Read in climatologies
ppts_hist_clim <- read_ncdf("prseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "prseasonality")
ppts_45_clim <- read_ncdf("prseasonality_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "prseasonality")
ppts_45_agree <- read_ncdf("prseasonality_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

ppts_hist <- get_his(ppts_hist_clim, ppts_hist_series)

ppts_fut <- get_fut(ppts_45_clim, ppts_hist_clim, ppts_45_agree, ppts_hist[2,,])

ppts_hist <- st_crop(ppts_hist, st_transform(extent_mask, st_crs(ppts_hist)), crop = FALSE)
ppts_fut[[1]] <- st_crop(ppts_fut[[1]], st_transform(extent_mask, st_crs(ppts_fut[[1]])), crop = FALSE)
ppts_fut[[2]] <- st_crop(ppts_fut[[2]], st_transform(extent_mask, st_crs(ppts_fut[[2]])), crop = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CWD SEASONALITY
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Seasonal_timing_of_climatic_water_deficit")
cwdseas_hist_series <- read_ncdf("cwdseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "cwdseasonality")
##### Read in climatologies
cwdseas_hist_clim <- read_ncdf("cwdseasonality_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "cwdseasonality")
cwdseas_45_clim <- read_ncdf("cwdseasonality_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "cwdseasonality")
cwdseas_45_agree <- read_ncdf("cwdseasonality_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

cwdseas_hist <- get_his(cwdseas_hist_clim, cwdseas_hist_series)

cwdseas_fut <- get_fut(cwdseas_45_clim, cwdseas_hist_clim, cwdseas_45_agree, cwdseas_hist[2,,])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




############## ~~ Extreme Hot-Dry Conditions ~~ ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CWD 10-Day Max
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Ten-day_maximum_climatic_water_deficit")
cwdmx_hist_series <- read_ncdf("cwdmx10d_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "cwdmx10d")
##### Read in climatologies
cwdmx_hist_clim <- read_ncdf("cwdmx10d_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "cwdmx10d")
cwdmx_45_clim <- read_ncdf("cwdmx10d_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "cwdmx10d")
cwdmx_45_agree <- read_ncdf("cwdmx10d_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

cwdmx_hist <- get_his(cwdmx_hist_clim, cwdmx_hist_series)

cwdmx_fut <- get_fut(cwdmx_45_clim, cwdmx_hist_clim, cwdmx_45_agree, cwdmx_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DDD Longest Duration
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Exposure_to_prolonged_snowfree_dry_degree_days")
dddsp_hist_series <- read_ncdf("dddspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "dddspell")
##### Read in climatologies
dddsp_hist_clim <- read_ncdf("dddspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "dddspell")
dddsp_45_clim <- read_ncdf("dddspell_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "dddspell")
dddsp_45_agree <- read_ncdf("dddspell_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

dddsp_hist <- get_his(dddsp_hist_clim, dddsp_hist_series)

dddsp_fut <- get_fut(dddsp_45_clim, dddsp_hist_clim, dddsp_45_agree, dddsp_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DSI Mean
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Spell_length_of_dry_soils")
dsi_hist_series <- read_ncdf("dsi_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "dsi")
##### Read in climatologies
dsi_hist_clim <- read_ncdf("dsi_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "dsi")
dsi_45_clim <- read_ncdf("dsi_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "dsi")
dsi_45_agree <- read_ncdf("dsi_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")


dsi_hist <- get_his(dsi_hist_clim, dsi_hist_series)

dsi_fut <- get_fut(dsi_45_clim, dsi_hist_clim, dsi_45_agree, dsi_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DSI Number
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Number_of_spells_of_dry_soils")
dsic_hist_series <- read_ncdf("dsicount_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "dsicount")
##### Read in climatologies
dsic_hist_clim <- read_ncdf("dsicount_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "dsicount")
dsic_45_clim <- read_ncdf("dsicount_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "dsicount")
dsic_45_agree <- read_ncdf("dsicount_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

dsic_hist <- get_his(dsic_hist_clim, dsic_hist_series)

dsic_fut <- get_fut(dsic_45_clim, dsic_hist_clim, dsic_45_agree, dsic_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




############## ~~ Recruitment ~~ ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Spring Recruitment Index
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Wet_degree_days_during_spring_recruitment_period")
sprec_hist_series <- read_ncdf("springrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "springrecruitwdd")
##### Read in climatologies
sprec_hist_clim <- read_ncdf("springrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "springrecruitwdd")
sprec_45_clim <- read_ncdf("springrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "springrecruitwdd")
sprec_45_series <- read_ncdf("springrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231.nc", var = "springrecruitwdd")
sprec_45_agree <- read_ncdf("springrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

sprec_hist <- get_his(sprec_hist_clim, sprec_hist_series)

sprec_fut <- get_fut(sprec_45_clim, sprec_hist_clim, sprec_45_agree, sprec_hist[[2]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fall Recruitment Index
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Wet_degree_days_during_fall_recruitment_period")
frec_hist_series <- read_ncdf("fallrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "fallrecruitwdd")
##### Read in climatologies
frec_hist_clim <- read_ncdf("fallrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "fallrecruitwdd")
frec_45_clim <- read_ncdf("fallrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "fallrecruitwdd")
frec_45_series <- read_ncdf("fallrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231.nc", var = "fallrecruitwdd")
frec_45_agree <- read_ncdf("fallrecruitwdd_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

frec_hist <- get_his(frec_hist_clim, frec_hist_series)

frec_fut <- get_fut(frec_45_clim, frec_hist_clim, frec_45_agree, frec_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Spring Recruitment Timing
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Timing_of_spring_recruitment_period")
sprecd_hist_series <- read_ncdf("springrecruitonset_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "springrecruitonset")
##### Read in climatologies
sprecd_hist_clim <- read_ncdf("springrecruitonset_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "springrecruitonset")
sprecd_45_clim <- read_ncdf("springrecruitonset_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "springrecruitonset")
sprecd_45_agree <- read_ncdf("springrecruitonset_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

sprecd_hist <- get_his(sprecd_hist_clim, sprecd_hist_series)

sprecd_fut <- get_fut(sprecd_45_clim, sprecd_hist_clim, sprecd_45_agree, sprecd_hist[2,,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Timing of Fall Recruitment
##### Set WD and read in 40-year netcdfs
setwd("/Users/dchenoweth/Dropbox/drought_metrics/Output_netCDF_20210830/Timing_of_fall_recruitment_period")
frecd_hist_series <- read_ncdf("fallrecruitonset_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", var = "fallrecruitonset")
##### Read in climatologies
frecd_hist_clim <- read_ncdf("fallrecruitonset_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", var = "fallrecruitonset")
frecd_45_clim <- read_ncdf("fallrecruitonset_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", var = "fallrecruitonset")
frecd_45_agree <- read_ncdf("fallrecruitonset_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", var = "Agreement")

frecd_hist <- get_his(frecd_hist_clim, frecd_hist_series)

frecd_fut <- get_fut(frecd_45_clim, frecd_hist_clim, frecd_45_agree, frecd_hist[2,,])


#Being plotted here--need to change this
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Number of Years with Recruitment (Spring)
prop <- function(x) {
       y <- sum(x)/40
       return(y)
}
sprec_hist_n <- st_apply(!is.na(sprec_hist_series), MARGIN = c("lon", "lat"), FUN= prop)
sprec_45_mid_n <- st_apply(!is.na(sprec_45_series[,,,1:40]), MARGIN = c("lon", "lat"), FUN= prop)
sprec_45_end_n <- st_apply(!is.na(sprec_45_series[,,,41:80]), MARGIN = c("lon", "lat"), FUN= prop)

sprec_n <- list(sprec_hist_n, sprec_45_mid_n, sprec_45_end_n)
sprec_n_1 <- st_crop(sprec_n[[1]], st_transform(extent_mask, st_crs(sprec_n[[1]])), crop = FALSE)
sprec_n_2 <- st_crop(sprec_n[[2]], st_transform(extent_mask, st_crs(sprec_n[[2]])), crop = FALSE)
sprec_n_3 <- st_crop(sprec_n[[3]], st_transform(extent_mask, st_crs(sprec_n[[3]])), crop = FALSE)
sprec_n <- list(sprec_n_1, sprec_n_2, sprec_n_3)

#Number of Years with Recruitment (Fall)
frec_hist_n <- st_apply(!is.na(frec_hist_series), MARGIN = c("lon", "lat"), FUN= prop)
frec_45_mid_n <- st_apply(!is.na(frec_45_series[,,,1:40]), MARGIN = c("lon", "lat"), FUN= prop)
frec_45_end_n <- st_apply(!is.na(frec_45_series[,,,41:80]), MARGIN = c("lon", "lat"), FUN= prop)

frec_n <- list(frec_hist_n, frec_45_mid_n, frec_45_end_n)
frec_n_1 <- st_crop(frec_n[[1]], st_transform(extent_mask, st_crs(frec_n[[1]])), crop = FALSE)
frec_n_2 <- st_crop(frec_n[[2]], st_transform(extent_mask, st_crs(frec_n[[2]])), crop = FALSE)
frec_n_3 <- st_crop(frec_n[[3]], st_transform(extent_mask, st_crs(frec_n[[3]])), crop = FALSE)
frec_n <- list(frec_n_1, frec_n_2, frec_n_3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Spring Recruitment Spell Length
##### Set WD and read in 40-year netcdfs
sprecspl_hist_series <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_spring_recruitment_period/springrecruitspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc",
                                  var = "springrecruitspell")
##### Read in climatologies
sprecspl_hist_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_spring_recruitment_period/springrecruitspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc",
                                var = "springrecruitspell")
sprecspl_45_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_spring_recruitment_periodspringrecruitspell_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", 
                              var = "springrecruitspell")
sprecspl_45_agree <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_spring_recruitment_period/springrecruitspell_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc",
                               var = "Agreement")

sprecspl_hist <- get_his(sprecspl_hist_clim, sprecspl_hist_series)

sprecspl_fut <- get_fut(sprecspl_45_clim, sprecspl_hist_clim, sprecspl_45_agree, sprecspl_hist[2,,])

st_transform(sprecspl_hist , st_crs(frec_n))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fall Recruitment Spell Length
##### Set WD and read in 40-year netcdfs
frecsl_hist_series <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_fall_recruitment_period/fallrecruitspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", 
                                var = "fallrecruitspell")
##### Read in climatologies
frecsl_hist_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_fall_recruitment_period/fallrecruitspell_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", 
                              var = "fallrecruitspell")
frecsl_45_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_fall_recruitment_period/fallrecruitspell_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", 
                            var = "fallrecruitspell")
frecsl_45_agree <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Spell_length_of_fall_recruitment_period/fallrecruitspell_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", 
                             var = "Agreement")

frecsl_hist <- get_his(frecsl_hist_clim, frecsl_hist_series)

frecsl_fut <- get_fut(frecsl_45_clim, frecsl_hist_clim, frecsl_45_agree, frecsl_hist[2,,])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############## ~~ Climate Metrics ~~ ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Yearly Temp
##### Set WD and read in 40-year netcdfs
tempy_hist_series <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Air_temperature/ta_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", 
                               var = "ta")
##### Read in climatologies
tempy_hist_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Air_temperatureta_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", 
                             var = "ta")
tempy_45_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Air_temperature/ta_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", 
                           var = "ta")
tempy_45_agree <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Air_temperature/ta_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", 
                            var = "Agreement")

tempy_hist <- get_his(tempy_hist_clim, tempy_hist_series)

tempy_fut <- get_fut(tempy_45_clim, tempy_hist_clim, tempy_45_agree, tempy_hist[2,,])

tempy_hist <- st_crop(tempy_hist, st_transform(extent_mask, st_crs(tempy_hist)), crop = FALSE)
tempy_fut[[1]] <- st_crop(tempy_fut[[1]], st_transform(extent_mask, st_crs(tempy_fut[[1]])), crop = FALSE)
tempy_fut[[2]] <- st_crop(tempy_fut[[2]], st_transform(extent_mask, st_crs(tempy_fut[[2]])), crop = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Yearly Precip
##### Set WD and read in 40-year netcdfs
precy_hist_series <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Precipitation_amount/pr_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", 
                               var = "pr")
##### Read in climatologies
precy_hist_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Precipitation_amount/pr_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", 
                             var = "pr")
precy_45_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Precipitation_amount/pr_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", 
                           var = "pr")
precy_45_agree <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Precipitation_amount/pr_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", 
                            var = "Agreement")

precy_hist <- get_his(precy_hist_clim, precy_hist_series)

precy_fut <- get_fut(precy_45_clim, precy_hist_clim, precy_45_agree, precy_hist[2,,])

precy_hist <- st_crop(precy_hist, st_transform(extent_mask, st_crs(precy_hist)), crop = FALSE)
precy_fut[[1]] <- st_crop(precy_fut[[1]], st_transform(extent_mask, st_crs(precy_fut[[1]])), crop = FALSE)
precy_fut[[2]] <- st_crop(precy_fut[[2]], st_transform(extent_mask, st_crs(precy_fut[[2]])), crop = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Frequency of occurrence of first and last frost
##### Set WD and read in 40-year netcdfs
#First frost
fff_hist_series <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_after_warm_season/hasfirstfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", 
                             var = "hasfirstfrost")
##### Read in climatologies
fff_hist_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_after_warm_season/hasfirstfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", 
                           var = "hasfirstfrost")
fff_45_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_after_warm_season/hasfirstfrost_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", 
                         var = "hasfirstfrost")
fff_45_agree <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_after_warm_seasonhasfirstfrost_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", 
                          var = "Agreement")

fff_hist <- get_his(fff_hist_clim, fff_hist_series)

fff_fut <- get_fut(fff_45_clim, fff_hist_clim, fff_45_agree, fff_hist[2,,])

fff_hist <- st_crop(fff_hist, st_transform(extent_mask, st_crs(fff_hist)), crop = FALSE)
fff_fut[[1]] <- st_crop(fff_fut[[1]], st_transform(extent_mask, st_crs(fff_fut[[1]])), crop = FALSE)
fff_fut[[2]] <- st_crop(fff_fut[[2]], st_transform(extent_mask, st_crs(fff_fut[[2]])), crop = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Last frost
flf_hist_series <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_before_warm_season/haslastfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", 
                             var = "haslastfrost")
##### Read in climatologies
flf_hist_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_before_warm_seasonhaslastfrost_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", 
                           var = "haslastfrost")
flf_45_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_before_warm_season/haslastfrost_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", 
                         var = "haslastfrost")
flf_45_agree <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Number_of_years_with_an_exposure_to_frost_before_warm_season/haslastfrost_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", 
                          var = "Agreement")

flf_hist <- get_his(flf_hist_clim, flf_hist_series)

flf_fut <- get_fut(flf_45_clim, flf_hist_clim, flf_45_agree, flf_hist[2,,])

flf_hist <- st_crop(flf_hist, st_transform(extent_mask, st_crs(flf_hist)), crop = FALSE)
flf_fut[[1]] <- st_crop(flf_fut[[1]], st_transform(extent_mask, st_crs(flf_fut[[1]])), crop = FALSE)
flf_fut[[2]] <- st_crop(flf_fut[[2]], st_transform(extent_mask, st_crs(flf_fut[[2]])), crop = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Aridity index
ai_hist_series <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Aridity_Index/ai_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc", 
                            var = "ai")
##### Read in climatologies
ai_hist_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Aridity_Index/ai_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc", 
                          var = "ai")
ai_45_clim <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Aridity_Index/ai_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc", 
                        var = "ai")
ai_45_agree <- read_ncdf("inputs/soilwat_output/historical_weather/Output_netCDF_20210830/Aridity_Index/ai_yr_SOILWAT2_RangeDroughtExposure_agreement_RCP45_gn_20210101-21001231-clim.nc", 
                         var = "Agreement")

ai_hist <- get_his(ai_hist_clim, ai_hist_series)

ai_fut <- get_fut(ai_45_clim, ai_hist_clim, ai_45_agree, ai_hist[2,,])

ai_hist <- st_crop(ai_hist, st_transform(extent_mask, st_crs(ai_hist)), crop = FALSE)
ai_fut[[1]] <- st_crop(ai_fut[[1]], st_transform(extent_mask, st_crs(ai_fut[[1]])), crop = FALSE)
ai_fut[[2]] <- st_crop(ai_fut[[2]], st_transform(extent_mask, st_crs(ai_fut[[2]])), crop = FALSE)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#save and load metric objects
save(tempy_hist,
     precy_hist,
     ai_hist,
     file = "data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/climate")

save(tdd_hist,
     tddld_hist,
     wdd_hist,
     ddd_hist,
     swa_hist,
     cwd_hist,
     first_hist,
     last_hist,
     fff_hist,
     flf_hist,
     file = "data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/overall.conditions")

save(tddseasvar_hist,
     swaseasvar_hist,
     cwdseasvar_hist,
     file = "data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/seasonal.variability")

save(wddseas_hist,
     swaseas_hist,
     ppts_hist,
     cwdseas_hist,
     file = "data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/moisture.seasonality")

save(cwdmx_hist,
     dddsp_hist,
     dsi_hist,
     dsic_hist,
     file = "data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/extreme.drought.conditions")

save(sprec_hist,
     sprecd_hist,
     frec_hist,
     frecd_hist,
     sprec_n,
     frec_n,
     sprecspl_hist,
     frecsl_hist,
     file = "data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/recruitment.conditions")

load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/climate")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/overall.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/seasonal.variability")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/moisture.seasonality")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/extreme.drought.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/recruitment.conditions")



