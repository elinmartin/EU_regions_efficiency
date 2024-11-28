
# -----------------------------
#
# Written by: Elin Martinsson
# 2024
#
# -----------------------------


# clear environment and set wd
rm(list = ls(all.names = TRUE))
getwd()



# ---------------------
#   LOAD PACKAGES     #
# ---------------------
library(readxl)
library(dplyr)
library(plm)
library(lpSolve)
library(Benchmarking)
library(tidyverse)
library(readxl)
library(vegan)
library(ggpubr)
library(ggplot2)
library(ggrepel)



# -----------------------------
# Step 1: Data preparation
# -----------------------------

###############################
# LOAD, FORMAT AND MERGE DATA #

## EUROSTAT DATA
eurostat <- read.csv("used_data/newagr_r_accts__custom_10483797_linear.csv.gz")
# Process the eurostat data
eurostat <- eurostat %>%
  # Replace missing OBS_VALUE (when OBS_FLAG is 'z') with 0
  mutate(OBS_VALUE = if_else(OBS_FLAG == "z" & is.na(OBS_VALUE), 0, OBS_VALUE)) %>% 
  # Remove the OBS_FLAG column
  select(-OBS_FLAG) %>% 
  # Filter the data for specific TIME_PERIOD and itm_newa values
  filter(TIME_PERIOD >= 2020, 
         itm_newa == 14000 | itm_newa == 1000 | itm_newa == 2000 | itm_newa == 3000 | 
           itm_newa == 4000 | itm_newa == 7000 | itm_newa == 8000 | itm_newa == 5000 | 
           itm_newa == 13000 | itm_newa == 10000 | itm_newa == 26000| itm_newa == 6000) %>% 
  # Pivot the data to make itm_newa values as column names 
  #and their corresponding OBS_VALUE as values
  pivot_wider(names_from = itm_newa, values_from = OBS_VALUE) %>%
  # Rename the columns for better readability
  rename(
    NUTS_ID = geo,        # Rename geo to NUTS_ID
    crop_output = '10000', # Rename '10000' to crop_output
    agri_output = '14000', # Rename '14000' to agri_output
    factor_inc = '26000', # Rename '26000' to factor_inc
    animal = '13000',      # Rename '13000' to animal
    wine = '7000',
    fruits = '6000',
    olive_oil = '8000',
    vegetables = '4000',
    potato = '5000'
  )

# CODES --------------
# 10000 - CROP OUTPUT
# 01000 - CEREALS
# 02000 - INDUSTRIAL CROPS
# 03000 - FORAGE PLANTS
# 04000 - FRUIT
# 05000 - POTATOES (including seeds)
# 07000 - WINE
# 08000 - OLIVE OIL
# ---------------------

# our_data1 only contains the variables from eurostat used in the analysis
our_data1 <- eurostat %>% 
  filter(TIME_PERIOD == 2020) %>% select(NUTS_ID, agri_output, animal, crop_output)

# -------------------------
# Impute some missing values

# Impute values with 2021 where 2020 is missing 
our_data2 <- eurostat %>% filter(TIME_PERIOD == 2021) %>% 
  select(NUTS_ID, agri_output, animal, crop_output) %>%
  rename(agri_output_21 = agri_output, animal_21 = animal, crop_output_21 = crop_output)

# merge 
our_data1 <- full_join(our_data1, our_data2, by = "NUTS_ID")

# when data is missing for animal, use the value for animal_21
our_data1$animal[is.na(our_data1$animal)] <- our_data1$animal_21[is.na(our_data1$animal)]
# when data is missing for crop_output, use value for crop_output_21
our_data1$crop_output[is.na(our_data1$crop_output)] <- our_data1$crop_output_21[is.na(our_data1$crop_output)]
# when data is missing for agri_output, use value for agri_output_21
our_data1$agri_output[is.na(our_data1$agri_output)] <- our_data1$agri_output_21[is.na(our_data1$agri_output)]

# 14 values imputed (XX of which are included in the analysis)

# -----------------------
# remove observations given our inclusion criteria 
# - Region should be located in Europe and be a member of the EU (remove Switzerland,
#   Norway and Iceland)
# - Region should have an agricultural production of more than 500 millions of EUR
our_data1 <- our_data1 %>% 
  filter(agri_output >= 200,
         !(NUTS_ID %in% c("ES70", "ES7", "FRY1", "FRY2", "FRA2", 
                          "FR92", "FRY3", "FRA3", "FR93", "FRY4", "FRY5",
                          "MT", "MT00", "PT20", "PT30", "FRY")),
         !(grepl("^NO", NUTS_ID) | grepl("^CH", NUTS_ID)| grepl("^IS", NUTS_ID)))

# 186 regions fulfilling our inclusion criteria.
# NUTS IDs outside of Europe:
# canaria (ES70), guadeloupe (FRY1), martinique (FRY2, FRA2, FR92), guyane (FRY3, FRA3, FR93), 
# la reunion (FRY4), mayotte (FRY5), malta (MT, MT00), acores (PT20), madeira (PT30),
# Régions Ultrapériphériques Françaises (FRY)

# -----------------------
# Later, we need indicators for wine, veggies, olive oil and fruits to identify
# the regions specialising in high-value crops. We will use the data from eurostat.
#
# As many regions are not producing any or some of these goods, we assume missing
# values to be zero.

our_data3 <- eurostat %>% 
  filter(TIME_PERIOD == 2020) %>% select(NUTS_ID, wine, vegetables, olive_oil, fruits, potato) %>%
  mutate(wine = ifelse(is.na(wine), 0, wine),
         vegetables = ifelse(is.na(vegetables), 0, vegetables),
         olive_oil = ifelse(is.na(olive_oil), 0, olive_oil),
         fruits = ifelse(is.na(fruits), 0, fruits),
         potato = ifelse(is.na(potato), 0, potato))
# -----------------------

# -----------------------
# Net value added

nva <- read.csv("used_data/net_value_added.gz")
# format the data such that each TIME_PERIOD is one column with the value from OBS_VALUE
nva <- nva %>% pivot_wider(names_from = TIME_PERIOD, values_from = OBS_VALUE)
# impute the missing values in 2020 with 2019, and if 2019 is also missing then use 2021
nva$`2020`[is.na(nva$`2020`)] <- nva$`2019`[is.na(nva$`2020`)]
nva$`2020`[is.na(nva$`2020`)] <- nva$`2021`[is.na(nva$`2020`)]
# only keep NUTS_ID and the value for 2020, rename 2020 to nva
nva <- nva %>% select(c(geo, `2020`)) %>% rename(NUTS_ID = 'geo', net_value_added = '2020')

# add nva to our_data1
our_data1 <- left_join(our_data1, nva, by = "NUTS_ID") %>% na.omit()

# remove observations with nva = 0 or < 0
our_data1 <- our_data1 %>% filter(net_value_added > 0)
# 25 observations removed with net value added < 0
# ---------------------------------------------------

# ---------------------------------------------------
# Data on UAA and different crops grown

uaa_crop <- read.csv("used_data/ef_lus_allcrops__custom_10382410_linear.csv.gz") 
arable <- uaa_crop %>% select(-c(OBS_FLAG, DATAFLOW, LAST.UPDATE, freq, so_eur,
                                 uaarea, TIME_PERIOD, unit)) %>% 
  pivot_wider(names_from = crops, values_from = OBS_VALUE) %>% 
  rename(arable_l = 'ARA', fallow = 'Q0000', cereals = 'C0000', prot_crop = 'P0000', 
         root_crop = 'R0000', ind_crop = 'I0000', plants  = 'G0000', veggies = 'V0000_S0000', 
         flowers = 'N0000', seed = 'E0000', other = 'ARA99', NUTS_ID = 'geo')

# ---------------------------------------------------
# data on organic farming and UAA

organic_data <- read.csv("used_data/NEWWWef_lus_main__custom_10486751_linear.csv.gz")
organic <- organic_data %>% filter(TIME_PERIOD == 2020) %>% subset(select = c(crops, geo, OBS_VALUE)) %>% 
  pivot_wider(names_from = crops, values_from = OBS_VALUE) %>%
  rename(organic = 'UAAXK0000_ORG', NUTS_ID = 'geo') %>%
  select(NUTS_ID, organic)

# ---------------------------------------------------
# GHG data from EDGAR (European Database for Global Atmospheric Research)

edgar_ch4 <- read_excel("used_data/EDGAR_GHG_NUTS2_by_sector_1990_2021.xlsx", sheet = "CH4 (AR4)", skip = 7)
edgar_n20 <- read_excel("used_data/EDGAR_GHG_NUTS2_by_sector_1990_2021.xlsx", sheet = "N2O (AR4)", skip = 7)
ch4 <- select(edgar_ch4, NUTS_ID, sector, TIME_PERIOD = '2020')
n20 <- select(edgar_n20, NUTS_ID, sector, TIME_PERIOD = '2020')

ch4_enteric <- ch4 %>% filter(sector == "ENF") %>% mutate(ENF = TIME_PERIOD)
ch4_manure <- ch4 %>% filter(sector == "MNM" ) %>% mutate(MNM_ch4 = TIME_PERIOD)
n2o_manure <- n20 %>% filter(sector == "MNM") %>% mutate(MNM_n2o = TIME_PERIOD)
n20_soil <- n20 %>% filter(sector == "AGS") %>% mutate(AGS = TIME_PERIOD)

# we suspect there are duplicates and check for this
ch4_enteric$NUTS_ID[duplicated(ch4_enteric$NUTS_ID)]
ch4_manure$NUTS_ID[duplicated(ch4_manure$NUTS_ID)]
n2o_manure$NUTS_ID[duplicated(n2o_manure$NUTS_ID)]
n20_soil$NUTS_ID[duplicated(n20_soil$NUTS_ID)]
# [1] "FRY2" "FRY4" "FRY1" "FRY3"

# we remove the duplicates
ch4_enteric_nodup <- ch4_enteric[!duplicated(ch4_enteric$NUTS_ID), ]
ch4_manure_nodup <- ch4_manure[!duplicated(ch4_manure$NUTS_ID), ]
n2o_manure_nodup <- n2o_manure[!duplicated(n2o_manure$NUTS_ID), ]
n20_soil_nodup <- n20_soil[!duplicated(n20_soil$NUTS_ID), ]

ghg <- full_join(full_join
                 (full_join(ch4_enteric_nodup, ch4_manure_nodup, by = "NUTS_ID"), 
                   n2o_manure_nodup, by = "NUTS_ID"),
                 n20_soil_nodup, by = "NUTS_ID") %>% select(c(NUTS_ID, ENF, MNM_ch4, MNM_n2o,AGS))


# ---------------------------------------------------
# Social indicators
# ---------------------------------------------------

# ---------------------------------------------------
# Data on age of farmer
age_class <- read.csv("used_data/age_class.gz")
age <- age_class %>% filter(TIME_PERIOD == 2020) %>% subset(select = c(age, geo, OBS_VALUE)) %>% 
  pivot_wider(names_from = age, values_from = OBS_VALUE) %>% select(-c('Y35-44')) %>%
  rename(NUTS_ID = 'geo')

# ---------------------------------------------------
# gender of farm manager
gender_class <- read.csv("used_data/gender.gz")
gender <- gender_class %>% filter(TIME_PERIOD == 2020) %>% pivot_wider(names_from = sex, values_from = OBS_VALUE) %>% 
  rename(NUTS_ID = 'geo') %>%
  select(c(NUTS_ID, F, M))

# ---------------------------------------------------
# education of farm manager
training_class <- read.csv("used_data/training.gz")
training <- training_class %>% filter(TIME_PERIOD == 2020) %>% pivot_wider(names_from = training, values_from = OBS_VALUE) %>% 
  rename(NUTS_ID = 'geo') %>%
  select(c(NUTS_ID, BASIC, FULL, PRACT))

# ---------------------------------------------------
# annual working unit
workers_16_20 <- read.csv("used_data/worker_2016_20.gz")
# annual working unit
# 2016 & 2020

# AWU (annual working unit)
workers <- workers_16_20 %>%
  mutate(NUTS_ID = geo, awu = OBS_VALUE) %>%
  select(NUTS_ID, awu, TIME_PERIOD) %>%
  pivot_wider(names_from = TIME_PERIOD, values_from = awu) %>%
  mutate(`2020` = ifelse(is.na(`2020`), `2016`, `2020`)) %>% # impute missing values
  select(NUTS_ID, awu = `2020`) # only keep 2020

# ---------------------------------------------------
# These were all data on regional level
#
# --> merge all regional data
#

list1 <- list(our_data1, arable, organic, ghg, age, gender, training, workers)
regional_level <- list1 %>% reduce(left_join, by=('NUTS_ID'))# %>% na.omit()

# ---------------------------------------------------
#### MISSING VALUES 
# arable land: there are some missing values on usage of arable land which we can impute
# we know that: 
# arable_l = other + cereals + seeds + plants + inc_crops + flowers + prot_crops + fallow + root_crops + veggies
# we can use this to impute values

# imput NAs in 'other' by subtracting all other goods from arable land. 
# OBS: Only for regions where this is the only NA.
regional_level <- regional_level %>%
  mutate(other = if_else(is.na(other) & 
                           !is.na(arable_l) & !is.na(cereals) & !is.na(seed) & !is.na(plants) &
                           !is.na(ind_crop) & !is.na(flowers) & !is.na(prot_crop) & !is.na(fallow) &
                           !is.na(root_crop) & !is.na(veggies), arable_l - 
                           (cereals + seed + plants + ind_crop + flowers + prot_crop + fallow + 
                              root_crop + veggies), other))

# One obs is negative, we set it to zero
regional_level$other[regional_level$other < 0] <- 0

# input NAs in 'flower' by subtracting all other goods from arable land. Only for regions where this is the only NA (1 obs).
regional_level <- regional_level %>%
  mutate(flowers = if_else(is.na(flowers) & 
                             !is.na(arable_l) & !is.na(cereals) & !is.na(seed) & !is.na(plants) &
                             !is.na(ind_crop) & !is.na(other) & !is.na(prot_crop) & !is.na(fallow) &
                             !is.na(root_crop) & !is.na(veggies), arable_l - 
                             (cereals + seed + plants + ind_crop + other + prot_crop + fallow + 
                                root_crop + veggies), flowers))


# imput NAs in 'prot_crop' by substracting all other goods from arable land. 
# OBS: Only for regions where this is the only NA (1 obs).
regional_level <- regional_level %>%
  mutate(prot_crop = if_else(is.na(prot_crop) & 
                               !is.na(arable_l) & !is.na(cereals) & !is.na(seed) & !is.na(plants) &
                               !is.na(ind_crop) & !is.na(other) & !is.na(flowers) & !is.na(fallow) &
                               !is.na(root_crop) & !is.na(veggies), arable_l - 
                               (cereals + seed + plants + ind_crop + other + flowers + fallow + 
                                  root_crop + veggies), prot_crop))

# impute NAs in 'seed' by substracting all other goods from arable land. 
# OBS: Only for regions where this is the only NA (1 obs).
regional_level <- regional_level %>%
  mutate(seed = if_else(is.na(seed) & 
                          !is.na(arable_l) & !is.na(cereals) & !is.na(prot_crop) & !is.na(plants) &
                          !is.na(ind_crop) & !is.na(other) & !is.na(flowers) & !is.na(fallow) &
                          !is.na(root_crop) & !is.na(veggies), arable_l - 
                          (cereals + prot_crop + plants + ind_crop + other + flowers + fallow + 
                             root_crop + veggies), seed))
# impute one negative value in 'seed' by 0
regional_level$seed[regional_level$seed < 0] <- 0

# we set missing values for seed and flowers to zero 
# (1 obs, the usages which are not missing add up approximately (-10) to the arable land used)
regional_level <- regional_level %>%
  mutate(seed = if_else(is.na(seed), 0, seed),
         flowers = if_else(is.na(flowers), 0, flowers))


# impute missing data on age group Y_LT25 by subtracting all other age groups from the total number of farm managers
regional_level$total_farmers <- regional_level$F + regional_level$M # get the total number of farm managers by adding up male and female
regional_level <- regional_level %>%
  mutate(Y_LT25 = if_else(is.na(Y_LT25), total_farmers - `Y25-34` - `Y35-39` - `Y40-44` - `Y45-54` - `Y55-64` - `Y_GE65`, `Y_LT25`))

# ------------------------------------------------------------------------------
# COUNTRY LEVEL DATA
# ------------------------------------------------------------------------------

# ----------------------------
# Ammonia emissions
ammonia_national <- read.csv("used_data/sdg_02_60_page_linear.csv.gz")

ammonia_national <- ammonia_national %>% mutate(CNTR_CODE = geo, ammonia = OBS_VALUE) %>%
  filter(TIME_PERIOD == 2020)%>%
  select(c(CNTR_CODE, ammonia))

# ----------------------------
# pesticide risk indicator 
pesticides_risk_inidcator <- read.csv("used_data/pesticides_risk_inidcator.gz")
pesticide <- pesticides_risk_inidcator %>% mutate(CNTR_CODE = geo, pesti_risk = OBS_VALUE) %>%
  filter(TIME_PERIOD == 2020) %>%
  select(c(CNTR_CODE, pesti_risk))

# ----------------------------
# Nitrates in groundwater

nitrates_groundwater <- read_excel("used_data/nitrates_groundwater_lvls.xlsx")
nitrates <- nitrates_groundwater %>% select(c(Country, `Class 4 (≥50 mg/l)`)) %>%
  rename(CNTR_Name = Country, high_nitrate = `Class 4 (≥50 mg/l)`)
# ----------------------------
# Antibiotics

antibiotics_data <- read_excel("used_data/antibiotics_data.xlsx", sheet = "Data", skip = 1)
antibiotics <- antibiotics_data %>% select(c(Country, `All others, Tonnes`)) %>%
  rename(antibiotics_all = `All others, Tonnes`, CNTR_Name = Country) 

# change name for consistency with the IDs of the other datasets
antibiotics$CNTR_Name[6] <- "Czechia"

# ----------------------------
# Country-level UAA

uaa_national <- read.csv("used_data/uaa_country.csv.gz")
uaa_national <- uaa_national %>% mutate(CNTR_CODE = geo, UAA_nation = OBS_VALUE) %>%
  filter(TIME_PERIOD == 2020) %>%
  select(c(CNTR_CODE, UAA_nation))

# ----------------------------
# Livestock units on a country level

LU_national <- read.csv("used_data/LU_national.gz")
LU_national <- LU_national %>% mutate(CNTR_CODE = geo, LSU_national = OBS_VALUE) %>%
  filter(TIME_PERIOD == 2020) %>%
  select(c(CNTR_CODE, LSU_national))


# ----------------------------
# purchasing power parity (to adjust income to make comparable between countries)

ppp_eurostat <- read.csv("used_data/ppp_eu.gz")
ppp_eu <- ppp_eurostat %>% mutate(CNTR_CODE = geo, ppp = OBS_VALUE) %>%
  filter(TIME_PERIOD == 2020) %>%
  select(c(CNTR_CODE, ppp))


# ------------------------------------------------------------------------------
# merge all country data

# first we need to associate the CNTR_CODE with the CNTR_NAME
codes <- data.frame(
  CNTR_CODE = c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
                "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                "NL", "PL", "PT", "RO", "SE", "SI", "SK", "CH", "NO"),
  CNTR_Name = c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", 
                "Germany", "Denmark", "Estonia", "Greece", "Spain", 
                "Finland", "France", "Croatia", "Hungary", "Ireland", 
                "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", 
                "Netherlands", "Poland", "Portugal", "Romania", "Sweden", 
                "Slovenia", "Slovakia", "Switzerland", "Norway"))

# create a dataset connecting the NUTS2 with the county_IDs
country_code <- subset(edgar_ch4, select = c(NUTS_ID, CNTR_CODE, NAME_LATN))
country_code <- unique(country_code)

all_codes <- full_join(codes, country_code)

ammonia_cntry <- full_join(ammonia_national, all_codes)

# we first merge ammonia with uaa_national because they both contain the "CNTR_CODE" variable
ammonia_uaa <- left_join(ammonia_cntry, uaa_national, by = "CNTR_CODE")
ammonia_uaa_LU <- left_join(ammonia_uaa, LU_national, by = "CNTR_CODE")
ammonia_uaa_LU_ppp <- left_join(ammonia_uaa_LU, ppp_eu, by = "CNTR_CODE")
ammonia_uaa_lu_ppp_pesti <- left_join(ammonia_uaa_LU_ppp, pesticide, by = "CNTR_CODE")

list1 <- list(ammonia_uaa_lu_ppp_pesti, nitrates, antibiotics)
country_level <- list1 %>% reduce(left_join, by=('CNTR_Name')) %>% na.omit()
# na.omit will omit Switzerland, Norway, Iceland, Luxembourg and Turkey, which is not in the regional data


# ----------------------------------
# Cleaning up the environment
rm(list = ls()[!ls() %in% c("regional_level", "country_level", "our_data3")])

# ----------------------------------
# Merge the regional and country data

total_data <- full_join(regional_level, country_level)  %>% na.omit()
# 158


# ----------------------------------
# Step 2: Defining indicators
# ----------------------------------

# Value added per hectare (expressed in 1000 EUR. to start with it is expressed in Mio)
total_data$value_prod <- (1000*total_data$net_value_added) / total_data$UAA

# ----------------------------------
# ENVIRONMENTAL INDICATORS

# GHG emissions per hectare (expressed in 1000kg CO2 (tonnes) equivalents)
total_data$ghg_uaa <- (total_data$ENF + total_data$MNM_ch4 + total_data$MNM_n2o + total_data$AGS)*1000 / total_data$UAA

# share of conventional farms (% of UAA)
total_data$conv_share <- ((total_data$UAA - total_data$organic) / total_data$UAA)
#total_data$conv_share <- total_data$conv_share * 100

# cropland diversity
arable_shannon <- subset(total_data, select = c("fallow", "cereals", "prot_crop", 
                                                "root_crop", "ind_crop", "plants", 
                                                "veggies", "flowers", "seed", 
                                                "other"))
#this dataset only contains the 10 different types of arable farming, which we 
# consider when computing the shannon diversity index
# Compute the simpson index:
total_data$shannon <- diversity(arable_shannon, index = "shannon")

total_data$div <- max(total_data$shannon) - total_data$shannon 

# to avoid zero-values, add the second largest number in the dataset to the observation with zero
total_data$div[total_data$div == 0] <- 0.013

# nitrate 
total_data$high_nitrate <- as.numeric(total_data$high_nitrate)

# ----------------------------------
# SOCIAL INDICATORS

# share of female farmers 
total_data$female_ratio <- total_data$F / total_data$total_farmers

# age ratio
total_data$age_ratio <- (total_data$Y_LT25 + total_data$`Y25-34`) / total_data$Y_GE65

# share of fully educated farmers 
total_data$share_full <- (total_data$FULL) / (total_data$PRACT + total_data$FULL + total_data$BASIC) 

# we rescale the ppp such that >1 indicate expensive countries and <1 cheap countries
total_data$ppp <- total_data$ppp / 100

# take the nominal wage divided by the PPP to get the real wage (reflecting what you can
# buy for the wage in the country)
total_data$nv_ppp <- total_data$net_value_added*100 / total_data$ppp 

# we need to make sure value added is positive. 1 OBS is negative so we remove this one
total_data <- total_data %>% filter(net_value_added > 0)

total_data$inc_awu <- total_data$nv_ppp / total_data$awu
# awu expressed in 100 workers.
# nv_ppp expressed in 1000 EUR

# antibiotics per LU
total_data$antibiotics_lu <- total_data$antibiotics_all*1000000 / total_data$LSU_national # grams per animal
# inverse of antibiotic usage
total_data$antibiotics_inv <- (max(total_data$antibiotics_lu) - (total_data$antibiotics_lu))
total_data$antibiotics_inv[total_data$antibiotics_inv == 0] <- 1
# the observation with the largest use of antibiotics (where max - max = 0) is
# manually assigned the value 1 (very small given the magnitude of the data) which
# means that this region barely provides any of this social good

## add the production of high-value goods to the dataframe
total_data <- left_join(total_data, our_data3, by = "NUTS_ID")

# -------------------------------------------
# Define the data-frame used for the analysis

# create a dataframe with only the variables we use for the analysis
total_data1 <- total_data %>% select(c(NUTS_ID, value_prod, ghg_uaa, conv_share, div, ammonia, pesti_risk, high_nitrate,
                                       female_ratio, age_ratio, share_full, inc_awu, antibiotics_inv, crop_output, 
                                       animal, agri_output, UAA))

social_data1 <- total_data %>% select(c(NUTS_ID, value_prod, female_ratio, age_ratio, 
                                        share_full, inc_awu, antibiotics_inv))


# ----------------------------------
# DATA PREPARATION FOR DEA
# ----------------------------------
# We standardize the variables using max-min standardisation (thus we
# do not obtain any negative values)
# we do this for the weight restrictions to make sense

# net value added
total_data$value_prod <- total_data$value_prod / sd(total_data$value_prod)

# environmental variables
total_data$ghg_uaa <- total_data$ghg_uaa / sd(total_data$ghg_uaa)
total_data$conv_share <- total_data$conv_share / sd(total_data$conv_share)
total_data$div <- total_data$div / sd(total_data$div)
total_data$ammonia <- total_data$ammonia / sd(total_data$ammonia)
total_data$pesti_risk <- total_data$pesti_risk / sd(total_data$pesti_risk)
total_data$high_nitrate <- total_data$high_nitrate / sd(total_data$high_nitrate)

# social variables
total_data$female_ratio <- total_data$female_ratio / sd(total_data$female_ratio)
total_data$age_ratio <- total_data$age_ratio / sd(total_data$age_ratio)
total_data$share_full <- total_data$share_full / sd(total_data$share_full)
total_data$inc_awu <- total_data$inc_awu / sd(total_data$inc_awu)
total_data$antibiotics_inv <- total_data$antibiotics_inv / sd(total_data$antibiotics_inv)
# ----------------------------------

# -----------------------------------------------------------------------------
# Create functions for assessing efficiency 
# as the packages in R to assess efficiency does not allow for manually specifying
# weigh restrictions, we set this up manually.

dea_in_fixed_weights <- function(x, y, eff, data) {
  
  eff <- numeric(nrow(data))
  weights <- matrix(0, nrow = nrow(data), ncol = 6)
  
  for (i in 1:nrow(x)) { 
    f.obj <- x[i, ] / y[i]
    
    # Create the constraint matrix
    # Initialize an empty list to store constraints
    constraints <- list()
    
    # Efficiency constraints: (x / y) for each crop_high
    for (j in 1:nrow(x)) {     
      constraints[[j]] <- x[j, ] / y[j]
    }
    
    # Convert list to matrix
    efficiency_constraints <- do.call(rbind, constraints)
    
    # Assurance region constraints
    # Each weight should be between 0.5 and 1.5 times the first weight
    assurance_constraints <- matrix(0, nrow=10, ncol=6)
    
    # Lower bounds: weight_k >= 0.5 * weight_1
    for (k in 2:6) {
      assurance_constraints[k-1, 1] <- -0.1  # -0.5 * weight_1
      assurance_constraints[k-1, k] <- 1     # weight_k
    }
    
    # Upper bounds: weight_k <= 1.5 * weight_1
    for (k in 2:6) {
      assurance_constraints[5 + k-1, 1] <- -2  # -1.5 * weight_1
      assurance_constraints[5 + k-1, k] <- 1     # weight_k
    }
    
    # Combine all constraints into a single matrix
    f.con <- rbind(
      efficiency_constraints, 
      assurance_constraints
    )
    
    # Define the direction of constraints
    f.dir <- c(rep(">=", nrow(x)),  rep(">=", 5), rep("<=", 5))
    
    # Define the right-hand side of constraints
    f.rhs <- c(rep(1, nrow(x)),  rep(0, 5), rep(0, 5))
    
    # Solve the LP problem to minimize the objective function
    solution <- lp("min", f.obj, f.con, f.dir, f.rhs)
    
    # Store the efficiency score and weights
    eff[i] <- 1 / solution$objval
  }
  return(eff)
}

# how to call it
#print(dea_in_fixed_weights(x,                       # environmental pressures
#                           y,                       # output
#                           numeric(nrow(data)),     # empty vector for efficiency scores
#                           data))                   # the dataset used
# it works. 


## function for output-oriented efficiency scores (SE)
dea_out_fixed_weights <- function(x, y, eff, data) {
  
  for (i in 1:nrow(y)) {
    # Objective y_crop_s coefficients
    f.obj <- y[i,] / x[i]
    
    constraints <- list()
    # Efficiency constraints: (x_crop / y_crop) for each crop
    for (j in 1:nrow(y)) {
      constraints[[j]] <- y[j, ] / x[j]
    }
    
    # Convert list to matrix
    efficiency_constraints <- do.call(rbind, constraints)
    
    # Non-negativity constraints: Identity matrix
    non_negativity_constraints <- diag(5)
    
    # Assurance region constraints
    # Each weight should be between 0.5 and 1.5 times the first weight
    assurance_constraints <- matrix(0, nrow=8, ncol=5)
    
    # Lower bounds: weight_k >= 0.5 * weight_1
    for (k in 2:5) {
      assurance_constraints[k-1, 1] <- -0.1  # -0.5 * weight_1
      assurance_constraints[k-1, k] <- 1     # weight_k
    }
    
    # Upper bounds: weight_k <= 1.5 * weight_1
    for (k in 2:5) {
      assurance_constraints[4 + k-1, 1] <- -2 # -1.5 * weight_1
      assurance_constraints[4 + k-1, k] <- 1     # weight_k
    }
    
    # Combine all constraints into a single matrix
    f.con <- rbind(
      efficiency_constraints,
      non_negativity_constraints,
      assurance_constraints
    )
    
    # Define the direction of constraints
    f.dir <- c(rep("<=", nrow(y)), rep(">=", 5), rep(">=", 4), rep("<=", 4))
    
    # Define the right-hand side of constraints
    f.rhs <- c(rep(1, nrow(y)), rep(0, 5), rep(0, 4), rep(0, 4))
    
    # Solve the LP problem
    solution <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    # Store the efficiency score and weights
    eff[i] <- solution$objval
  }
  return(eff)
}

# how to call it
#print(dea_out_fixed_weights(x,                       # output
#                            y,                       # social indicators
#                            numeric(nrow(data)),     # empty vector for efficiency scores
#                            data))                   # the dataset used
# it works. 


superdea_in_fixed_weights <- function(x, y, s_eff, data) {
  
  for (i in 1:nrow(x)) {
    # Objective function coefficients for the i-th DMU
    f.obj <- x[i, ] / y[i]
    
    # Create the constraint matrix for all other DMUs (excluding the i-th DMU)
    constraints <- list()
    
    # Efficiency constraints: (x_crop / y_crop) for each crop (excluding DMU i)
    count <- 1
    for (j in 1:nrow(x)) {
      if (j != i) {
        constraints[[count]] <- x[j, ] / y[j]
        count <- count + 1
      }
    }
    
    # Convert the list of constraints into a matrix
    efficiency_constraints <- do.call(rbind, constraints)
    
    # Assurance region constraints
    assurance_constraints <- matrix(0, nrow=10, ncol=6)
    
    # Lower bounds: weight_k >= 0.5 * weight_1
    for (k in 2:6) {
      assurance_constraints[k-1, 1] <- -0.1  # -0.5 * weight_1
      assurance_constraints[k-1, k] <- 1     # weight_k
    }
    
    # Upper bounds: weight_k <= 1.5 * weight_1
    for (k in 2:6) {
      assurance_constraints[5 + k-1, 1] <- -2  # -1.5 * weight_1
      assurance_constraints[5 + k-1, k] <- 1     # weight_k
    }
    
    # Combine efficiency constraints and assurance region constraints
    f.con <- rbind(
      efficiency_constraints, 
      assurance_constraints
    )
    
    # Define the direction of constraints
    # Update the direction of constraints due to the exclusion of DMU i
    f.dir <- c(rep(">=", (nrow(x) -1)), rep(">=", 5), rep("<=", 5))
    
    # Define the right-hand side of the constraints
    f.rhs <- c(rep(1, (nrow(x) -1)), rep(0, 5), rep(0, 5))
    
    # Solve the LP problem
    solution <- lp("min", f.obj, f.con, f.dir, f.rhs)
    
    # Store the super-efficiency score and weights
    s_eff[i] <- 1 / solution$objval
  }
  return(s_eff)
}

# how to call it
#print(superdea_in_fixed_weights(x, 
#                                y,
#                                numeric(nrow(data)), # s_eff, empty vector to store scores of super efficiency
#                                data))

# function for output oriented super efficiency (SE, for outlier deterction)
superdea_out_fixed_weights <- function(x, y, s_eff, data) {
  for (i in 1:nrow(y)) {
    # Objective y_livestock_s coefficients for firm i
    f.obj <- y[i,] / x[i]
    
    constraints <- list()
    # Efficiency constraints: (x_livestock / y_livestock) for each livestock, excluding firm i
    count <- 1
    for (j in 1:nrow(y)) {
      if (j != i) {  # Exclude firm i from its own reference set
        constraints[[count]] <- y[j, ] / x[j]
        count <- count + 1
      }
    }
    
    # Convert list to matrix and remove the excluded row (firm i)
    efficiency_constraints <- do.call(rbind, constraints)
    efficiency_constraints <- efficiency_constraints[!sapply(constraints, is.null),]
    
    # Non-negativity constraints: Identity matrix
    non_negativity_constraints <- diag(5)
    
    # Assurance region constraints
    # Each weight should be between 0.5 and 1.5 times the first weight
    assurance_constraints <- matrix(0, nrow = 8, ncol = 5)
    
    # Lower bounds: weight_k >= 0.5 * weight_1
    for (k in 2:5) {
      assurance_constraints[k - 1, 1] <- -0.5  # -0.5 * weight_1
      assurance_constraints[k - 1, k] <- 1     # weight_k
    }
    
    # Upper bounds: weight_k <= 1.5 * weight_1
    for (k in 2:5) {
      assurance_constraints[4 + k - 1, 1] <- -1.5 # -1.5 * weight_1
      assurance_constraints[4 + k - 1, k] <- 1     # weight_k
    }
    
    # Combine all constraints into a single matrix
    f.con <- rbind(
      efficiency_constraints,
      non_negativity_constraints,
      assurance_constraints
    )
    
    # Define the direction of constraints
    f.dir <- c(rep("<=", (nrow(y) - 1)), rep(">=", 5), rep(">=", 4), rep("<=", 4))
    
    # Define the right-hand side of constraints
    f.rhs <- c(rep(1, (nrow(y) - 1)), rep(0, 5), rep(0, 4), rep(0, 4))
    
    # Solve the LP problem
    solution <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    # Store the super-efficiency score and weights
    s_eff[i] <- solution$objval
  }
  return(s_eff)
}



# ------------------------------------------------------------------------------
#   ANALYSIS
#   we assess efficiency for three groupings of the data
#   1) Livestock and crop specialization
#   2) Biogeographical zones
#   3) Population density
# 
#   Before assessing efficiency, we use superefficiency to detect and remove outliers
# ------------------------------------------------------------------------------

################ LIVESTOCK AND CROP SPECIALISATION #############################

# share of crop and LS production
total_data$share_crop <- total_data$crop_output / total_data$agri_output
total_data$share_ls <- total_data$animal / total_data$agri_output

livestock <- subset(total_data, share_ls >= 0.5) #
crop <- subset(total_data, share_crop >= 0.5) # 

# we want to distinguish regions prodicing a high share of high-value crops
#crop <- left_join(crop, our_data3, by= "NUTS_ID")

crop$share_high_crop <- (crop$wine + crop$fruits + crop$olive_oil + crop$vegetables) / crop$crop_output
# set NA to zero
crop$share_high_crop[is.na(crop$share_high_crop)] <- 0

crop_high <- subset(crop, share_high_crop >= 0.5) # 43
arable_crop <- subset(crop, share_high_crop < 0.5) # 65

# create dummies for specialistaion in total_data (needed for later)
# set 1 if the NUTS_ID is present in dhe livestock_dataset
total_data$ls_dum <- ifelse(total_data$NUTS_ID %in% livestock$NUTS_ID, 1, 0)
total_data$crop_high_dum <- ifelse(total_data$NUTS_ID %in% crop_high$NUTS_ID, 1, 0)
total_data$arable_crop_dum <- ifelse(total_data$NUTS_ID %in% arable_crop$NUTS_ID, 1, 0)
# create an indicators with "livestock" if ls_dum == 1, "High-value Crop" if 
#crop_high_dum == 1 and "Arable Crop" if arable_crop_dum == 1
total_data$specialisation <- ifelse(total_data$ls_dum == 1, "livestock", 
                                    ifelse(total_data$crop_high_dum == 1, 
                                           "High-value Crop", "Arable Crop"))

# Define the input and output matrices for EE
x_livestock <- matrix(c(livestock$ghg_uaa, livestock$conv_share, livestock$div,
                        livestock$ammonia, livestock$pesti_risk, livestock$high_nitrate), ncol = 6)
y_livestock <- matrix(c(livestock$value_prod), ncol = 1)

x_arable_crop <- matrix(c(arable_crop$ghg_uaa, arable_crop$conv_share, arable_crop$div,
                          arable_crop$ammonia, arable_crop$pesti_risk, arable_crop$high_nitrate), ncol = 6)
y_arable_crop <- matrix(c(arable_crop$value_prod), ncol = 1)

x_crop_high <- matrix(c(crop_high$ghg_uaa, crop_high$conv_share, crop_high$div,
                        crop_high$ammonia, crop_high$pesti_risk, crop_high$high_nitrate), ncol = 6)
y_crop_high <- matrix(c(crop_high$value_prod), ncol = 1)


# Define the input and output matrices for SE
x_livestock_s <- matrix(c(livestock$value_prod), ncol = 1)
y_livestock_s <- matrix(c(livestock$inc_awu, livestock$female_ratio, livestock$age_ratio, 
                          livestock$share_full, livestock$antibiotics_inv), ncol = 5)

x_arable_crop_s <- matrix(c(arable_crop$value_prod), ncol = 1)
y_arable_crop_s <- matrix(c(arable_crop$inc_awu, arable_crop$female_ratio, arable_crop$age_ratio, 
                            arable_crop$share_full, arable_crop$antibiotics_inv), ncol = 5)

x_crop_high_s <- matrix(c(crop_high$value_prod), ncol = 1)
y_crop_high_s <- matrix(c(crop_high$inc_awu, crop_high$female_ratio, crop_high$age_ratio, 
                          crop_high$share_full, crop_high$antibiotics_inv), ncol = 5)

# First, assess whether there are outliers 
# Initialize vectors/matrices for efficiency scores and weights

# Super EE
efficiency_scores_livestock <- print(superdea_in_fixed_weights(x_livestock, y_livestock,
                                                               numeric(nrow(livestock)),
                                                               livestock))


summary(efficiency_scores_livestock)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.13163
# Q3: 0.50047
(0.50047 - 0.13163)*1.5 + 0.50047
# Outlier if > 1.05373

# add the efficiency scores and the weights to the livestock_df_rm dataset
livestock$eff_ee_outlier <- efficiency_scores_livestock
eff_liv <- subset(livestock, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: NL12, NL31, ES11

# SE
efficiency_scores_livestock_soc <- print(superdea_out_fixed_weights(x_livestock, y_livestock,
                                                                    numeric(nrow(livestock)),
                                                                    livestock))

summary(efficiency_scores_livestock_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.021385 
# Q3: 0.103548 
(0.103548  - 0.021385)*1.5 + 0.103548 
# Outlier if > 0.2267925

# add the efficiency scores and the weights to the livestock_df_rm dataset
livestock$eff_se_outlier <- efficiency_scores_livestock_soc
eff_liv <- subset(livestock, select = c(NUTS_ID, eff_se_outlier))
# OUTLIERS: SK03, FRI2, AT32, FRC2

# Arable Crop

efficiency_scores_arable_crop <- print(superdea_in_fixed_weights(x_arable_crop, y_arable_crop,
                                                                 numeric(nrow(arable_crop)),
                                                                 arable_crop))

summary(efficiency_scores_arable_crop)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.29261      
# Q3: 0.54947      
(0.54947       - 0.29261    )*1.5 + 0.54947      
# Outlier if > 0.93476

# add the efficiency scores and the weights to the arable_crop dataset
arable_crop$eff_ee_outlier <- efficiency_scores_arable_crop
eff_arable_crop <- subset(arable_crop, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: BE31, FRE1, SE22, HR02

# SE
efficiency_scores_arable_crop_soc <- print(superdea_out_fixed_weights(x_arable_crop_s, y_arable_crop_s,
                                                                      numeric(nrow(arable_crop)),
                                                                      arable_crop))



summary(efficiency_scores_arable_crop_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.02599    
# Q3: 0.05404    
(0.05404     - 0.02599 )*1.5 + 0.05404    
# Outlier if > 0.096115

# add the efficiency scores and the weights to the arable_crop_df_rm dataset
arable_crop$eff_se_outlier <- efficiency_scores_arable_crop_soc
eff_arable_crop <- subset(arable_crop, select = c(NUTS_ID, eff_se_outlier))
# OUTLIERS: SK04, FI1C, SE12, FRF3, BG41,BG34

# High-value Crop
efficiency_scores_crop_high<- print(superdea_in_fixed_weights(x_crop_high, y_crop_high,
                                                              numeric(nrow(crop_high)),
                                                              crop_high))


summary(efficiency_scores_crop_high)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.073074     
# Q3: 0.310418    
(0.310418     - 0.073074   )*1.5 + 0.310418    
# Outlier if > 0.666434

# add the efficiency scores and the weights to the crop_high dataset
crop_high$eff_ee_outlier <- efficiency_scores_crop_high
eff_crop_high <- subset(crop_high, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: NL33

# SE
# crop_high
efficiency_scores_crop_high_soc<- print(superdea_out_fixed_weights(x_crop_high_s, y_crop_high_s,
                                                                   numeric(nrow(crop_high)),
                                                                   crop_high))

summary(efficiency_scores_crop_high_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.01494      
# Q3: 0.06161     
(0.06161      - 0.01494   )*1.5 + 0.048036    
# Outlier if > 0.118041

# add the efficiency scores and the weights to the crop_high_df_rm dataset
crop_high$eff_se_outlier <- efficiency_scores_crop_high_soc
eff_crop_high <- subset(crop_high, select = c(NUTS_ID, eff_se_outlier))
# OUTLIERS: 	HR03, FRK2, FRM0, RO22, ES43

# total outliers from livestock, crop_high and arable_crop:
# NL12, NL31, ES11, SK03, FRI2, AT32, FRC2, EL52, EL61, EL53, SE22, HR02, SE12, BG34, BG33,
# NL33, EL65, PT15, SK04, HR03, FRF3, BG41

# ------------------------------------------------------------------------------
#### ASSESSING EE WITHOUT OUTLIERS 

# remove the outliers
crop_ls_rm <- total_data %>% 
  filter(!(NUTS_ID %in% c("NL12", "NL31", "ES11", "BE31", "FRE1", "SE22", 
                          "HR02", "NL33", "SK03", "FRI2", "AT32", "FRC2",
                          "SK04", "FI1C",  "SE12", "FRF3", "BG41", "BG34",
                          "HR03", "FRK2", "FRM0", "RO22", "ES43")))

# 22 outliers removed
# 135 obs

# define livestock and crop dataset 
livestock <- subset(crop_ls_rm, share_ls >= 0.5) # 43 obs
crop <- subset(crop_ls_rm, share_crop >= 0.5) # 92 obs

crop$share_high_crop <- (crop$wine + crop$fruits + crop$olive_oil + crop$vegetables) / crop$crop_output
# set NA to zero

crop$share_high_crop
crop_high <- subset(crop, share_high_crop >= 0.5) # 37  
arable_crop <- subset(crop, share_high_crop < 0.5)   # 55


# Define the input and output matrices for EE
x_livestock <- matrix(c(livestock$ghg_uaa, livestock$conv_share, livestock$div,
                        livestock$ammonia, livestock$pesti_risk, livestock$high_nitrate), ncol = 6)
y_livestock <- matrix(c(livestock$value_prod), ncol = 1)

x_arable_crop <- matrix(c(arable_crop$ghg_uaa, arable_crop$conv_share, arable_crop$div,
                          arable_crop$ammonia, arable_crop$pesti_risk, arable_crop$high_nitrate), ncol = 6)
y_arable_crop <- matrix(c(arable_crop$value_prod), ncol = 1)

x_crop_high <- matrix(c(crop_high$ghg_uaa, crop_high$conv_share, crop_high$div,
                        crop_high$ammonia, crop_high$pesti_risk, crop_high$high_nitrate), ncol = 6)
y_crop_high <- matrix(c(crop_high$value_prod), ncol = 1)


# Define the input and output matrices for SE
x_livestock_s <- matrix(c(livestock$value_prod), ncol = 1)
y_livestock_s <- matrix(c(livestock$inc_awu, livestock$female_ratio, livestock$age_ratio, 
                          livestock$share_full, livestock$antibiotics_inv), ncol = 5)

x_arable_crop_s <- matrix(c(arable_crop$value_prod), ncol = 1)
y_arable_crop_s <- matrix(c(arable_crop$inc_awu, arable_crop$female_ratio, arable_crop$age_ratio, 
                            arable_crop$share_full, arable_crop$antibiotics_inv), ncol = 5)

x_crop_high_s <- matrix(c(crop_high$value_prod), ncol = 1)
y_crop_high_s <- matrix(c(crop_high$inc_awu, crop_high$female_ratio, crop_high$age_ratio, 
                          crop_high$share_full, crop_high$antibiotics_inv), ncol = 5)

# assess efficiency for livestock and crop regions:

# LIVESTOCK

# EE 
livestock$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_livestock,
                       y_livestock,
                       numeric(nrow(livestock)),
                       livestock))            

# SE
livestock$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_livestock_s,
                        y_livestock_s,
                        numeric(nrow(livestock)),
                        livestock))   

# arable_crop
# EE 
arable_crop$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_arable_crop,
                       y_arable_crop,
                       numeric(nrow(arable_crop)),
                       arable_crop))            

# SE
arable_crop$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_arable_crop_s,
                        y_arable_crop_s,
                        numeric(nrow(arable_crop)),
                        arable_crop))  

# High-value Crop
# EE 
crop_high$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_crop_high,
                       y_crop_high,
                       numeric(nrow(crop_high)),
                       crop_high))            

# SE
crop_high$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_crop_high_s,
                        y_crop_high_s,
                        numeric(nrow(crop_high)),
                        crop_high))   

# ------------------------------------------------------------------------------
#### Assess SE when only omiting the EE outliers, set EE to 1 for these regions
# and add result to the result dataset

# We create a dataset only removing the outliers in SE, and NL33, which is classified
# as an outlier in all dimensions (the only outlier in all dimensions in EE)
crop_ls_rm_se <- total_data %>% 
  filter(!(NUTS_ID %in% c("SK03", "FRI2", "AT32", "FRC2", "SK04", "FI1C",  
                          "SE12", "FRF3", "BG41", "BG34", "HR03", "FRK2", 
                          "FRM0", "RO22", "ES43", "NL33")))


livestock_rm <- subset(crop_ls_rm_se, share_ls >= 0.5) # 46 obs
crop_rm <- subset(crop_ls_rm_se, share_crop >= 0.5) # 97 obs

crop_rm$share_high_crop <- (crop_rm$wine + crop_rm$fruits + crop_rm$olive_oil + 
                              crop_rm$vegetables) / crop_rm$crop_output
# set NA to zero

crop$share_high_crop
crop_high_rm <- subset(crop_rm, share_high_crop >= 0.5) # 37  
arable_crop_rm <- subset(crop_rm, share_high_crop < 0.5)   # 59

# Define the input and output matrices for SE
x_livestock_s <- matrix(c(livestock_rm$value_prod), ncol = 1)
y_livestock_s <- matrix(c(livestock_rm$inc_awu, livestock_rm$female_ratio, livestock_rm$age_ratio, 
                          livestock_rm$share_full, livestock_rm$antibiotics_inv), ncol = 5)

x_arable_crop_s <- matrix(c(arable_crop_rm$value_prod), ncol = 1)
y_arable_crop_s <- matrix(c(arable_crop_rm$inc_awu, arable_crop_rm$female_ratio, arable_crop_rm$age_ratio, 
                            arable_crop_rm$share_full, arable_crop_rm$antibiotics_inv), ncol = 5)

# livestock
# SE
livestock_rm$eff_se_lpsolve_outliers_ee <- print(
  dea_out_fixed_weights(x_livestock_s,
                        y_livestock_s,
                        numeric(nrow(livestock_rm)),
                        livestock_rm))   

# arable_crop
# SE
arable_crop_rm$eff_se_lpsolve_outliers_ee <- print(
  dea_out_fixed_weights(x_arable_crop_s,
                        y_arable_crop_s,
                        numeric(nrow(arable_crop_rm)),
                        arable_crop_rm))   

# add the special cases to the livestock results
outliers_ls <- livestock_rm %>% select(NUTS_ID, eff_se_lpsolve_outliers_ee) %>%
  filter(NUTS_ID %in% c("NL12", "NL31", "ES11")) %>%
  rename(eff_se_lpsolve = eff_se_lpsolve_outliers_ee)
outliers_ls$eff_ee_lpsolve <- 1.05

# add outliers_ls to the livestock data
livestock <- full_join(livestock, outliers_ls)

# add the special cases to the livestock results
outliers_arable_crop <- arable_crop_rm %>% select(NUTS_ID, eff_se_lpsolve_outliers_ee) %>%
  filter(NUTS_ID %in% c("BE31", "FRE1", "SE22", "HR02")) %>%
  rename(eff_se_lpsolve = eff_se_lpsolve_outliers_ee)
outliers_arable_crop$eff_ee_lpsolve <- 1.05

# add outliers_ls to the livestock data
arable_crop <- full_join(arable_crop, outliers_arable_crop)

# ------------------------------------------------------------------------------
# Assess SE when only omiting the EE outliers, set EE to 1 for these regions
# and add result to the result dataset

# We create a dataset only removing the outliers in SE, and NL33, which is classified
# as an outlier in all dimensions (the only outlier in all dimensions in EE)
crop_ls_rm_ee <- total_data %>% 
  filter(!(NUTS_ID %in% c("NL12", "NL31", "ES11", "BE31", "FRE1", 
                          "SE22", "HR02", "NL33", "SK03", "FRI2", "AT32", 
                          "FRC2", "SK04", "SE12", "FRF3", "HR03")))
# we remove the special cases in EE, and the outliers in all dimension, to assess
# EE also for the special cases in SE.

livestock_rm <- subset(crop_ls_rm_ee, share_ls >= 0.5) # 43 obs
crop_rm <- subset(crop_ls_rm_ee, share_crop >= 0.5) # 97 obs

crop_rm$share_high_crop <- (crop_rm$wine + crop_rm$fruits + crop_rm$olive_oil + 
                              crop_rm$vegetables) / crop_rm$crop_output
# set NA to zero

crop_high_rm <- subset(crop_rm, share_high_crop >= 0.5) # 41  
arable_crop_rm <- subset(crop_rm, share_high_crop < 0.5)   # 57

# Define the input and output matrices for EE
x_arable_crop <- matrix(c(arable_crop_rm$ghg_uaa, arable_crop_rm$conv_share, arable_crop_rm$div,
                          arable_crop_rm$ammonia, arable_crop_rm$pesti_risk, arable_crop_rm$high_nitrate), ncol = 6)
y_arable_crop <- matrix(c(arable_crop_rm$value_prod), ncol = 1)

x_crop_high <- matrix(c(crop_high_rm$ghg_uaa, crop_high_rm$conv_share, crop_high_rm$div,
                        crop_high_rm$ammonia, crop_high_rm$pesti_risk, crop_high_rm$high_nitrate), ncol = 6)
y_crop_high <- matrix(c(crop_high_rm$value_prod), ncol = 1)

# arable_crop
arable_crop_rm$eff_ee_lpsolve_outliers_ee <- print(
  dea_in_fixed_weights(x_arable_crop,
                       y_arable_crop,
                       numeric(nrow(arable_crop_rm)),
                       arable_crop_rm))  


# crop_high
crop_high_rm$eff_ee_lpsolve_outliers_ee <- print(
  dea_in_fixed_weights(x_crop_high,
                       y_crop_high,
                       numeric(nrow(crop_high_rm)),
                       crop_high_rm))  

# add the special cases to the arable_crop results
outliers_arable_crop <- arable_crop_rm %>% select(NUTS_ID, eff_ee_lpsolve_outliers_ee) %>%
  filter(NUTS_ID %in% c("FI1C", "BG34")) %>%
  rename(eff_ee_lpsolve = eff_ee_lpsolve_outliers_ee)
outliers_arable_crop$eff_se_lpsolve <- 1.05

# add outliers_ls to the livestock data
arable_crop <- full_join(arable_crop, outliers_arable_crop)

# add the special cases to the crop_high results
outliers_crop_high <- crop_high_rm %>% select(NUTS_ID, eff_ee_lpsolve_outliers_ee) %>%
  filter(NUTS_ID %in% c("FRK2", "FRM0", "RO22", "ES43")) %>%
  rename(eff_ee_lpsolve = eff_ee_lpsolve_outliers_ee)
outliers_crop_high$eff_se_lpsolve <- 1.05 

# add outliers_ls to the livestock data
crop_high <- full_join(crop_high, outliers_crop_high)

# ------------------------------------------------------------------------------
# PLOT RESULTS

colnames(livestock) <- make.unique(names(livestock))
colnames(arable_crop) <- make.unique(names(arable_crop))
colnames(crop_high) <- make.unique(names(crop_high))

# results
p_livestock <- ggplot(livestock, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(title = "Livestock", x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) +
  xlim(0, 1.11) + ylim(0, 1.11) +
  geom_point(data = livestock %>% filter(str_detect(NUTS_ID, "NL31")), color = "grey", size = 2) +
  geom_point(data = livestock %>% filter(str_detect(NUTS_ID, "ES11")), color = "grey", size = 2)

p_arable_crop <- ggplot(arable_crop, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(title = "Arable Crop", x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) +
  xlim(0, 1.11) + ylim(0, 1.11) + 
  geom_point(data = arable_crop %>% filter(str_detect(NUTS_ID, "BE31")), color = "grey", size = 2) +
  geom_point(data = arable_crop %>% filter(str_detect(NUTS_ID, "FRE1")), color = "grey", size = 2) +
  geom_point(data = arable_crop %>% filter(str_detect(NUTS_ID, "SE22")), color = "grey", size = 2) +
  geom_point(data = arable_crop %>% filter(str_detect(NUTS_ID, "HR02")), color = "grey", size = 2) +
  geom_point(data = arable_crop %>% filter(str_detect(NUTS_ID, "BG34")), color = "grey", size = 2)

p_crop_high <- ggplot(crop_high, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(title = "High-value Crop", x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) +
  xlim(0, 1.11) + ylim(0, 1.11) + 
  geom_point(data = crop_high %>% filter(str_detect(NUTS_ID, "FRK2")), color = "grey", size = 2) +
  geom_point(data = crop_high %>% filter(str_detect(NUTS_ID, "FRM0")), color = "grey", size = 2) +
  geom_point(data = crop_high %>% filter(str_detect(NUTS_ID, "RO22")), color = "grey", size = 2) +
  geom_point(data = crop_high %>% filter(str_detect(NUTS_ID, "ES43")), color = "grey", size = 2)

# ------------------------------------------------------------------------------
## Biogeo zones
# ------------------------------------------------------------------------------

biogeo_regions <- read_excel("used_data/biogeo_regions.xlsx")
# merge the data with the total_data dataset
total_data <- left_join(total_data, biogeo_regions, by = "NUTS_ID")

# create subsets of the data with the different Climatic Zones in biogeo_regions
continental <- subset(total_data, `Climatic Zone` == "Continental") # 70
atlantic <- subset(total_data, `Climatic Zone` == "Atlantic") # 43
mediterranean <- subset(total_data, `Climatic Zone` == "Mediterranean") # 27
boreal <- subset(total_data, `Climatic Zone` == "Boreal") # 6
Pannonian <- subset(total_data, `Climatic Zone` == "Pannonian") # 10
# + 2 Alpine regions
##### We proceed to assess efficiency of the Continental, Atlantic and Mediterranean regions.

###### Define the input and output matrixes for EE and SE########
### Define the input and output matrices for EE
x_continental <- matrix(c(continental$ghg_uaa, continental$conv_share, continental$div,
                          continental$ammonia, continental$pesti_risk, continental$high_nitrate), ncol = 6)
y_continental <- matrix(c(continental$value_prod), ncol = 1)

x_atlantic <- matrix(c(atlantic$ghg_uaa, atlantic$conv_share, atlantic$div,
                       atlantic$ammonia, atlantic$pesti_risk, atlantic$high_nitrate), ncol = 6)
y_atlantic <- matrix(c(atlantic$value_prod), ncol = 1)

x_mediterranean <- matrix(c(mediterranean$ghg_uaa, mediterranean$conv_share, mediterranean$div,
                            mediterranean$ammonia, mediterranean$pesti_risk, mediterranean$high_nitrate), ncol = 6)
y_mediterranean <- matrix(c(mediterranean$value_prod), ncol = 1)

# Define the input and output matrices for SE
x_continental_s <- matrix(c(continental$value_prod), ncol = 1)
y_continental_s <- matrix(c(continental$inc_awu, continental$female_ratio, continental$age_ratio, 
                            continental$share_full, continental$antibiotics_inv), ncol = 5)

x_atlantic_s <- matrix(c(atlantic$value_prod), ncol = 1)
y_atlantic_s <- matrix(c(atlantic$inc_awu, atlantic$female_ratio, atlantic$age_ratio, 
                         atlantic$share_full, atlantic$antibiotics_inv), ncol = 5)

x_mediterranean_s <- matrix(c(mediterranean$value_prod), ncol = 1)
y_mediterranean_s <- matrix(c(mediterranean$inc_awu, mediterranean$female_ratio, mediterranean$age_ratio, 
                              mediterranean$share_full, mediterranean$antibiotics_inv), ncol = 5)
# ------------------------------------------------------------------------------


# Outlier detection using super efficiency with weight restrictions

efficiency_scores_continental <- print(superdea_in_fixed_weights(x_continental, y_continental,
                                                                 numeric(nrow(continental)),
                                                                 continental))

summary(efficiency_scores_continental)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.19220 
# Q3: 0.49045 
(0.49045  - 0.19220 )*1.5 + 0.49045 # = 0.937825
# EE > 0.937825
continental$eff_ee_outlier <- efficiency_scores_continental
eff_continental <- subset(continental, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: DEB3, SE22, FRF1, BE33, FRJ1, DEA1, HR02

efficiency_scores_continental_soc <- print(
  superdea_out_fixed_weights(x_continental_s, 
                             y_continental_s,
                             numeric(nrow(continental)),
                             continental))

summary(efficiency_scores_continental_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.06974   
# Q3: 0.19389   
(0.19389    - 0.06974  )*1.5 + 0.19389 # = 0.380115 
# Outlier if > 0.380115

# add the efficiency scores and the weights to the continental_df_rm dataset
continental$eff_se_outlier <- efficiency_scores_continental_soc
eff_continental <- subset(continental, select = c(NUTS_ID, eff_se_outlier))
# OUTLIER: FRI2, FRF3, BG41, FRC2

# Atlantic
efficiency_scores_atlantic <- print(
  superdea_in_fixed_weights(x_atlantic, 
                            y_atlantic,
                            numeric(nrow(atlantic)),
                            atlantic))

summary(efficiency_scores_atlantic)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.06502  
# Q3: 0.14531  
(0.14531   - 0.06502  )*1.5 + 0.14531  # = 0.265745
# EE > 0.265745
atlantic$eff_ee_outlier <- efficiency_scores_atlantic
eff_atlantic <- subset(atlantic, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: NL33, NL32, FRL0, NL42, NL23

efficiency_scores_atlantic_soc <- print(
  superdea_out_fixed_weights(x_atlantic_s, 
                             y_atlantic_s,
                             numeric(nrow(atlantic)),
                             atlantic))

summary(efficiency_scores_atlantic_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.20830    
# Q3: 0.61318    
(0.61318     - 0.20830   )*1.5 + 0.61318  # = 1.2205
# Outlier if > 1.2205

# add the efficiency scores and the weights to the atlantic_df_rm dataset
atlantic$eff_se_outlier <- efficiency_scores_atlantic_soc
eff_atlantic <- subset(atlantic, select = c(NUTS_ID, eff_se_outlier))
# OUTLIER: FRJ2

# Mediterranean
efficiency_scores_mediterranean <- print(
  superdea_in_fixed_weights(x_mediterranean, 
                            y_mediterranean,
                            numeric(nrow(mediterranean)),
                            mediterranean))

summary(efficiency_scores_mediterranean)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.20405   
# Q3: 0.72343   
(0.72343    - 0.20405   )*1.5 + 0.72343   # = 1.5025
# EE > 1.5025
mediterranean$eff_ee_outlier <- efficiency_scores_mediterranean
eff_mediterranean <- subset(mediterranean, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: No outliers

# mediterranean
efficiency_scores_mediterranean_soc <- print(
  superdea_out_fixed_weights(x_mediterranean_s, 
                             y_mediterranean_s,
                             numeric(nrow(mediterranean)),
                             mediterranean))

summary(efficiency_scores_mediterranean_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.01405      
# Q3: 0.05525     
(0.05525      - 0.01405     )*1.5 + 0.05525   # = 0.11705
# Outlier if > 0.11705

# add the efficiency scores and the weights to the mediterranean_df_rm dataset
mediterranean$eff_se_outlier <- efficiency_scores_mediterranean_soc
eff_mediterranean <- subset(mediterranean, select = c(NUTS_ID, eff_se_outlier))
# OUTLIER: HR03, ES43

# remove outliers
# also remove the observations which are not included in Continental, Atlantic,
# or Mediterranean: 
# Pannonian: HU12, HU21, HU22, HU23, HU31, HU32, HU33, SK02, SK03, SK04
# Boreal: SE12, SE21, SE23, FI19, FI19, FI1C, FI1D 
# Alpine: AT22, AT32

biogeo_rm <- total_data %>% 
  filter(!(NUTS_ID %in% c("DEB3", "SE22", "FRF1", "BE33", "FRJ1", "DEA1", "HR02", 
                          "FRI2", "FRF3", "BG41", "FRC2", "NL33", "NL32", "FRL0", 
                          "NL42", "NL23", "FRJ2", "HR03", "ES43", "HU12", "HU21", 
                          "HU22", "HU23", "HU31", "HU32", "HU33", "SK02", "SK03", 
                          "SK04", "SE12", "SE21", "SE23", "FI19", "FI19", "FI1C", 
                          "FI1D", "AT22", "AT32")))
# 121 obs

# ------------------------------------------------------------------------------
# Define the intput and output matrixes without the outliers

continental <- subset(biogeo_rm, `Climatic Zone` == "Continental") # 70
atlantic <- subset(biogeo_rm, `Climatic Zone` == "Atlantic") # 43
mediterranean <- subset(biogeo_rm, `Climatic Zone` == "Mediterranean") # 27

x_continental <- matrix(c(continental$ghg_uaa, continental$conv_share, continental$div,
                          continental$ammonia, continental$pesti_risk, continental$high_nitrate), ncol = 6)
y_continental <- matrix(c(continental$value_prod), ncol = 1)

x_atlantic <- matrix(c(atlantic$ghg_uaa, atlantic$conv_share, atlantic$div,
                       atlantic$ammonia, atlantic$pesti_risk, atlantic$high_nitrate), ncol = 6)
y_atlantic <- matrix(c(atlantic$value_prod), ncol = 1)

x_mediterranean <- matrix(c(mediterranean$ghg_uaa, mediterranean$conv_share, mediterranean$div,
                            mediterranean$ammonia, mediterranean$pesti_risk, mediterranean$high_nitrate), ncol = 6)
y_mediterranean <- matrix(c(mediterranean$value_prod), ncol = 1)

# Define the input and output matrices for SE
x_continental_s <- matrix(c(continental$value_prod), ncol = 1)
y_continental_s <- matrix(c(continental$inc_awu, continental$female_ratio, continental$age_ratio, 
                            continental$share_full, continental$antibiotics_inv), ncol = 5)

x_atlantic_s <- matrix(c(atlantic$value_prod), ncol = 1)
y_atlantic_s <- matrix(c(atlantic$inc_awu, atlantic$female_ratio, atlantic$age_ratio, 
                         atlantic$share_full, atlantic$antibiotics_inv), ncol = 5)

x_mediterranean_s <- matrix(c(mediterranean$value_prod), ncol = 1)
y_mediterranean_s <- matrix(c(mediterranean$inc_awu, mediterranean$female_ratio, mediterranean$age_ratio, 
                              mediterranean$share_full, mediterranean$antibiotics_inv), ncol = 5)

# ------------------------------------------------------------------------------
# Continental
# EE
continental$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_continental,
                       y_continental,
                       numeric(nrow(continental)),
                       continental))   

# SE
continental$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_continental_s,
                        y_continental_s,
                        numeric(nrow(continental)),
                        continental))   

# Atlantic
# EE
atlantic$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_atlantic,
                       y_atlantic,
                       numeric(nrow(atlantic)),
                       atlantic))   

# SE
atlantic$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_atlantic_s,
                        y_atlantic_s,
                        numeric(nrow(atlantic)),
                        atlantic))   

# Mediterranean
# EE
mediterranean$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_mediterranean,
                       y_mediterranean,
                       numeric(nrow(mediterranean)),
                       mediterranean))   

# SE
mediterranean$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_mediterranean_s,
                        y_mediterranean_s,
                        numeric(nrow(mediterranean)),
                        mediterranean))   

# Add the special cases to the dataset

# ------------------------------------------------------------------------------
# We create a dataset only removing the special cases in SE and the outliers (NL33)

biogeo_rm_se <- total_data %>% 
  filter(!(NUTS_ID %in% c("FRI2", "FRF3", "BG41", "FRC2", "FRJ2", "HR03", 
                          "ES43", "NL33")))

continental_rm <- subset(biogeo_rm_se, `Climatic Zone` == "Continental") # 64
atlantic_rm <- subset(biogeo_rm_se, `Climatic Zone` == "Atlantic") # 41
# we do not consider Mediterranean here as it has no outliers in EE 

x_continental_s <- matrix(c(continental_rm$value_prod), ncol = 1)
y_continental_s <- matrix(c(continental_rm$inc_awu, continental_rm$female_ratio, continental_rm$age_ratio, 
                            continental_rm$share_full, continental_rm$antibiotics_inv), ncol = 5)

x_atlantic_s <- matrix(c(atlantic_rm$value_prod), ncol = 1)
y_atlantic_s <- matrix(c(atlantic_rm$inc_awu, atlantic_rm$female_ratio, atlantic_rm$age_ratio, 
                         atlantic_rm$share_full, atlantic_rm$antibiotics_inv), ncol = 5)


# Continental

# SE
continental_rm$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_continental_s,
                        y_continental_s,
                        numeric(nrow(continental_rm)),
                        continental_rm))   


# Atlantic

# SE
atlantic_rm$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_atlantic_s,
                        y_atlantic_s,
                        numeric(nrow(atlantic_rm)),
                        atlantic_rm))   


# add the special cases to the livestock results
outliers_continental <- continental_rm %>% select(NUTS_ID, eff_se_lpsolve) %>%
  filter(NUTS_ID %in% c("DEB3", "FRF1", "BE33", "FRJ1", "DEA1", "HR02"))
outliers_continental$eff_ee_lpsolve <- 1.05

# add outliers_ls to the livestock data
continental <- full_join(continental, outliers_continental)


# add the special cases to the livestock results
outliers_atlantic <- atlantic_rm %>% select(NUTS_ID, eff_se_lpsolve) %>%
  filter(NUTS_ID %in% c("NL32", "FRL0", "NL42", "NL23"))
outliers_atlantic$eff_ee_lpsolve <- 1.05

# add outliers_ls to the livestock data
atlantic <- full_join(atlantic, outliers_atlantic)

# We create a dataset only removing the outliers in SE, and NL33, which is classified
# as an outlier in all dimensions (the only outlier in all dimensions in EE)
biogeo_rm_ee <- total_data %>% 
  filter(!(NUTS_ID %in% c("DEB3", "SE22", "FRF1", "BE33", "FRJ1", "DEA1", "HR02",
                          "NL33", "NL32", "FRL0", "NL42", "NL23", "FRI2", "FRF3", 
                          "BG41", "FRC2", "HR03")))

atlantic_rm <- subset(biogeo_rm_ee, `Climatic Zone` == "Atlantic") # 38
mediterranean_rm <- subset(biogeo_rm_ee, `Climatic Zone` == "Mediterranean") # 26


x_atlantic <- matrix(c(atlantic_rm$ghg_uaa, atlantic_rm$conv_share, atlantic_rm$div,
                       atlantic_rm$ammonia, atlantic_rm$pesti_risk, atlantic_rm$high_nitrate), ncol = 6)
y_atlantic <- matrix(c(atlantic_rm$value_prod), ncol = 1)

x_mediterranean <- matrix(c(mediterranean_rm$ghg_uaa, mediterranean_rm$conv_share, mediterranean_rm$div,
                            mediterranean_rm$ammonia, mediterranean_rm$pesti_risk, mediterranean_rm$high_nitrate), ncol = 6)
y_mediterranean <- matrix(c(mediterranean_rm$value_prod), ncol = 1)

# Atlantic
# SE
atlantic_rm$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_atlantic,
                       y_atlantic,
                       numeric(nrow(atlantic_rm)),
                       atlantic_rm))   

# SE
mediterranean_rm$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_mediterranean,
                       y_mediterranean,
                       numeric(nrow(mediterranean_rm)),
                       mediterranean_rm))   


# add the special cases to the livestock results
outliers_atlantic <- atlantic_rm %>% select(NUTS_ID, eff_ee_lpsolve) %>%
  filter(NUTS_ID %in% c("FRJ2"))
outliers_atlantic$eff_se_lpsolve <- 1.05

# add outliers_ls to the atlantic data
atlantic <- full_join(atlantic, outliers_atlantic)

# add the special cases to the livestock results
outliers_mediterranean <- mediterranean_rm %>% select(NUTS_ID, eff_ee_lpsolve) %>%
  filter(NUTS_ID %in% c("ES43"))
outliers_mediterranean$eff_se_lpsolve <- 1.05

# add outliers_ls to the mediterranean data
mediterranean <- full_join(mediterranean, outliers_mediterranean)

# ------------------------------------------------------------------------------
# Result biogeo regions
# ------------------------------------------------------------------------------

colnames(continental) <- make.unique(names(continental))
colnames(atlantic) <- make.unique(names(atlantic))
colnames(mediterranean) <- make.unique(names(mediterranean))

p_continental <- ggplot(continental, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) + 
  ggtitle("Continental") + 
  xlim(0, 1.11) + ylim(0, 1.11) +
  geom_point(data = continental %>% 
               filter(str_detect(NUTS_ID, "DEB3|FRF1|BE33|FRJ1|DEA1|HR02")), 
             color = "grey", size = 3)
# FRF1 and FRJ1 have almost the same SE, thus they are not so clear in the graph

# atlantic
p_atlantic <- ggplot(atlantic, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) +
  ggtitle("Atlantic") + 
  xlim(0, 1.11) + ylim(0, 1.11) + 
  geom_point(data = atlantic %>% filter(str_detect(NUTS_ID, "NL32|FRL0|NL42|NL23|FRJ2")), 
             color = "grey", size = 3)

# Mediterranean
p_mediterranean <- ggplot(mediterranean, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) + 
  ggtitle("Mediterranean") + 
  xlim(0, 1.11) + ylim(0, 1.11) + 
  geom_point(data = mediterranean %>% filter(NUTS_ID == "ES43"), 
             color = "grey", size = 3)

# ------------------------------------------------------------------------------
#                     URBAN RURAL AND INTERMEDIATE REGIONS                     
# ------------------------------------------------------------------------------
# we use the typology from Bianchi et al. (2020) to categorise the regions into
# urban, intermediate or rural regions.

# testing different subsets
bianchi_typology <- read_excel("used_data/bianchi_typology.xlsx")

# Bianchi uses the old typology (which was changed in 2016). We use the
# regions package in R to update the names that changed (mainly France)
library(regions)
data(nuts_changes)
# subset only keeping typology == nuts_level_2
change <- nuts_changes %>% filter(typology == "nuts_level_2")
# only keep observations with different values in code_2013 and code_2016
change <- change %>% filter(code_2013 != code_2016) %>% select(code_2013, code_2016)
bianchi_typology <- left_join(bianchi_typology, change, by = c("NUTS_ID" = "code_2013"))
# replace NUTS_ID in bianchi typology with the value for code_2015 for the rows it is not NA
bianchi_typology$NUTS_ID[!is.na(bianchi_typology$code_2016)] <- bianchi_typology$code_2016[!is.na(bianchi_typology$code_2016)]

# the NUTS2 for Croatia changed in 2020, Bianchi uses the "old" typology and 
# we use the new. For consistency, we split the HR4 (in Bianchi typology) into
# HR02, HR05 and HR06 following the new NUTS-classification (intermediate regions)
# HR03 remains as it is. 
croatia <- data.frame(NUTS_ID = c("HR02", "HR05", "HR06"), Typology = "Intermediate")
bianchi_typology <- full_join(bianchi_typology, croatia, by = c("NUTS_ID", "Typology"))

# add to total_data
total_data <- left_join(total_data, bianchi_typology, by = c("NUTS_ID"))

# Define the subsets
rural <- total_data %>% filter(Typology == "Pred. rural") # 71
intermediate <- total_data %>% filter(Typology == "Intermediate") # 59 
urban <- total_data %>% filter(Typology == "Pred. urban") # 24
# Regions in Ireland are missing... What to do?

# ---------------------------------------
# define the input and output matrices

x_rural <- matrix(c(rural$ghg_uaa, rural$conv_share, rural$div,
                    rural$ammonia, rural$pesti_risk, rural$high_nitrate), ncol = 6)
y_rural <- matrix(c(rural$value_prod), ncol = 1)

x_intermediate <- matrix(c(intermediate$ghg_uaa, intermediate$conv_share, intermediate$div,
                           intermediate$ammonia, intermediate$pesti_risk, intermediate$high_nitrate), ncol = 6)
y_intermediate <- matrix(c(intermediate$value_prod), ncol = 1)

x_urban <- matrix(c(urban$ghg_uaa, urban$conv_share, urban$div,
                    urban$ammonia, urban$pesti_risk, urban$high_nitrate), ncol = 6)
y_urban <- matrix(c(urban$value_prod), ncol = 1)

# Define the input and output matrices for SE
x_rural_s <- matrix(c(rural$value_prod), ncol = 1)
y_rural_s <- matrix(c(rural$inc_awu, rural$female_ratio, rural$age_ratio, 
                      rural$share_full, rural$antibiotics_inv), ncol = 5)

x_intermediate_s <- matrix(c(intermediate$value_prod), ncol = 1)
y_intermediate_s <- matrix(c(intermediate$inc_awu, intermediate$female_ratio, intermediate$age_ratio, 
                             intermediate$share_full, intermediate$antibiotics_inv), ncol = 5)

x_urban_s <- matrix(c(urban$value_prod), ncol = 1)
y_urban_s <- matrix(c(urban$inc_awu, urban$female_ratio, urban$age_ratio, 
                      urban$share_full, urban$antibiotics_inv), ncol = 5)

# ---------------------------------------

# RURAL
efficiency_scores_rural <- print(
  superdea_in_fixed_weights(x_rural, 
                            y_rural,
                            numeric(nrow(rural)),
                            rural))

summary(efficiency_scores_rural)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.124278  
# Q3: 0.297937  
(0.297937   - 0.124278  )*1.5 + 0.297937  # = 0.5584255
# EE > 0.5584255
rural$eff_ee_outlier <- efficiency_scores_rural
eff_rural <- subset(rural, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: EL65, EL61, EL52, EL63, EL53, ES11


# rural
efficiency_scores_rural_soc <- print(
  superdea_out_fixed_weights(x_rural_s, 
                             y_rural_s,
                             numeric(nrow(rural)),
                             rural))

summary(efficiency_scores_rural_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.026530     
# Q3: 0.064327     
(0.064327      - 0.026530   )*1.5 + 0.064327   # = 0.1210225 
# Outlier if > 0.1210225

# add the efficiency scores and the weights to the rural_df_rm dataset
rural$eff_se_outlier <- efficiency_scores_rural_soc
eff_rural <- subset(rural, select = c(NUTS_ID, eff_se_outlier))
# OUTLIER: SK04, SK03, HR03, FRI2, AT32, FRF3, FRC2, BE34

## URBAN
# remove outliers
efficiency_scores_urban <- print(
  superdea_in_fixed_weights(x_urban, 
                            y_urban,
                            numeric(nrow(urban)),
                            urban))

summary(efficiency_scores_urban)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.07869   
# Q3: 0.22579   
(0.22579    - 0.07869   )*1.5 + 0.22579   # = 0.44644
# EE > 0.44644
urban$eff_ee_outlier <- efficiency_scores_urban
eff_urban <- subset(urban, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: NL33

# urban
efficiency_scores_urban_soc <- print(
  superdea_out_fixed_weights(x_urban_s, 
                             y_urban_s,
                             numeric(nrow(urban)),
                             urban))

summary(efficiency_scores_urban_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.14963     
# Q3: 0.37013     
(0.37013      - 0.14963   )*1.5 + 0.37013   # = 0.70088 
# Outlier if > 0.70088

# add the efficiency scores and the weights to the urban_df_rm dataset
urban$eff_se_outlier <- efficiency_scores_urban_soc
eff_urban <- subset(urban, select = c(NUTS_ID, eff_se_outlier))
# OUTLIER: CZ02, FR10, EL30

## intermediate
# remove outliers
efficiency_scores_intermediate <- print(
  superdea_in_fixed_weights(x_intermediate, 
                            y_intermediate,
                            numeric(nrow(intermediate)),
                            intermediate))

summary(efficiency_scores_intermediate)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.12207     
# Q3: 0.51494     
(0.51494  - 0.12207 )*1.5 + 0.51494  # = 1.104245
# EE > 1.104245
intermediate$eff_ee_outlier <- efficiency_scores_intermediate
eff_intermediate <- subset(intermediate, select = c(NUTS_ID, eff_ee_outlier))
# OUTLIERS: PT15, FRL0, NL34

efficiency_scores_intermediate_soc <- print(
  superdea_out_fixed_weights(x_intermediate_s, 
                             y_intermediate_s,
                             numeric(nrow(intermediate)),
                             intermediate))

summary(efficiency_scores_intermediate_soc)
# calculate Q3 + 1.5(Q3-Q1) to identify outliers
# Q1: 0.05655       
# Q3: 0.22889       
(0.22889        - 0.05655    )*1.5 + 0.22889     # = 0.4874 
# Outlier if > 0.4874

# add the efficiency scores and the weights to the intermediate_df_rm dataset
intermediate$eff_se_outlier <- efficiency_scores_intermediate_soc
eff_intermediate <- subset(intermediate, select = c(NUTS_ID, eff_se_outlier))
# OUTLIER: FI1C, SE12, BG41, SE23


# ---------------------------------------
# Remove the outliers
population_rm <- total_data %>% 
  filter(!(NUTS_ID %in% c("EL65", "EL61", "EL52", "EL63", "EL53", "ES11", "SK04", "SK03", "HR03", 
                          "FRI2", "AT32", "FRF3", "FRC2", "BE34", "NL33", "CZ02", "FR10", "EL30", 
                          "PT15", "FRL0", "NL34", "FI1C", "SE12", "BG41", "SE23")))


# Define the subsets
rural <- population_rm %>% filter(Typology == "Pred. rural") # 57
intermediate <- population_rm %>% filter(Typology == "Intermediate") # 52 
urban <- population_rm %>% filter(Typology == "Pred. urban") # 20

# ---------------------------------------
# define the input and output matrixes
x_rural <- matrix(c(rural$ghg_uaa, rural$conv_share, rural$div,
                    rural$ammonia, rural$pesti_risk, rural$high_nitrate), ncol = 6)
y_rural <- matrix(c(rural$value_prod), ncol = 1)

x_intermediate <- matrix(c(intermediate$ghg_uaa, intermediate$conv_share, intermediate$div,
                           intermediate$ammonia, intermediate$pesti_risk, intermediate$high_nitrate), ncol = 6)
y_intermediate <- matrix(c(intermediate$value_prod), ncol = 1)

x_urban <- matrix(c(urban$ghg_uaa, urban$conv_share, urban$div,
                    urban$ammonia, urban$pesti_risk, urban$high_nitrate), ncol = 6)
y_urban <- matrix(c(urban$value_prod), ncol = 1)

# Define the input and output matrices for SE
x_rural_s <- matrix(c(rural$value_prod), ncol = 1)
y_rural_s <- matrix(c(rural$inc_awu, rural$female_ratio, rural$age_ratio, 
                      rural$share_full, rural$antibiotics_inv), ncol = 5)

x_intermediate_s <- matrix(c(intermediate$value_prod), ncol = 1)
y_intermediate_s <- matrix(c(intermediate$inc_awu, intermediate$female_ratio, intermediate$age_ratio, 
                             intermediate$share_full, intermediate$antibiotics_inv), ncol = 5)

x_urban_s <- matrix(c(urban$value_prod), ncol = 1)
y_urban_s <- matrix(c(urban$inc_awu, urban$female_ratio, urban$age_ratio, 
                      urban$share_full, urban$antibiotics_inv), ncol = 5)

# ---------------------------------------
# Assess efficiency

# Urban
urban$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_urban,
                       y_urban,
                       numeric(nrow(urban)),
                       urban))  

urban$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_urban_s,
                        y_urban_s,
                        numeric(nrow(urban)),
                        urban))  


# rural
rural$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_rural,
                       y_rural,
                       numeric(nrow(rural)),
                       rural))  

rural$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_rural_s,
                        y_rural_s,
                        numeric(nrow(rural)),
                        rural))  


# intermediate
intermediate$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_intermediate,
                       y_intermediate,
                       numeric(nrow(intermediate)),
                       intermediate))  

intermediate$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_intermediate_s,
                        y_intermediate_s,
                        numeric(nrow(intermediate)),
                        intermediate))  

# ---------------------------------------
# We create a dataset only removing the outliers in SE, and NL33, which is classified
# as an outlier in all dimensions (the only outlier in all dimensions in EE)
population_rm_ee <- total_data %>% 
  filter(!(NUTS_ID %in% c("CZ02", "FR10", "EL30", "FI1C", "SE12", "BG41", "SK04",
                          "SK03", "HR03", "FRI2", "AT32", "FRF3", "FRC2", "BE34", 
                          "NL33", "SE23")))

rural_rm <- population_rm_ee %>% filter(Typology == "Pred. rural") # 63
intermediate_rm <- population_rm_ee %>% filter(Typology == "Intermediate") # 55

# Define the input and output matrices for SE
x_rural_s <- matrix(c(rural_rm$value_prod), ncol = 1)
y_rural_s <- matrix(c(rural_rm$inc_awu, rural_rm$female_ratio, rural_rm$age_ratio, 
                      rural_rm$share_full, rural_rm$antibiotics_inv), ncol = 5)

x_intermediate_s <- matrix(c(intermediate_rm$value_prod), ncol = 1)
y_intermediate_s <- matrix(c(intermediate_rm$inc_awu, intermediate_rm$female_ratio, intermediate_rm$age_ratio, 
                             intermediate_rm$share_full, intermediate_rm$antibiotics_inv), ncol = 5)

# Rural
rural_rm$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_rural_s,
                        y_rural_s,
                        numeric(nrow(rural_rm)),
                        rural_rm))  

# intermediate
intermediate_rm$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_intermediate_s,
                        y_intermediate_s,
                        numeric(nrow(intermediate_rm)),
                        intermediate_rm))  
# ---------------------------------------

# add the special cases to the livestock results
outliers_rural <- rural_rm %>% select(NUTS_ID, eff_se_lpsolve) %>%
  filter(NUTS_ID %in% c("EL65", "EL61", "EL52", "EL63", "EL53", "ES11"))
outliers_rural$eff_ee_lpsolve <- 1.05

# add outliers_ls to the rural data
rural <- full_join(rural, outliers_rural)

# add the special cases to the livestock results
outliers_intermediate <- intermediate_rm %>% select(NUTS_ID, eff_se_lpsolve) %>%
  filter(NUTS_ID %in% c("PT15", "FRL0", "NL34"))
outliers_intermediate$eff_ee_lpsolve <- 1.05

# add outliers_ls to the intermediate data
intermediate <- full_join(intermediate, outliers_intermediate)


# adding the SE special cases and assessing EE
population_rm_se <- total_data %>% 
  filter(!(NUTS_ID %in% c("NL33", "PT15", "FRL0", "NL34", "EL65", 
                          "EL61", "EL52", "EL63", "EL53", "ES11", "FI1C", 
                          "SE12", "BG41", "SK04", "SK03", "HR03", "FRI2", 
                          "AT32", "FRF3", "FRC2")))

rural_rm <- population_rm_se %>% filter(Typology == "Pred. rural") # 58
urban_rm <- population_rm_se %>% filter(Typology == "Pred. urban") # 23
intermediate_rm <- population_rm_se %>% filter(Typology == "Intermediate") # 53


x_rural <- matrix(c(rural_rm$ghg_uaa, rural_rm$conv_share, rural_rm$div,
                    rural_rm$ammonia, rural_rm$pesti_risk, rural_rm$high_nitrate), ncol = 6)
y_rural <- matrix(c(rural_rm$value_prod), ncol = 1)

x_urban <- matrix(c(urban_rm$ghg_uaa, urban_rm$conv_share, urban_rm$div,
                    urban_rm$ammonia, urban_rm$pesti_risk, urban_rm$high_nitrate), ncol = 6)
y_urban <- matrix(c(urban_rm$value_prod), ncol = 1)

x_intermediate <- matrix(c(intermediate_rm$ghg_uaa, intermediate_rm$conv_share, intermediate_rm$div,
                           intermediate_rm$ammonia, intermediate_rm$pesti_risk, intermediate_rm$high_nitrate), ncol = 6)
y_intermediate <- matrix(c(intermediate_rm$value_prod), ncol = 1)

# Rural
rural_rm$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_rural,
                       y_rural,
                       numeric(nrow(rural_rm)),
                       rural_rm))  
# intermediate
intermediate_rm$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_intermediate,
                       y_intermediate,
                       numeric(nrow(intermediate_rm)),
                       intermediate_rm))  
# Rural
urban_rm$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_urban,
                       y_urban,
                       numeric(nrow(urban_rm)),
                       urban_rm))  



# add the special cases to the rural results
outliers_rural <- rural_rm %>% select(NUTS_ID, eff_ee_lpsolve) %>%
  filter(NUTS_ID %in% c("BE34"))
outliers_rural$eff_se_lpsolve <- 1.05

# add outliers_ls to the rural data
rural <- full_join(rural, outliers_rural)

outliers_intermediate <- intermediate_rm %>% select(NUTS_ID, eff_ee_lpsolve) %>%
  filter(NUTS_ID %in% c("SE23"))
outliers_intermediate$eff_se_lpsolve <- 1.05

# add outliers_ls to the intermediate data
intermediate <- full_join(intermediate, outliers_intermediate)

outliers_urban <- urban_rm %>% select(NUTS_ID, eff_ee_lpsolve) %>%
  filter(NUTS_ID %in% c("CZ02", "FR10", "EL30"))
outliers_urban$eff_se_lpsolve <- 1.05

# add outliers_ls to the urban data
urban <- full_join(urban, outliers_urban)

# ---------------------------------------
# Results for Urban, Rural and Intermediate regions

# ggplot is complaining about duplicate columns for some reason, 
# this resolves the issue. 
colnames(urban) <- make.unique(names(urban))
colnames(rural) <- make.unique(names(rural))
colnames(intermediate) <- make.unique(names(intermediate))

p_urban <- ggplot(urban, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(title = "Urban", x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) +
  xlim(0, 1.11) + ylim(0, 1.11) +
  geom_point(data = urban %>% 
               filter(str_detect(NUTS_ID, "CZ02|FR10|EL30")), 
             color = "grey", size = 3)

p_rural <- ggplot(rural, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(title= "Rural", x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) +
  xlim(0, 1.11) + ylim(0, 1.11) +
  geom_point(data = rural %>% 
               filter(str_detect(NUTS_ID, "EL65|EL61|EL52|EL63|EL53|ES11|BE34")), 
             color = "grey", size = 3)

p_intermediate <- ggplot(intermediate, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(title = "Intermediate", x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 2) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) +
  xlim(0, 1.11) + ylim(0, 1.11) +
  geom_point(data = intermediate %>% 
               filter(str_detect(NUTS_ID, "PT15|FRL0|NL34|NL12|SE23")), 
             color = "grey", size = 3)

# ------------------------------------------------------------------------------
# Assess the metafrontier (full sample efficiency)

# Create total_data_rm only including the regions which are used in the analysis
# for any of the subsets

crop_ls <- crop_ls_rm %>% select(NUTS_ID)
biogeo <- biogeo_rm %>% select(NUTS_ID)
population <- population_rm %>% select(NUTS_ID)

# merge crop_ls, biogeo and population
list1 <- list(crop_ls, biogeo, population)
total_ids <- list1 %>% reduce(full_join, by=('NUTS_ID'))# %>% na.omit()

# create total_data_rm containing only the ids in total_ids
total_data_rm <- left_join(total_ids, total_data)

total_ids$dummy <- 1
total_outlier <- full_join(total_ids, total_data)
total_outlier <- subset(total_outlier, select = c(NUTS_ID, dummy))

# meta-sample 
x_full <- matrix(c(total_data_rm$ghg_uaa, total_data_rm$conv_share, total_data_rm$div,
                   total_data_rm$ammonia, total_data_rm$pesti_risk, total_data_rm$high_nitrate), ncol = 6)
y_full <- matrix(c(total_data_rm$value_prod), ncol = 1)

x_full_s <- matrix(c(total_data_rm$value_prod), ncol = 1)
y_full_s <- matrix(c(total_data_rm$inc_awu, total_data_rm$female_ratio, total_data_rm$age_ratio, 
                     total_data_rm$share_full, total_data_rm$antibiotics_inv), ncol = 5)


# EE
total_data_rm$eff_ee_lpsolve <- print(
  dea_in_fixed_weights(x_full,
                       y_full,
                       numeric(nrow(total_data_rm)),
                       total_data_rm))              

# SE
total_data_rm$eff_se_lpsolve <- print(
  dea_out_fixed_weights(x_full_s,
                        y_full_s,
                        numeric(nrow(total_data_rm)),
                        total_data_rm))


# ------------------------------------------------------------------------------
# Plotting the results 
# ------------------------------------------------------------------------------

# ------------------------------------
# Figure 5
# ------------------------------------
# meta-plot without labels and with larger dots
p_meta_eff <- ggplot(total_data_rm, aes(x = eff_ee_lpsolve, y = eff_se_lpsolve)) +
  geom_point(size = 3, color = "black") +  # Points
  geom_text(aes(label = NUTS_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add labels
  labs(title = "Meta efficiency", x = "Efficiency EE", y = "Efficiency SE") +
  theme_bw() +
  xlim(0, 1.05) + ylim(0, 1.05)

ggsave("figure_5.png", p_meta_eff, width = 10, height = 5)



# ------------------------------------
# create same plot as p_meta_eff but without labeling the dots with NUTS_ID
# this is the basis layer for Figure 7

p_meta <- ggplot(total_data_rm, aes(x = eff_ee_lpsolve , y = eff_se_lpsolve)) +
  geom_point() +
  labs(title = "Meta efficiency", x = "Efficiency EE",
       y = "Efficiency SE") +
  theme_bw() +
  geom_point(size = 3, color = "grey") + aes(ymin=0)  + aes(xmin=0) +
  xlim(0, 1.05) + ylim(0, 1.05)


# first we need to add the efficiency scores of each grouping to the 
# total_data_rm dataset.
arable_crop <- arable_crop %>% 
  rename(eff_ee_arable_crop = eff_ee_lpsolve, eff_se_arable_crop = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_arable_crop, eff_se_arable_crop)

crop_high <- crop_high %>% 
  rename(eff_ee_crop_high = eff_ee_lpsolve, eff_se_crop_high = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_crop_high, eff_se_crop_high)

livestock <- livestock %>%
  rename(eff_ee_livestock = eff_ee_lpsolve, eff_se_livestock = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_livestock, eff_se_livestock)

continental <- continental %>%
  rename(eff_ee_continental = eff_ee_lpsolve, eff_se_continental = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_continental, eff_se_continental)

atlantic <- atlantic %>%
  rename(eff_ee_atlantic = eff_ee_lpsolve, eff_se_atlantic = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_atlantic, eff_se_atlantic)

mediterranean <- mediterranean %>%
  rename(eff_ee_mediterranean = eff_ee_lpsolve, eff_se_mediterranean = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_mediterranean, eff_se_mediterranean)

urban <- urban %>%
  rename(eff_ee_urban = eff_ee_lpsolve, eff_se_urban = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_urban, eff_se_urban)

rural <- rural %>%
  rename(eff_ee_rural = eff_ee_lpsolve, eff_se_rural = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_rural, eff_se_rural)

intermediate <- intermediate %>%
  rename(eff_ee_intermediate = eff_ee_lpsolve, eff_se_intermediate = eff_se_lpsolve) %>%
  select(NUTS_ID, eff_ee_intermediate, eff_se_intermediate)

list1 <- list(arable_crop, crop_high, livestock, continental, atlantic, mediterranean, 
              urban, rural, intermediate, total_data_rm)
total_data_rm <- list1 %>% reduce(full_join, by=('NUTS_ID'))


h <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) + 
  geom_point(data = total_data_rm %>% filter(ls_dum == 1), color = "black", size = 3) +
  labs(title = "Livestock") +
  geom_point(data = total_data_rm, aes(x = eff_ee_livestock, y = eff_se_livestock), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(ls_dum == 1),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_livestock, yend = eff_se_livestock),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_livestock, y = eff_se_livestock, label = NUTS_ID),
                  size = 4, color = "black", max.overlaps = 20) 

i <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(crop_high_dum == 1), color = "black", size = 3) +
  labs(title = "High-value Crop") +
  geom_point(data = total_data_rm, aes(x = eff_ee_crop_high, y = eff_se_crop_high), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(crop_high_dum == 1),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_crop_high, yend = eff_se_crop_high),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_crop_high, y = eff_se_crop_high, label = NUTS_ID),
                  size = 4, color = "black", max.overlaps = 20)



j <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(arable_crop_dum == 1), color = "black", size = 3) +
  labs(title = "Arable Crop") +
  geom_point(data = total_data_rm, aes(x = eff_ee_arable_crop, y = eff_se_arable_crop), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(arable_crop_dum == 1),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_arable_crop, yend = eff_se_arable_crop),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_arable_crop, y = eff_se_arable_crop, label = NUTS_ID),
                  size = 4, color = "black", max.overlaps = 20)

a <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(`Climatic Zone` == "Continental"), color = "black", size = 3) +
  labs(title = "Continental") +
  geom_point(data = total_data_rm, aes(x = eff_ee_continental, y = eff_se_continental), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(`Climatic Zone` == "Continental"),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_continental, yend = eff_se_continental),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_continental, y = eff_se_continental, label = NUTS_ID),
                  size = 5, color = "black", max.overlaps = 20)

b <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(`Climatic Zone` == "Atlantic"), color = "black", size = 3) +
  labs(title = "Atlantic") +
  geom_point(data = total_data_rm, aes(x = eff_ee_atlantic, y = eff_se_atlantic), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(`Climatic Zone` == "Atlantic"),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_atlantic, yend = eff_se_atlantic),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_atlantic, y = eff_se_atlantic, label = NUTS_ID),
                  size = 5, color = "black", max.overlaps = 20)

c <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(`Climatic Zone` == "Mediterranean"), color = "black", size = 3) +
  labs(title = "Mediterranean") +
  geom_point(data = total_data_rm, aes(x = eff_ee_mediterranean, y = eff_se_mediterranean), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(`Climatic Zone` == "Mediterranean"),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_mediterranean, yend = eff_se_mediterranean),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_mediterranean, y = eff_se_mediterranean, label = NUTS_ID),
                  size = 5, color = "black", max.overlaps = 20)


d <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(Typology == "Pred. rural"), color = "black", size = 3) +
  labs(title = "Rural") +
  geom_point(data = total_data_rm, aes(x = eff_ee_rural, y = eff_se_rural), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(Typology == "Pred. rural"),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_rural, yend = eff_se_rural),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_rural, y = eff_se_rural, label = NUTS_ID),
                  size = 4, color = "black", max.overlaps = 20)

e <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(Typology == "Pred. urban"), color = "black", size = 3) +
  labs(title = "Urban") +
  geom_point(data = total_data_rm, aes(x = eff_ee_urban, y = eff_se_urban), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(Typology == "Pred. urban"),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_urban, yend = eff_se_urban),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_urban, y = eff_se_urban, label = NUTS_ID),
                  size = 4, color = "black", max.overlaps = 20)

g <- p_meta + 
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "grey25", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, fill = "grey25", alpha = 0.3) +
  geom_point(data = total_data_rm %>% filter(Typology == "Intermediate"), color = "black", size = 3) +
  labs(title = "Intermediate") +
  geom_point(data = total_data_rm, aes(x = eff_ee_intermediate, y = eff_se_intermediate), color = "skyblue4", size = 3) +
  geom_segment(data = total_data_rm %>% filter(Typology == "Intermediate"),
               aes(x = eff_ee_lpsolve, y = eff_se_lpsolve,
                   xend = eff_ee_intermediate, yend = eff_se_intermediate),
               color = "grey63", linetype = "dashed") +
  geom_text_repel(data = total_data_rm, aes(x = eff_ee_intermediate, y = eff_se_intermediate, label = NUTS_ID),
                  size = 4, color = "black", max.overlaps = 20)


# ---------------------------------------
# Figure 7: Meta efficiency and the group efficiencies
# ---------------------------------------
fig7 <- ggarrange(h, j, i, a, b, c, d, e, g)

ggsave("figure_7.png", fig7, width = 15, height = 15)

# ---------------------------------------
# Figure 8: Group efficiencies
# ---------------------------------------
fig8 <- ggarrange(p_livestock, p_arable_crop, p_crop_high, p_continental, p_atlantic, 
          p_mediterranean, p_urban, p_rural, p_intermediate, ncol = 3, nrow = 3)

ggsave("figure_8.png", fig8, width = 10, height = 10)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ----------------------------
# Appendix 2
# robustness check
# different benchmarking-indicators for the assurance ranges
# ----------------------------

# conv_share instead
x_full <- matrix(c(total_data_rm$conv_share, total_data_rm$ghg_uaa, total_data_rm$div,
                   total_data_rm$ammonia, total_data_rm$pesti_risk, total_data_rm$high_nitrate), ncol = 6)
y_full <- matrix(c(total_data_rm$value_prod), ncol = 1)

total_data_rm$ee_robust_conv <- print(
  dea_in_fixed_weights(x_full,
                       y_full,
                       numeric(nrow(total_data_rm)),
                       total_data_rm))
# div instead
x_full <- matrix(c(total_data_rm$div, total_data_rm$ghg_uaa, total_data_rm$conv_share,
                   total_data_rm$ammonia, total_data_rm$pesti_risk, total_data_rm$high_nitrate), ncol = 6)
y_full <- matrix(c(total_data_rm$value_prod), ncol = 1)

total_data_rm$ee_robust_div <- print(
  dea_in_fixed_weights(x_full,
                       y_full,
                       numeric(nrow(total_data_rm)),
                       total_data_rm))

p_robust_conv <- ggplot(total_data_rm, aes(x = eff_ee_lpsolve, y = ee_robust_conv)) +
  geom_point() +
  labs(title = "Robustness check, % conventional", x = "Efficiency EE",
       y = "Efficiency EE robust") +
  theme_bw() +
  geom_point(size = 3) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

p_robust_div <- ggplot(total_data_rm, aes(x = eff_ee_lpsolve, y = ee_robust_div)) +
  geom_point() +
  labs(title = "Robustness check, diversity index", x = "Efficiency EE",
       y = "Efficiency EE robust") +
  theme_bw() +
  geom_point(size = 3) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

# ----
# Figure A.1
ggarrange(p_robust_conv, p_robust_div)

# varying the social indicator used as benchmark
x_full_s <- matrix(c(total_data_rm$value_prod), ncol = 1)
y_full_s <- matrix(c(total_data_rm$female_ratio, total_data_rm$inc_awu, total_data_rm$age_ratio, 
                     total_data_rm$share_full, total_data_rm$antibiotics_inv), ncol = 5)


total_data_rm$se_robust_fem <- print(
  dea_out_fixed_weights(x_full_s,
                        y_full_s,
                        numeric(nrow(total_data_rm)),
                        total_data_rm))

y_full_s <- matrix(c(total_data_rm$age_ratio, total_data_rm$inc_awu, total_data_rm$female_ratio, 
                     total_data_rm$share_full, total_data_rm$antibiotics_inv), ncol = 5)

total_data_rm$se_robust_age <- print(
  dea_out_fixed_weights(x_full_s,
                        y_full_s,
                        numeric(nrow(total_data_rm)),
                        total_data_rm))

y_full_s <- matrix(c(total_data_rm$share_full, total_data_rm$inc_awu, total_data_rm$female_ratio, 
                     total_data_rm$age_ratio, total_data_rm$antibiotics_inv), ncol = 5)

total_data_rm$se_robust_educ <- print(
  dea_out_fixed_weights(x_full_s,
                        y_full_s,
                        numeric(nrow(total_data_rm)),
                        total_data_rm))

p_robust_fem <- ggplot(total_data_rm, aes(x = eff_se_lpsolve, y = se_robust_fem)) +
  geom_point() +
  labs(title = "Robustness check, % female farmers", x = "Efficiency SE",
       y = "Efficiency EE robust") +
  theme_bw() +
  geom_point(size = 3) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

p_robust_age <- ggplot(total_data_rm, aes(x = eff_se_lpsolve, y = se_robust_age)) +
  geom_point() +
  labs(title = "Robustness check, Geneartional renewal", x = "Efficiency SE",
       y = "Efficiency EE robust") +
  theme_bw() +
  geom_point(size = 3) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

p_robust_educ <- ggplot(total_data_rm, aes(x = eff_se_lpsolve, y = se_robust_educ)) +
  geom_point() +
  labs(title = "Robustness check, % fully educated farmers", x = "Efficiency SE",
       y = "Efficiency EE robust") +
  theme_bw() +
  geom_point(size = 3) + aes(ymin=0)  + aes(xmin=0) +
  geom_text(aes(label = NUTS_ID), nudge_x = 0.01, nudge_y = 0.01) + 
  geom_abline(intercept = 0, slope = 1, color = "red")


#-------------
# Figure A.2
ggarrange(p_robust_fem, p_robust_age, p_robust_educ, ncol = 3)


# ----------------------------
# Appendix 3
# Illustration of the outliers
# ----------------------------

# ---------------------------
# Figure A3
# plot value_UAA against NUTS_ID and  highlight NL33 in red.
figA3 <- total_data %>% 
  ggplot(aes(x = NUTS_ID, y = value_prod)) +
  geom_point() +
  geom_text(aes(label = NUTS_ID), nudge_y = 0.01) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Value per UAA") +
  xlab("NUTS_ID") +
  geom_point(data = total_data %>% filter(NUTS_ID == "NL33"), color = "red", size = 3) +
  theme_bw()

# save the plot to the wd under the name "figure_A3"
ggsave("figure_A3.png", figA3, width = 10, height = 5)

# ---------------------------
# Figure A4
# plot value_UAA against NUTS_ID and  highlight AT32, FI1C, FRC2, FRF3, BG41, 
# FRI2, HR03, SE12, SK03 and SK04 in red. set ylim to 5
figA4 <- total_data %>% 
  ggplot(aes(x = NUTS_ID, y = value_prod)) +
  geom_point() +
  geom_text(aes(label = NUTS_ID), nudge_y = 0.01) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Value per UAA") +
  xlab("NUTS_ID") +
  geom_point(data = total_data %>% filter(NUTS_ID %in% c("AT32", "FI1C", 
                                                         "FRC2", "FRF3", "FRI2", 
                                                         "HR03", "SK03", "SK04",
                                                         "SE12", "BG41")), 
             color = "red", size = 3) +
  ylim(0, 2) +
  theme_bw()

ggsave("figure_A4.png", figA4, width = 10, height = 5)

# Figure A.5. shows the share of female farmers, generational renewal, share of fully educated farmers and the inverse antibiotic usage divided by value per UAA. 
# The figure highlights the regions AT32, FI1C, FRC2, FRF3, BG41, 
# FRI2, HR03, SE12, SK03 and SK04 in red.
figA5a <- total_data %>% 
  ggplot(aes(x = NUTS_ID, y = (female_ratio/value_prod))) +
  geom_point() +
  geom_text(aes(label = NUTS_ID), nudge_y = 0.01) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Share of Female farmers") +
  xlab("NUTS_ID") +
  geom_point(data = total_data %>% filter(NUTS_ID %in% c("AT32", "FI1C",
                                                         "FRC2", "FRF3", "FRI2", 
                                                         "HR03", "SK03", "SK04",
                                                         "SE12", "BG41")), 
             color = "red", size = 3) +
  theme_bw()

figA5b <- total_data %>% 
  ggplot(aes(x = NUTS_ID, y = (age_ratio/value_prod))) +
  geom_point() +
  geom_text(aes(label = NUTS_ID), nudge_y = 0.01) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Share of Female farmers") +
  xlab("NUTS_ID") +
  geom_point(data = total_data %>% filter(NUTS_ID %in% c("AT32", "FI1C",
                                                         "FRC2", "FRF3", "FRI2", 
                                                         "HR03", "SK03", "SK04",
                                                         "SE12", "BG41")), 
             color = "red", size = 3) +
  theme_bw()

figA5c <- total_data %>% 
  ggplot(aes(x = NUTS_ID, y = (share_full/value_prod))) +
  geom_point() +
  geom_text(aes(label = NUTS_ID), nudge_y = 0.01) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Share of Female farmers") +
  xlab("NUTS_ID") +
  geom_point(data = total_data %>% filter(NUTS_ID %in% c("AT32", "FI1C",
                                                         "FRC2", "FRF3", "FRI2", 
                                                         "HR03", "SK03", "SK04",
                                                         "SE12", "BG41")), 
             color = "red", size = 3) +
  theme_bw()

figA5d <- total_data %>% 
  ggplot(aes(x = NUTS_ID, y = (antibiotics_inv/value_prod))) +
  geom_point() +
  geom_text(aes(label = NUTS_ID), nudge_y = 0.01) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Share of Female farmers") +
  xlab("NUTS_ID") +
  geom_point(data = total_data %>% filter(NUTS_ID %in% c("AT32", "FI1C",
                                                         "FRC2", "FRF3", "FRI2", 
                                                         "HR03", "SK03", "SK04",
                                                         "SE12", "BG41")), 
             color = "red", size = 3) +
  theme_bw()

# Figure A5
figA5 <- ggarrange(figA5a, figA5b, figA5c, figA5d, ncol = 4)
ggsave("figure_A5.png", figA5, width = 10, height = 5)

# ----------------------------
# End
