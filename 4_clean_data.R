# This code cleans the data before analysis.

# hearing aid emulation
library(tidyverse)
library(MatchIt)

source('0_helper_functions.R')

hear <- readRDS('hearing_masterfile_prepped.rds')

# file with censoring dates
cens_dates <- readxl::read_excel('censoring_dates.xlsx')
cens_dates$date <- as.Date(cens_dates$date, format = '%d.%m.%Y')

# We are interested in only those participants that have hearing loss; 
# thus, we remove participants without it. We do not keep those with congenital hearing loss.
# we also remove those with cochlear implants, as they cannot remove them before the SRT
hear <- filter(hear, hear_loss_any == 1 & (congenital == 0 | is.na(congenital)) & 
                 is.na(hear_congenital) & cochl_impl_any == 0 | 
                 (cochl_impl_any == 1 & date_cochl_impl_any > date_hear_loss_any))

# remove those with HL before baseline assessment due to strong selection into study
hear <- filter(hear, date_hear_loss_any >= date_0)

# for those that remain, cochlear implant date will become HA date of it's earlier than HA date
hear$date_hear_aid_any[(hear$date_hear_aid_any > hear$date_cochl_impl_any & 
                          hear$hear_aid_any == 1 & hear$cochl_impl_any == 1) |
                         (hear$hear_aid_any == 0 & hear$cochl_impl_any == 1)] <- 
  hear$date_cochl_impl_any[(hear$date_hear_aid_any > hear$date_cochl_impl_any & 
                              hear$hear_aid_any == 1 & hear$cochl_impl_any == 1) |
                           (hear$hear_aid_any == 0 & hear$cochl_impl_any == 1)]


# Remove those with dementia before or at the same date as hearing loss.
hear <- filter(hear, dementia == 0 | (date_hear_loss_any < dementia_date))

# Remove those without dates of hearing loss or hearing aid because it prevents us from ascertaining the timeline.
no_aid_date <- filter(hear, hear_aid_any == 0 & hear_aid_nodate == 1)
hear <- filter(hear, !id %in% no_aid_date$id) %>%
  filter(hear_loss_any == 0 | !is.na(date_hear_loss_any))

# Select only those that were diagnosed with HL before getting HA
hear <- filter(hear, hear_aid_any == 0 | (date_hear_loss_any <= date_hear_aid_any))


# determine the assessment that is closest to HL start and which will be used for covariate measurement 
hear <- find_closest_non_missing(hear, 'education', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'g', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'srt_min', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'tinnitus_sr', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'soc_isol', date_hear_loss_any = 'date_hear_loss_any')

# Those without imputed censoring date get the value '0' for data provider value
hear$data_provider_last[is.na(hear$data_provider_last)] <- '0'
hear$data_provider_last <- as.factor(hear$data_provider_last)
# Same for data provider used as confounder
hear$data_provider_freq[is.na(hear$data_provider_freq)] <- '0'
hear$data_provider_freq <- as.factor(hear$data_provider_freq)

# set the correct data censoring date based on data provider
hear$data_cens_date <- NA
hear$data_cens_date[hear$data_provider_last == 'HES'] <- 
  cens_dates$date[cens_dates$disorder == 'dementia' & cens_dates$data_provider == 'HES']
hear$data_cens_date[hear$data_provider_last == 'SMR'] <- 
  cens_dates$date[cens_dates$disorder == 'dementia' & cens_dates$data_provider == 'SMR']
hear$data_cens_date[hear$data_provider_last == 'PEDW'] <- 
  cens_dates$date[cens_dates$disorder == 'dementia' & cens_dates$data_provider == 'PEDW']
hear$data_cens_date[hear$data_provider_last == '0'] <- 
  min(cens_dates$date[cens_dates$disorder == 'dementia'])
hear$data_cens_date <- zoo::as.Date(hear$data_cens_date)

# set the censoring dates to the date that occurs earlier.
hear <- hear %>%
  mutate(censor_date = reduce(across(c('dementia_date', 'death_date', 'follow_loss_date',
                                       'data_cens_date')), pmin, na.rm = TRUE))

# for those that are labelled as having experienced the outcome after the censoring date, set the outcome to 0
hear$dementia[hear$censor_date < hear$dementia_date] <- 0
hear$death[hear$censor_date < hear$death_date] <- 0

# Some will have experienced HL only after the censoring date; remove those.
hear <- filter(hear, date_hear_loss_any < censor_date)

# age at HL
hear$age_USE <- as.numeric(difftime(hear$date_hear_loss_any, 
                                    hear$birth_date, units = 'days'))/365.25

# for mood disorders, remove dates after starting date 
hear$mood_dis[hear$mood_dis == 1 & hear$mood_dis_date >= hear$date_hear_loss_any] <- 0
hear$mood_dis_date[hear$mood_dis == 1 & hear$mood_dis_date >= hear$date_hear_loss_any] <- NA


# define the 'grace period'. This is the period from HL in which start of use of 
# HA will still lead to the classification as 'exposed'
hear$hear_loss_time <- as.numeric(difftime(hear$date_hear_aid_any, 
                                           hear$date_hear_loss_any, units='days'))/365.25
hear$grace_period[hear$hear_loss_time <= 1 | hear$hear_aid_any == 0] <- 1
hear$grace_period[hear$hear_loss_time > 1] <- 0
# for those with HA beyond the grace period, set HA status to 0
hear$hear_aid_any[hear$grace_period == 0] <- 0

# just keep non-missing rows and individuals with assessment data <=5 years removed from the date of HL
hear <- hear %>% drop_na(any_of(c('age_USE', 'sex', 'education_USE', 'deprivation', 
                                  'g_USE', 'srt_min_USE', 'data_provider_freq',
                                  'tinnitus_sr_USE', 'ethnicity',
                                  'dementia'))) %>%
  filter(min_diff_education <= 5*365.25 & 
           min_diff_g <= 5*365.25 & 
           min_diff_srt_min <= 5*365.25 &
           min_diff_tinnitus_sr <= 5*365.25)



# To factors.
hear <- hear %>% mutate(across(c(education_USE, sex, data_provider_freq, 
                                 soc_isol_USE, mood_dis, tinnitus_sr_USE,
                                 ethnicity), 
                               as.factor))
# Some participants might have been censored due to death or dementia after HL 
# but within the grace period. Thus, within the period between HL and censoring, 
# those participants were at the same time exposed and unexposed. To avoid this, 
# we will - among those that are censored before the grace period ends - 
# randomly assign participants to either treatment or non-treatment group.
hear$early_cens <- 0
hear$early_cens[difftime(hear$censor_date, hear$date_hear_loss_any) < 365.25] <- 1
# if early_cens == 1, randomly choose 0 or 1 and assign to hear_aid_any; 
# otherwise (i.e., if early_cens = 0), keep old value the random sampling is 
# biased so that there is only a chance of being assigned to the treatment group that corresponds with
# the prevalence of hearing aid use in the rest of the sample
prop_ha <- round(as.numeric(prop.table(table(filter(hear, early_cens == 0)$hear_aid_any))[2]), 2)
prop_han <- round(as.numeric(prop.table(table(filter(hear, early_cens == 0)$hear_aid_any))[1]), 2)
set.seed(24)
hear <- hear %>%
  rowwise %>%
  mutate(hear_aid_any = ifelse(early_cens == 1, sample(
    c(0, 1), 
    1, 
    replace = TRUE,
    prob = c(prop_han, prop_ha)),
    hear_aid_any)) %>%
  ungroup()



## add healthcare contact control
time_zero <- hear %>%
  select(id, date_hear_loss_any)

# inpatient diagnoses
inpatient <- readRDS('inpatient_diagnoses.rds')
colnames(inpatient)[colnames(inpatient) == 'diagnosis'] <- 'code'
inpatient$date <- as.Date(inpatient$date, format = '%Y-%m-%d')
inpatient <- merge(inpatient, time_zero, by = 'id')

# number of secondary hospital contacts
inpatient_contact <- inpatient %>%
  filter(date < date_hear_loss_any) %>% # keep only those with contact dates before HL
  distinct(id, date, .keep_all = TRUE) %>% # count multiple contacts on same day as one contact
  group_by(id) %>%
  mutate(inpatient_contact = n()) %>% # count per participant
  ungroup() %>%
  select(id, inpatient_contact) %>%
  distinct(id, .keep_all = TRUE)

# we assume that there are no true NAs in hospital data and that NAs are actually 0s
inpatient_contact <- merge(inpatient_contact, time_zero, by = 'id', all = TRUE)
inpatient_contact$inpatient_contact[is.na(inpatient_contact$inpatient_contact)] <- 0


## primary care
meds_regs <- read.csv('gp_registrations.txt', sep='\t', header=TRUE, quote='') %>% rename(id = eid)
meds_presc <- data.table::fread('gp_scripts.csv', sep='\t', header=TRUE, quote='') %>% 
  as.data.frame() %>%
  rename(id = eid)
meds_diagnoses <- data.table::fread('gp_clinical.txt', sep='\t', header=TRUE, quote='') %>%
  as.data.frame() %>%
  rename(id = eid)

# create a vector of all IDs that have primary care data
gp_data <- unique(c(meds_regs$id, meds_presc$id, meds_diagnoses$id))

# number of unique dates among prescriptions and GP diagnoses per person


# the diagnoses and prescriptions will also be used for primary care contact
meds_diagnoses$event_dt <- as.Date(meds_diagnoses$event_dt, format = '%d/%m/%Y')
meds_diagnoses <- merge(meds_diagnoses, time_zero, by = 'id')
meds_diagnoses <- subset(meds_diagnoses, select = c(id, event_dt, date_hear_loss_any))

meds_presc$issue_date <- as.Date(meds_presc$issue_date, format = '%d/%m/%Y')
meds_presc <- merge(meds_presc, time_zero, by = 'id')
meds_presc <- meds_presc %>% 
  select(id, issue_date, date_hear_loss_any) %>%
  rename(event_dt = issue_date)

gp_all <- rbind(meds_presc, meds_diagnoses)

# for events and prescriptions, remove duplicate entries for each date per ID
gp_contact <- gp_all %>%
  filter(event_dt < date_hear_loss_any) %>%
  distinct(id, event_dt, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(gp_contact = n()) %>%
  ungroup() %>%
  select(id, gp_contact) %>%
  distinct(id, .keep_all = TRUE)

# set those with gp data but NAs in gp_contact to 0; the rest are true NAs
gp_contact <- merge(gp_contact, time_zero, by = 'id', all = TRUE)
gp_contact$gp_contact[is.na(gp_contact$gp_contact) & gp_contact$id %in% gp_data] <- 0

# merge both sources and create new variable for any healthcare contact
inpatient_contact$date_hear_loss_any <- NULL; gp_contact$date_hear_loss_any <- NULL
health_contact <- merge(inpatient_contact, gp_contact, by = 'id', all = TRUE)
hear <- merge(hear, health_contact, by = 'id', all.x = TRUE)
# categorise inpatient contact
hear$inpatient_contact_cat <- NA
hear$inpatient_contact_cat[hear$inpatient_contact == 0] <- '0' # 0
hear$inpatient_contact_cat[hear$inpatient_contact == 1] <- '1' # 1
hear$inpatient_contact_cat[hear$inpatient_contact == 2] <- '2' # 2
hear$inpatient_contact_cat[hear$inpatient_contact >= 3 & 
                             hear$inpatient_contact <= 4] <- '3-4' # 3-4
hear$inpatient_contact_cat[hear$inpatient_contact >= 5 & 
                             hear$inpatient_contact <= 6] <- '5-6' # 5-6
hear$inpatient_contact_cat[hear$inpatient_contact >= 7]  <- '7+' # 7+
hear$inpatient_contact_cat <- as.factor(hear$inpatient_contact_cat)


## for GP data, calculate the number of GP visits in the last full year
## before time 0
# set GP data censoring date
hear$censor_date_gp <- zoo::as.Date('01/01/1900', format = '%d/%m/%Y')
hear$censor_date_gp[hear$data_provider_last_gp == '1'] <-
  zoo::as.Date('30/06/2017', format = '%d/%m/%Y')
hear$censor_date_gp[hear$data_provider_last_gp == '2'] <-
  zoo::as.Date('31/05/2017', format = '%d/%m/%Y')
hear$censor_date_gp[hear$data_provider_last_gp == '3'] <-
  zoo::as.Date('31/08/2016', format = '%d/%m/%Y')
hear$censor_date_gp[hear$data_provider_last_gp == '4'] <-
  zoo::as.Date('30/09/2017', format = '%d/%m/%Y')



## hospital specialty
diagnoses_dates <- read.csv('hesin.txt', sep='\t')  # UKB category 2006
diagnoses_dates$epistart <- as.Date(diagnoses_dates$epistart, format = '%d/%m/%Y')
diagnoses_dates <- rename(diagnoses_dates, id = eid)
diagnoses_dates <- merge(diagnoses_dates, time_zero, by = 'id', all = TRUE)

hosp_spec <- diagnoses_dates %>%
  filter(epistart < date_hear_loss_any) %>%
  distinct(id, tretspef_uni) %>%
  group_by(id) %>%
  mutate(specialty_n = n()) %>%
  distinct(id, .keep_all = TRUE)
# categorise
hosp_spec$specialty_n_cat <- NA
hosp_spec$specialty_n_cat[hosp_spec$specialty_n == 0] <- '0' # 0
hosp_spec$specialty_n_cat[hosp_spec$specialty_n == 1] <- '1' # 1
hosp_spec$specialty_n_cat[hosp_spec$specialty_n == 2] <- '2' # 2
hosp_spec$specialty_n_cat[hosp_spec$specialty_n == 3] <- '3' 
hosp_spec$specialty_n_cat[hosp_spec$specialty_n == 4] <- '4' 
hosp_spec$specialty_n_cat[hosp_spec$specialty_n >= 5]  <- '5+' # 5+

# NA were not admitted so set to 0
hear <- merge(hear, hosp_spec, by = 'id', all.x = TRUE)
hear$specialty_n_cat[is.na(hear$specialty_n_cat)] <- '0'
hear$specialty_n_cat <- as.factor(hear$specialty_n_cat)












# INTENTION-TO-TREAT: assume no treatment switching; this is what we have for now in `hear`: 
# we defined HA use within the grace period, but did not look at treatment adherence beyond the baseline



# PER-PROTOCOL: account for switching between treatments.
hear_PP <- data.frame(hear)

# Switching from non-HA to HA:
# for the participants beyond the grace period that got HA before the censoring date, set censoring at date of HA
hear_PP$censor_date[hear_PP$grace_period == 0 & 
                      hear_PP$date_hear_aid_any < hear_PP$censor_date] <- 
  hear_PP$date_hear_aid_any[hear_PP$grace_period == 0 & 
                              hear_PP$date_hear_aid_any < hear_PP$censor_date]
# for those same participants, if dementia or death if occur after the (new) censoring date set them, respectively, to 0
hear_PP$dementia[hear_PP$grace_period == 0 & hear_PP$dementia == 1 & 
                   hear_PP$dementia_date > hear_PP$censor_date] <- 0
hear_PP$death[hear_PP$grace_period == 0 & hear_PP$death == 1 & 
                hear_PP$death_date > hear_PP$censor_date] <- 0

# Switching from HA to non-HA (i.e., HA cessation)
# this will not identify everybody  that switched since only some participants participated in assessments 1, 2, and 3, or have
# EHR data available about HA cessation, but it's better than nothing
hear_PP$ha_cease <- NA
hear_PP$ha_cease[hear_PP$hear_aid_any == 1] <- 0

# HA termination according to the EHR
hear_PP$ha_cease_date_EHR <- NA
hear_PP$ha_cease[hear_PP$hear_aid_any == 1 & hear_PP$date_hear_aid_any < 
                   hear_PP$ha_cessation_date] <- 1
hear_PP$ha_cease_date_EHR[hear_PP$hear_aid_any == 1 & hear_PP$date_hear_aid_any < 
                            hear_PP$ha_cessation_date &
                            !is.na(hear_PP$ha_cessation_date)] <- 
  hear_PP$ha_cessation_date[hear_PP$hear_aid_any == 1 & hear_PP$date_hear_aid_any < 
                              hear_PP$ha_cessation_date &
                              !is.na(hear_PP$ha_cessation_date)]
if (!is.null(hear_PP$ha_cessation_date)){
  hear_PP$ha_cessation_date <- zoo::as.Date(hear_PP$ha_cessation_date)
}
# HA start before assessment 0 but no HA at assessment 0
hear_PP$ha_cease_date_0 <- NA
subset_conditions <- hear_PP$hear_aid_any == 1 & hear_PP$hear_aid_0 == 0 & 
  !is.na(hear_PP$date_0) & hear_PP$date_hear_aid_any < hear_PP$date_0
valid_indices <- which(!is.na(subset_conditions) & subset_conditions) # explicitly exclude NA indices
hear_PP$ha_cease[valid_indices] <- 1
hear_PP$ha_cease_date_0[valid_indices] <- hear_PP$date_0[valid_indices]
if (!is.null(hear_PP$ha_cease_date_0)){
  hear_PP$ha_cease_date_0 <- zoo::as.Date(hear_PP$ha_cease_date_0)
}
# HA start before assessment 1 but no HA at assessment 1
hear_PP$ha_cease_date_1 <- NA
subset_conditions <- hear_PP$hear_aid_any == 1 & hear_PP$hear_aid_1 == 0 & 
  !is.na(hear_PP$date_1) & hear_PP$date_hear_aid_any < hear_PP$date_1
valid_indices <- which(!is.na(subset_conditions) & subset_conditions)
hear_PP$ha_cease[valid_indices] <- 1
hear_PP$ha_cease_date_1[valid_indices] <- hear_PP$date_1[valid_indices]
hear_PP$ha_cease_date_1 <- zoo::as.Date(hear_PP$ha_cease_date_1)
if (!is.null(hear_PP$ha_cease_date_1)){
  hear_PP$ha_cease_date_1 <- zoo::as.Date(hear_PP$ha_cease_date_1)
}
# HA start before assessment 2 but no HA at assessment 2
hear_PP$ha_cease_date_2 <- NA
subset_conditions <- hear_PP$hear_aid_any == 1 & hear_PP$hear_aid_2 == 0 & 
  !is.na(hear_PP$date_2) & hear_PP$date_hear_aid_any < hear_PP$date_2
valid_indices <- which(!is.na(subset_conditions) & subset_conditions)
hear_PP$ha_cease[valid_indices] <- 1
hear_PP$ha_cease_date_2[valid_indices] <- hear_PP$date_2[valid_indices]
hear_PP$ha_cease_date_2 <- zoo::as.Date(hear_PP$ha_cease_date_2)
if (!is.null(hear_PP$ha_cease_date_2)){
  hear_PP$ha_cease_date_2 <- zoo::as.Date(hear_PP$ha_cease_date_2)
}
# HA start before assessment 3 but no HA at assessment 3
hear_PP$ha_cease_date_3 <- NA
subset_conditions <- hear_PP$hear_aid_any == 1 & hear_PP$hear_aid_3 == 0 & 
  !is.na(hear_PP$date_3) & hear_PP$date_hear_aid_any < hear_PP$date_3
valid_indices <- which(!is.na(subset_conditions) & subset_conditions)
hear_PP$ha_cease[valid_indices] <- 1
hear_PP$ha_cease_date_3[valid_indices] <- hear_PP$date_3[valid_indices]
hear_PP$ha_cease_date_3 <- zoo::as.Date(hear_PP$ha_cease_date_3)
if (!is.null(hear_PP$ha_cease_date_3)){
  hear_PP$ha_cease_date_3 <- zoo::as.Date(hear_PP$ha_cease_date_3)
}
# find the earliest date of HA cessation
hear_PP <- transform(hear_PP, ha_cease_date = pmin(ha_cease_date_EHR, 
                                                   ha_cease_date_0, 
                                                   ha_cease_date_1,
                                                   ha_cease_date_2, 
                                                   ha_cease_date_3, na.rm = TRUE))
hear_PP$ha_cease_date <- zoo::as.Date(hear_PP$ha_cease_date)

# if censoring occurs after the switch, set censoring to the switch date
hear_PP$censor_date[!is.na(hear_PP$ha_cease) & hear_PP$ha_cease == 1 & 
                      hear_PP$ha_cease_date < hear_PP$censor_date] <- 
  hear_PP$ha_cease_date[!is.na(hear_PP$ha_cease) & hear_PP$ha_cease == 1 & 
                          hear_PP$ha_cease_date < hear_PP$censor_date]
# if dementia or death or loss to follow-up occur after the (new) censoring date, 
# set them, respectively, to 0
hear_PP$dementia[!is.na(hear_PP$ha_cease) & hear_PP$ha_cease == 1 & 
                   hear_PP$dementia == 1 & hear_PP$dementia_date > hear_PP$censor_date] <- 0
hear_PP$death[!is.na(hear_PP$ha_cease) & hear_PP$ha_cease == 1 & 
                hear_PP$death == 1 & hear_PP$death_date > hear_PP$censor_date] <- 0
hear_PP$follow_loss[!is.na(hear_PP$ha_cease) & hear_PP$ha_cease == 1 & 
                hear_PP$follow_loss == 1 & hear_PP$follow_loss_date > hear_PP$censor_date] <- 0

# calculate follow-up
hear$follow_up <- as.numeric(difftime(hear$censor_date, 
                                      hear$date_hear_loss_any, units = 'days'))/365.25
hear_PP$follow_up <- as.numeric(difftime(hear_PP$censor_date, 
                                         hear_PP$date_hear_loss_any, units = 'days'))/365.25

# export
saveRDS(hear, file = 'hearing_masterfile_ITT.rds')
saveRDS(hear_PP, file = 'hearing_masterfile_PP.rds')

rm(list = ls()); gc()