# analogous to `4_clean_data.R` but prepared to estimate the effect of HA on
# subsequent(post-randomisation) healthcare usilisation

library(tidyverse)
library(MatchIt)

source('0_helper_functions.R')

hear <- readRDS('hearing_masterfile_prepped.rds')

# file with censoring dates
cens_dates <- readxl::read_excel('censoring_dates.xlsx')
cens_dates$date <- as.Date(cens_dates$date, format = '%d.%m.%Y')

# we are interested in only those participants that have hearing loss; 
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


# remove those with dementia before or at the same date as hearing loss
hear <- filter(hear, dementia == 0 | (date_hear_loss_any < dementia_date))

# remove those without dates of hearing loss or hearing aid because it prevents us from ascertaining the timeline
no_aid_date <- filter(hear, hear_aid_any == 0 & hear_aid_nodate == 1)
hear <- filter(hear, !id %in% no_aid_date$id) %>%
  filter(hear_loss_any == 0 | !is.na(date_hear_loss_any))

# select only those that were diagnosed with HL before getting HA
hear <- filter(hear, hear_aid_any == 0 | (date_hear_loss_any <= date_hear_aid_any))


# determine the assessment that is closest to HL start and which will be used for covariate measurement 
hear <- find_closest_non_missing(hear, 'education', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'g', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'srt_min', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'tinnitus_sr', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'soc_isol', date_hear_loss_any = 'date_hear_loss_any')

# those without imputed censoring date get the value '0' for data provider value
hear$data_provider_last[is.na(hear$data_provider_last)] <- '0'
hear$data_provider_last <- as.factor(hear$data_provider_last)
# same for data provider used as confounder
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

# set the censoring dates to the date that occurs earlier
hear <- hear %>%
  mutate(censor_date = reduce(across(c('dementia_date', 'death_date', 'follow_loss_date',
                                       'data_cens_date')), pmin, na.rm = TRUE))

# for those that are labelled as having experienced the outcome after the censoring date, set the outcome to 0
hear$dementia[hear$censor_date < hear$dementia_date] <- 0
hear$death[hear$censor_date < hear$death_date] <- 0

# some will have experienced HL only after the censoring date; remove those
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



# to factors
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








### add healthcare contact control ###


time_zero <- hear %>%
  select(id, date_hear_loss_any, censor_date)



## inpatient diagnoses
inpatient <- readRDS('inpatient_diagnoses.rds')
colnames(inpatient)[colnames(inpatient) == 'diagnosis'] <- 'code'
inpatient$date <- as.Date(inpatient$date, format = '%Y-%m-%d')
inpatient <- merge(inpatient, time_zero, by = 'id')

# number of secondary hospital contacts after time 0
inpatient_contact <- inpatient %>%
  filter(date > date_hear_loss_any & 
           date < censor_date) %>% # keep only those with contact dates after HL
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
meds_presc <- data.table::fread('gp_scripts.csv', sep='\t', header=TRUE, quote='') %>% 
  as.data.frame() %>%
  rename(id = eid)
meds_diagnoses <- data.table::fread('gp_clinical.txt', sep='\t', header=TRUE, quote='') %>%
  as.data.frame() %>%
  rename(id = eid)

# number of unique dates among prescriptions and GP diagnoses per person
# the diagnoses and prescriptions will also be used for primary care contact
meds_diagnoses$event_dt <- as.Date(meds_diagnoses$event_dt, format = '%d/%m/%Y')
meds_diagnoses <- merge(meds_diagnoses, time_zero, by = 'id')
meds_diagnoses <- subset(meds_diagnoses, select = c(id, event_dt, date_hear_loss_any,
                                                    censor_date))

meds_presc$issue_date <- as.Date(meds_presc$issue_date, format = '%d/%m/%Y')
meds_presc <- merge(meds_presc, time_zero, by = 'id')
meds_presc <- meds_presc %>% 
  select(id, issue_date, date_hear_loss_any, censor_date) %>%
  rename(event_dt = issue_date)

gp_all <- rbind(meds_presc, meds_diagnoses)

gp_all <- merge(gp_all, (hear %>% select(id, data_provider_last_gp)),
                by = 'id',
                all.x = TRUE)




# import registration periods
data_period <- read.csv('data_period.csv', row.names = 1) %>%
  rename(id = eid)
data_period$from <- as.Date(data_period$from, format = '%Y-%m-%d')
data_period$to <- as.Date(data_period$to, format = '%Y-%m-%d')

# merge period of interest into registration period data
dates_per_person <- distinct(gp_all, id, .keep_all = TRUE)
data_period <- merge(data_period,
                     select(dates_per_person, id, date_hear_loss_any),
                     by = 'id')
# set periods of registration to NA for those participants, for whom
# all periods of registration occur before HL or occur after, but their
# sum is less than 1 year
data_period$to_hear_dif <- as.numeric(difftime(data_period$to, data_period$date_hear_loss_any),
                                      units = 'days')/365.25
data_period$from_hear_dif <- as.numeric(difftime(data_period$from, data_period$date_hear_loss_any),
                                        units = 'days')/365.25
data_period$to_from_dif <- as.numeric(difftime(data_period$to, data_period$from),
                                      units = 'days')/365.25
# periods that at least partially occurr after HL are summed
data_period_positive <- data_period %>%
  filter(to_hear_dif > 0) %>%
  mutate(gp_time_after_HL = ifelse(from_hear_dif <= 0, to_hear_dif, (to_hear_dif - from_hear_dif))) %>%
  group_by(id) %>%
  mutate(gp_time_after_HL = sum(gp_time_after_HL)) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, gp_time_after_HL)
  
# we don't have to calculate anything for registration periods before HL
data_period_negative <- data_period %>%
  filter(to_hear_dif < 0) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id)
data_period_negative$gp_time_after_HL <- NA

# keep distinct IDs, prioritising relevant registration periods
data_period <- rbind(data_period_positive, data_period_negative) %>%
  distinct(id, .keep_all = TRUE)
data_period$gp_time_after_HL[data_period$gp_time_after_HL == 0] <- NA
  
# set GP data censoring date
gp_all$censor_date_gp <- zoo::as.Date('01/01/1900', format = '%d/%m/%Y')
gp_all$censor_date_gp[gp_all$data_provider_last_gp == '1'] <-
  zoo::as.Date('31/05/2017', format = '%d/%m/%Y')
gp_all$censor_date_gp[gp_all$data_provider_last_gp == '2'] <-
  zoo::as.Date('31/03/2017', format = '%d/%m/%Y')
gp_all$censor_date_gp[gp_all$data_provider_last_gp == '3'] <-
  zoo::as.Date('31/05/2016', format = '%d/%m/%Y')
gp_all$censor_date_gp[gp_all$data_provider_last_gp == '4'] <-
  zoo::as.Date('31/08/2017', format = '%d/%m/%Y')

# minimum censor date between GP ascertainment and loss to follow-up
gp_all <- gp_all %>%
  mutate(censor_date_any = reduce(across(starts_with('censor_date')), 
                                  pmin, na.rm = TRUE))



# for events and prescriptions, remove duplicate entries for each date per ID
gp_contact <- gp_all %>%
  # keep only events after HL
  filter(event_dt > date_hear_loss_any & 
           event_dt < censor_date_any) %>%
  distinct(id, event_dt, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(gp_contact = n()) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, gp_contact, censor_date_gp, censor_date_any)

# set those with gp data but NAs in gp_contact to 0; 
# the ones we labelled as such above are true NAs
gp_contact <- merge(gp_contact, data_period, by = 'id', all = TRUE)
gp_contact$gp_contact[is.na(gp_contact$gp_contact) &
                        !is.na(gp_contact$gp_time_after_HL)] <- 0

# merge both sources and create new variable for any healthcare contact
inpatient_contact <- inpatient_contact %>%
  select(-c(date_hear_loss_any, censor_date))
gp_contact <- gp_contact %>%
  select(c(id, gp_contact, censor_date_gp, censor_date_any, gp_time_after_HL))
health_contact <- merge(inpatient_contact, gp_contact, by = 'id', all = TRUE)
hear <- merge(hear, health_contact, by = 'id', all.x = TRUE)

# for those without GP contact, we have no GP data providers; use
# the inpatient provider for those participants
hear$censor_date_gp[is.na(hear$censor_date_gp) & !is.na(hear$gp_contact) &
                      hear$data_provider_freq == 'HES'] <-
  zoo::as.Date('31/05/2016', format = '%d/%m/%Y')

hear$censor_date_gp[is.na(hear$censor_date_gp) & !is.na(hear$gp_contact) & 
                      hear$data_provider_freq == 'SMR'] <-
  zoo::as.Date('31/03/2017', format = '%d/%m/%Y')

hear$censor_date_gp[is.na(hear$censor_date_gp) & !is.na(hear$gp_contact) & 
                      hear$data_provider_freq == 'PEDW'] <-
  zoo::as.Date('31/08/2017', format = '%d/%m/%Y')

# find the minimum censoring date for those participants
hear <- hear %>%
  mutate(censor_date_any = reduce(across(starts_with('censor_date')), 
                                  pmin, na.rm = TRUE))

# calculate follow-up
hear$follow_up <- as.numeric(difftime(hear$censor_date, 
                                      hear$date_hear_loss_any, units = 'days'))/365.25

# categorise
hear$inpatient_contact_avg <- hear$inpatient_contact/hear$follow_up

## participants with less than one year of follow-up get NA
hear$inpatient_contact_avg[hear$follow_up < 1] <- NA

hear$inpatient_contact_cat <- NA
hear$inpatient_contact_cat[hear$inpatient_contact_avg == 0] <- '1'    # 0
hear$inpatient_contact_cat[hear$inpatient_contact_avg > 0 &
                             hear$inpatient_contact_avg <= 0.2] <- '2'# 0-0.2
hear$inpatient_contact_cat[hear$inpatient_contact_avg > 0.2 &
                             hear$inpatient_contact_avg <= 0.5] <- '3'# 0.2-0.5
hear$inpatient_contact_cat[hear$inpatient_contact_avg > 0.5] <- '4'   # >0.5
hear$inpatient_contact_cat <- as.factor(hear$inpatient_contact_cat)

hear$gp_contact_avg <- hear$gp_contact/hear$gp_time_after_HL
hear$gp_contact_cat <- NA
hear$gp_contact_cat[hear$gp_contact_avg >= 0 & 
                      hear$gp_contact_avg <= 12] <- '1' # <9x/year

hear$gp_contact_cat[hear$gp_contact_avg > 12 & 
                      hear$gp_contact_avg <= 24] <- '2' # <22x/year

hear$gp_contact_cat[hear$gp_contact_avg > 24] <- '3'   # >22x/year
hear$gp_contact_cat <- as.factor(hear$gp_contact_cat)

# due to low numbers of non-white participants, let's set ethnicity to binary
hear$ethnicity_simple <- as.character(hear$ethnicity)
hear$ethnicity_simple[hear$ethnicity != '1'] <- '2'
hear$ethnicity_simple <- as.factor(hear$ethnicity_simple)




# export
saveRDS(hear, file = 'hearing_masterfile_subsequent_healthcare_ITT.rds')
rm(list = ls()); gc()