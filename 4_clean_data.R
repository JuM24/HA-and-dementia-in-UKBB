# This code cleans the data before analysis.




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

# remove those without dates of hearing loss or hearing aid because 
# it prevents us from ascertaining the timeline
no_aid_date <- filter(hear, hear_aid_any == 0 & hear_aid_nodate == 1)
hear <- filter(hear, !id %in% no_aid_date$id) %>%
  filter(hear_loss_any == 0 | !is.na(date_hear_loss_any))

# retain only those whose dates of HL are before their dates of HA
hear <- filter(hear, hear_aid_any == 0 | (date_hear_loss_any <= date_hear_aid_any))


# determine the assessment that is closest to HL start and which will be used for covariate ascertainment 
hear <- find_closest_non_missing(hear, 'education', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'g', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'srt_min', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'tinnitus_sr', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'soc_isol', date_hear_loss_any = 'date_hear_loss_any')

# Those without imputed censoring date get the value '0' for data provider value
hear$data_provider_last[is.na(hear$data_provider_last)] <- '0'
hear$data_provider_last <- as.factor(hear$data_provider_last)
# Same for data provider used as covariate
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

# set the censoring dates to the date that occurs earliest
hear <- hear %>%
  mutate(censor_date = reduce(across(c('dementia_date', 'death_date', 'follow_loss_date',
                                       'data_cens_date')), pmin, na.rm = TRUE))

# for those that are labelled as having experienced the outcome 
# after the censoring date, set the outcome to 0
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


# define the 'grace period', the period in which start of use of 
# HA will still lead to the classification of 'treated'
hear$hear_loss_time <- as.numeric(difftime(hear$date_hear_aid_any, 
                                           hear$date_hear_loss_any, units='days'))/365.25
hear$grace_period[hear$hear_loss_time <= 1 | hear$hear_aid_any == 0] <- 1
hear$grace_period[hear$hear_loss_time > 1] <- 0
# for those with HA beyond the grace period, set HA status and HA origin to 0
hear$hear_aid_any[hear$grace_period == 0] <- 0
hear$hear_aid_origin[hear$grace_period == 0] <- NA

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
# otherwise (i.e., if early_cens = 0), keep old value. The sampling is 
# biased so that there is a chance of being assigned to the treatment group that corresponds with
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








### add healthcare contact controls ###


time_zero <- hear %>%
  select(id, date_hear_loss_any, censor_date)

## inpatient diagnoses
inpatient <- readRDS('inpatient_diagnoses.rds')
colnames(inpatient)[colnames(inpatient) == 'diagnosis'] <- 'code'
inpatient$date <- as.Date(inpatient$date, format = '%Y-%m-%d')
inpatient <- merge(inpatient, time_zero, by = 'id')

# number of secondary hospital contacts in the last five years
inpatient_contact <- inpatient %>%
  filter(date < date_hear_loss_any & 
           date > (date_hear_loss_any - 5*365.25)) %>% # keep only those with contact dates before HL
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
data_period <- read.csv('data_period.csv') %>%
  rename(id = eid)
data_period$from <- as.Date(data_period$from, format = '%Y-%m-%d')
data_period$to <- as.Date(data_period$to, format = '%Y-%m-%d')

# calculate date 5 years before HL to get period of interest
gp_all$gp_start_date <- gp_all$date_hear_loss_any - 5*365.25

# merge period of interest back into registration period data
dates_per_person <- distinct(gp_all, id, .keep_all = TRUE)
data_period <- merge(data_period,
                     select(dates_per_person, id, gp_start_date, date_hear_loss_any),
                     by = 'id')

# define different extents of overlap between periods of registration and period of interest
data_period$outside_period <- 1
# set people to 0 whose periods of registration do not overlap with period of interest
data_period$outside_period[(data_period$from > data_period$date_hear_loss_any) |
                             (data_period$to < data_period$gp_start_date)] <- 0
# those that are fully within period of registration are unproblematic
data_period$outside_period[(data_period$from <= data_period$gp_start_date) &
                             (data_period$to >= data_period$date_hear_loss_any)] <- 2
# calculate the duration of overlap
# some don't have registration data during our period of interest
data_period$gp_data_duration[data_period$outside_period == 0] <- NA
# others have complete data
data_period$gp_data_duration[data_period$outside_period == 2] <- 5
# for those whose periods of interest partially overlap with the period of 
# registration, we have to figure out how much it reduces the required 5 years

# period of registration starts before but ends within the period of interest
data_period$gp_data_duration[data_period$outside_period == 1 &
                               (data_period$gp_start_date >= data_period$from) &
                               (data_period$date_hear_loss_any >= data_period$to)] <- 
  as.numeric(difftime(data_period$to[data_period$outside_period == 1 &
                            (data_period$gp_start_date >= data_period$from) &
                            (data_period$date_hear_loss_any >= data_period$to)],
           data_period$gp_start_date[data_period$outside_period == 1 &
                                       (data_period$gp_start_date >= data_period$from) &
                                       (data_period$date_hear_loss_any >= data_period$to)],
           units = 'days'))/365.25
# period of registration starts within and ends within the period of interest
data_period$gp_data_duration[data_period$outside_period == 1 &
                               (data_period$gp_start_date <= data_period$from) &
                               (data_period$date_hear_loss_any >= data_period$to)] <- 
  as.numeric(difftime(data_period$to[data_period$outside_period == 1 &
                                       (data_period$gp_start_date <= data_period$from) &
                                       (data_period$date_hear_loss_any >= data_period$to)],
                      data_period$from[data_period$outside_period == 1 &
                                         (data_period$gp_start_date <= data_period$from) &
                                         (data_period$date_hear_loss_any >= data_period$to)],
                      units = 'days'))/365.25
# period of registration starts within and ends after the period of interest
data_period$gp_data_duration[data_period$outside_period == 1 &
                               (data_period$gp_start_date <= data_period$from) &
                               (data_period$date_hear_loss_any <= data_period$to)] <- 
as.numeric(difftime(data_period$date_hear_loss_any[data_period$outside_period == 1 &
                                          (data_period$gp_start_date <= data_period$from) &
                                          (data_period$date_hear_loss_any <= data_period$to)],
         data_period$from[data_period$outside_period == 1 &
                            (data_period$gp_start_date <= data_period$from) &
                            (data_period$date_hear_loss_any <= data_period$to)],
         units = 'days'))/365.25


# sum partially overlapping periods
data_period_partial <- data_period %>%
  filter(outside_period == 1) %>%
  group_by(id) %>%
  mutate(gp_data_duration = sum(gp_data_duration)) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, outside_period, gp_data_duration)

# remove duplicate records of people that have data for entire period or
# no relevant registration data at all
data_period_complete <- data_period %>%
  filter(outside_period != 1) %>%
  arrange(desc(outside_period)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, outside_period, gp_data_duration)

# combine the periods for all participants and - in case of duplicates per id -
# keep just the relevant registration period per id
data_period <- rbind(data_period_partial, data_period_complete) %>%
  arrange(desc(outside_period)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, gp_data_duration)

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
  # keep only events before HL, but no more than 5 years before,
  filter(event_dt < date_hear_loss_any & 
           event_dt > gp_start_date) %>%
  distinct(id, event_dt, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(gp_contact = n()) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, gp_contact, censor_date_gp, censor_date_any)


# keep just participants with registrationd data within the relevant period
# and with a duration of ascertainment within that period of at least 1 year
gp_contact <- merge(gp_contact, data_period, by = 'id', all.y = TRUE) %>%
  filter(!is.na(gp_data_duration) & gp_data_duration > 1)

# those with full GP data but no contact, have not visited the GP
gp_contact$gp_contact[is.na(gp_contact$gp_contact)] <- 0

# merge both sources and create new variable for any healthcare contact
inpatient_contact <- inpatient_contact %>%
  select(-c(date_hear_loss_any, censor_date))
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


# categorise
# inpatient data
hear$inpatient_contact_avg <- hear$inpatient_contact/5

hear$inpatient_contact_cat <- NA
hear$inpatient_contact_cat[hear$inpatient_contact_avg == 0] <- '1'    # 0
hear$inpatient_contact_cat[hear$inpatient_contact_avg > 0 &
                                       hear$inpatient_contact_avg <= 0.2] <- '2'# 0-0.2
hear$inpatient_contact_cat[hear$inpatient_contact_avg > 0.2 &
                                       hear$inpatient_contact_avg <= 0.5] <- '3'# 0.2-0.5
hear$inpatient_contact_cat[hear$inpatient_contact_avg > 0.5] <- '4'   # >0.5
hear$inpatient_contact_cat <- as.factor(hear$inpatient_contact_cat)

# GP data
hear$gp_contact_avg <- hear$gp_contact/hear$gp_data_duration
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









# INTENTION-TO-TREAT: assume no treatment switching; this is what we have for 
# now as default (`hear`): we defined HA use within the grace period but did not look 
# at treatment adherence beyond baseline



# PER-PROTOCOL: account for switching between treatments.
hear_PP <- data.frame(hear)

# switching from non-HA to HA: for the participants beyond the grace period that 
# got HA before the censoring date, set censoring at date of HA
hear_PP$censor_date[hear_PP$grace_period == 0 & 
                      hear_PP$date_hear_aid_any < hear_PP$censor_date] <- 
  hear_PP$date_hear_aid_any[hear_PP$grace_period == 0 & 
                              hear_PP$date_hear_aid_any < hear_PP$censor_date]
# for those same participants, if dementia or death occur after the (new) 
# censoring date, set them, respectively, to 0
hear_PP$dementia[hear_PP$grace_period == 0 & hear_PP$dementia == 1 & 
                   hear_PP$dementia_date > hear_PP$censor_date] <- 0
hear_PP$death[hear_PP$grace_period == 0 & hear_PP$death == 1 & 
                hear_PP$death_date > hear_PP$censor_date] <- 0

# switching from HA to non-HA (i.e., HA cessation): this will not identify 
# everybody  that switched since only some participants participated in 
# assessments 1, 2, and 3, or have EHR data available about HA cessation, 
# but it's better than nothing
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
saveRDS(hear, file = 'hearing_masterfile_ITT_ALT.rds')
saveRDS(hear_PP, file = 'hearing_masterfile_PP_ALT.rds')

rm(list = ls()); gc()