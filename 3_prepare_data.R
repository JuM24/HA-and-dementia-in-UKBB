# This code imports all the relevant variables, creates new ones, and combines everything for the analysis.
library(tidyverse)
library(zoo)

source('0_helper_functions.R')
opt_outs <- read.csv('participant_opt_out.csv')

# import the file with all the hearing loss and hearing aid UKB variables
# field IDs 2247, 2257, 4849, 3393, 20019, 20021, 131258, 131260, 131259, 131261, 4792, 132460

hear <- readRDS('main_vars.Rds') %>%
  filter(!eid %in% opt_outs$id) %>%
  select(eid, starts_with(c('X2247.', 'X2257.', 'X4849.', 'X3393.', 'X20019.', 
                            'X20021.', 'X131258.', 'X131260.', 'X131259.', 'X131261.',
                            'X4792.', 'X132460.')))
hear$eid <- as.character(hear$eid)


# change colnames and code emtpy strings as NAs
colnames(hear) <- c('id', 'hear_dif_0', 'hear_dif_1', 'hear_dif_2', 'hear_dif_3', 
                    'hear_difn_0', 'hear_difn_1', 'hear_difn_2', 'hear_difn_3',
                    'hear_test_0', 'hear_test_1', 'hear_test_2', 'hear_test_3', 
                    'hear_aid_0', 'hear_aid_1', 'hear_aid_2', 'hear_aid_3',
                    'srt_r_0', 'srt_r_1', 'srt_r_2', 'srt_r_3', 'srt_l_0', 
                    'srt_l_1', 'srt_l_2', 'srt_l_3', 'date_hear_loss_a', 
                    'date_hear_loss_b', 'source_hear_loss_a', 'source_hear_loss_b', 
                    'cochl_impl_0', 'cochl_impl_1', 'cochl_impl_2', 'cochl_impl_3',
                    'hear_congenital')
hear[hear == ''] <- NA


## people who didn't know or didn't want to answer are coded as NA for all hearing-related self-report data
hear$hear_dif_0[hear$hear_dif_0 == -1 | hear$hear_dif_0 == -3] <- NA
hear$hear_dif_1[hear$hear_dif_1 == -1 | hear$hear_dif_1 == -3] <- NA
hear$hear_dif_2[hear$hear_dif_2 == -1 | hear$hear_dif_2 == -3] <- NA
hear$hear_dif_3[hear$hear_dif_3 == -1 | hear$hear_dif_3 == -3] <- NA

hear$hear_difn_0[hear$hear_difn_0 == -1 | hear$hear_difn_0 == -3] <- NA
hear$hear_difn_1[hear$hear_difn_1 == -1 | hear$hear_difn_1 == -3] <- NA
hear$hear_difn_2[hear$hear_difn_2 == -1 | hear$hear_difn_2 == -3] <- NA
hear$hear_difn_3[hear$hear_difn_3 == -1 | hear$hear_difn_3 == -3] <- NA

hear$hear_aid_0[hear$hear_aid_0 == -3] <- NA
hear$hear_aid_1[hear$hear_aid_1 == -3] <- NA
hear$hear_aid_2[hear$hear_aid_2 == -3] <- NA
hear$hear_aid_3[hear$hear_aid_3 == -3] <- NA

hear$cochl_impl_0[hear$cochl_impl_0 == -3] <- NA
hear$cochl_impl_1[hear$cochl_impl_1 == -3] <- NA
hear$cochl_impl_2[hear$cochl_impl_2 == -3] <- NA
hear$cochl_impl_3[hear$cochl_impl_3 == -3] <- NA



### medical diagnoses (hearing aids, hearing loss)
###

# This imports the medical hospital and primary-care data and searches 
# for the relevant codes in each. It then combines both sources to create 
# a single source of diagnoses, keeping just the earliest date of diagnosis.

# Requires:
# inpatient, GP, diagnosis codes

# diagnosis codes
diagnosis_codes <- read.csv('hearing_codes.csv') # EHR codes for HL and HA
# remove duplicate codes within one source (so within ICD10, or ICD9, etc.)
diagnosis_codes <- distinct(diagnosis_codes, code, source, .keep_all = TRUE)

## hearing loss and hearing aid ascertainment
# inpatient diagnoses
inpatient <- readRDS('inpatient_diagnoses.rds') # field IDs 41270, 41271, 41280, and 41281
colnames(inpatient)[colnames(inpatient) == 'diagnosis'] <- 'code'
inpatient <- filter(inpatient, code %in% diagnosis_codes$code[diagnosis_codes$source == 'icd9'] | # keep only relevant diagnoses
                      code %in% diagnosis_codes$code[diagnosis_codes$source == 'icd10']) %>%
  # replace invalid dates with NAs
  mutate(across(date, ~ replace(., . %in% c('1900-01-01', '1901-01-01', 
                                           '1902-02-02', '1903-03-03', '2037-07-07'), NA))) 
inpatient$date <- as.Date(inpatient$date, format = '%Y-%m-%d')

# GP diagnoses
gp_diagnoses <- data.table::fread('gp_clinical.txt', sep='\t', header=TRUE, quote='') %>%
  as.data.frame() %>%
  select(eid, data_provider, event_dt, read_2, read_3) %>%
  filter(!eid %in% opt_outs$id)
colnames(gp_diagnoses) <- c('id', 'data_provider', 'date_primary', 'read2', 'read3')
gp_diagnoses <- filter(gp_diagnoses, 
                       (read2 %in% diagnosis_codes$code[diagnosis_codes$source == 'read2']) | # keep only relevant diagnoses
                         (read3 %in% diagnosis_codes$code[diagnosis_codes$source == 'read3'])) %>%
  # replace invalid dates with NAs
  mutate(across(date_primary, ~ replace(., . %in% c('01/01/1900', '01/01/1901', 
                                                   '02/02/1902', '03/03/1903', 
                                                   '07/07/2037'), NA))) 
gp_diagnoses$date_primary <- as.Date(gp_diagnoses$date_primary, format = '%d/%m/%Y')
gp_diagnoses$year <- as.numeric(format(as.Date(gp_diagnoses$date_primary), '%Y'))
gp_diagnoses[gp_diagnoses == ''] <- NA

# match codes with descriptions
for (d in c('icd9', 'icd10')){
  for (diagnosis in diagnosis_codes$code[diagnosis_codes$source == d]){
    inpatient$description[inpatient$version == d & inpatient$code == diagnosis] <-
      diagnosis_codes$description[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
    inpatient$diagnosis[inpatient$version == d & inpatient$code == diagnosis] <- 
      diagnosis_codes$simple[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
    diagnosis_codes$n[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis] <- 
      length(inpatient$diagnosis[inpatient$version == d & inpatient$code == diagnosis])
  }
}
# just keep first instance per diagnosis type
inpatient <- inpatient %>%
  arrange(date) %>%
  distinct(id, diagnosis, .keep_all = TRUE)


# repeat for primary care
for (d in c('read2', 'read3')){
  for (diagnosis in diagnosis_codes$code[diagnosis_codes$source == d]){
    gp_diagnoses$description[!is.na(gp_diagnoses[[d]]) & gp_diagnoses[[d]] == diagnosis] <- 
      diagnosis_codes$description[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
    gp_diagnoses$diagnosis[!is.na(gp_diagnoses[[d]]) & gp_diagnoses[[d]] == diagnosis] <- 
      diagnosis_codes$simple[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
    diagnosis_codes$n[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis] <- 
      length(gp_diagnoses$diagnosis[!is.na(gp_diagnoses[[d]]) & gp_diagnoses[[d]] == diagnosis])
  }
}



# save the number of identified diagnoses
write.csv(diagnosis_codes, 'hearing_codes_filled.csv')

# combine inpatient and gp diagnoses and export
read2 <- gp_diagnoses %>%
  # create two separate files for read2 and read3 code from the GP record
  filter(!is.na(read2)) %>%
  arrange(date_primary) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-read3)
read3 <- gp_diagnoses %>%
  filter(!is.na(read3)) %>%
  arrange(date_primary) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-read2)
read2$diag_source <- 'read2'; read3$diag_source <- 'read3'
colnames(read2) <- c('id', 'diag_data_provider', 'diag_date', 'diag_code', 
                     'diag_year', 'diag_desc', 'diag', 'diag_source')
colnames(read3) <- c('id', 'diag_data_provider', 'diag_date', 'diag_code', 
                     'diag_year', 'diag_desc', 'diag', 'diag_source')
# re-combine read2 and read3
gp_diagnoses <- rbind(subset(read2, select = c(id, diag, diag_date, diag_year, diag_code, 
                                               diag_desc, diag_source, diag_data_provider)), 
                      subset(read3, select = c(id, diag, diag_date, diag_year, diag_code, 
                                               diag_desc, diag_source, diag_data_provider)))
inpatient$year <- as.numeric(format(as.Date(inpatient$date), '%Y'))
inpatient$data_provider <- NA
colnames(inpatient) <- c('id', 'diag_code', 'diag_date', 'diag_source', 'diag_desc', 
                         'diag', 'diag_year', 'diag_data_provider')
# bind and keep earliest record per diagnosis type
diagnoses <- rbind(gp_diagnoses,
                   subset(inpatient, select = c(id, diag, diag_date, diag_year, diag_code, 
                                                diag_desc, diag_source, diag_data_provider))) %>%
  # remove duplicate diagnoses of the same type and check for multiple types for same participant
  arrange(diag_date) %>%
  distinct(id, diag, .keep_all = TRUE)

# add column for congenital disorders
diagnosis_codes <- subset(diagnosis_codes, select = c(code, congenital))
colnames(diagnosis_codes)[colnames(diagnosis_codes) == 'code'] <- 'diag_code'
diagnoses <- merge(diagnoses, diagnosis_codes, by = 'diag_code', all.x = TRUE)

# separate into data frames of distinct diagnoses, rename columns; 
# then merge with main data frame; tag the ones without dates (they are going to be removed later)
# do this for hearing loss, hearing aid use, hearing aid use cessation, and cochlear implants
hear_loss_diag <- filter(diagnoses, diag == 'hl') %>%
  arrange(diag_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  rename(hear_loss_code = diag_code, hear_loss_diag = diag, hear_loss_date = diag_date, 
         hear_loss_year = diag_year, hear_loss_desc = diag_desc,
         hear_loss_source = diag_source, hear_loss_data_provider = diag_data_provider)
hear_loss_diag$hear_loss <- 1
hear_loss_diag$hear_loss_nodate <- 0
hear_loss_diag$hear_loss_nodate[is.na(hear_loss_diag$hear_loss_date)] <- 1

hear_aid <- diagnoses %>%
  filter(diag == 'ha') %>%
  arrange(diag_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(c(id, diag_date, diag_year, diag_code, 
           diag_desc, diag_source, diag_data_provider))
hear_aid$hear_aid <- 1
colnames(hear_aid) <- c('id',  'hear_aid_date', 'hear_aid_year', 'hear_aid_code', 'hear_aid_desc', 
                        'hear_aid_source', 'hear_aid_data_provider', 'hear_aid')
hear_aid$hear_aid_nodate <- 0
hear_aid$hear_aid_nodate[is.na(hear_aid$hear_aid_date)] <- 1

cochl_impl <- diagnoses %>%
  filter(diag == 'ci') %>%
  arrange(diag_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, diag_date, diag_year, diag_code, 
          diag_desc, diag_source, diag_data_provider)
cochl_impl$cochl_impl <- 1
colnames(cochl_impl) <- c('id',  'cochl_impl_date', 'cochl_impl_year', 
                          'cochl_impl_code', 'cochl_impl_desc', 'cochl_impl_source', 
                          'cochl_impl_data_provider', 'cochl_impl')
cochl_impl$cochl_impl_nodate <- 0
cochl_impl$cochl_impl_nodate[is.na(cochl_impl$cochl_impl_date)] <- 1

ha_cessation <- diagnoses %>%
  filter(diag == 'ha_cessation') %>%
  arrange(diag_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, diag_date, diag_year, diag_code, 
         diag_desc, diag_source, diag_data_provider)

if (nrow(ha_cessation) > 0){ # it throws error if none were found
  ha_cessation$ha_cessation <- 1
  colnames(ha_cessation) <- c('id',  'ha_cessation_date', 'ha_cessation_year', 
                              'ha_cessation_code', 'ha_cessation_desc', 
                              'ha_cessation_source', 'ha_cessation_data_provider', 
                              'ha_cessation')
  ha_cessation$ha_cessation_nodate <- 0
  ha_cessation$ha_cessation_nodate[is.na(ha_cessation$ha_cessation_date)] <- 1
} else {
  colnames(ha_cessation) <- c('id',  'ha_cessation_date', 'ha_cessation_year', 
                              'ha_cessation_code', 'ha_cessation_desc', 
                              'ha_cessation_source', 'ha_cessation_data_provider')
}
  


# combine the separate diagnoses into a single data frame and merge it with the hearing masterfile
diagnoses <- merge(hear_loss_diag, hear_aid, by = 'id', all = TRUE)
diagnoses <- merge(diagnoses, cochl_impl, by = 'id', all = TRUE)
diagnoses <- merge(diagnoses, ha_cessation, by = 'id', all = TRUE)
hear <- merge(hear, diagnoses, by = 'id', all.x = TRUE)

# for now, all participants without an explicit diagnosis are considered undiagnosed; 
# diagnoses without dates will be removed later using the 'nodate' variables created above
hear$hear_loss[is.na(hear$hear_loss)] <- 0 
hear$hear_aid[is.na(hear$hear_aid)] <- 0

if ('ha_cessation' %in% colnames(hear)){
  hear$ha_cessation[is.na(hear$ha_cessation)] <- 0
} else{
  hear$ha_cessation <- 0
}


# addition of other variables relevant for the analysis
covs <- readRDS('covariates_prepped.Rds')
hear <- merge(hear, covs, by = 'id', all.x = TRUE)
saveRDS(hear, file = 'hearing_masterfile.rds')





### Addition of new variables relevant for analysis ###
# create a variable for hearing loss according to both questions from self-report. An affirmative answer to
# default is 0; both hearing loss questions results in a hearing-loss coding (1); all other combinations of responses
# are NA as previously done in 10.1016/j.ajhg.2019.09.008
hear$hear_dif_both_0 <- rowSums(hear[ , c('hear_dif_0', 'hear_difn_0')], na.rm = TRUE)
# NA if both of them are NA
hear$hear_dif_both_0[is.na(hear$hear_dif_0) & is.na(hear$hear_difn_0)] <- NA
# NA if one of them is 1 and the other one is NA (because they would be classified as having hearing loss if the other one was 1)
hear$hear_dif_both_0[is.na(hear$hear_dif_0) & hear$hear_difn_0 == 1] <- NA 
hear$hear_dif_both_0[hear$hear_dif_0 == 1 & is.na(hear$hear_difn_0)] <- NA

hear$hear_dif_both_1 <- rowSums(hear[ , c('hear_dif_1', 'hear_difn_1')], na.rm = TRUE)
hear$hear_dif_both_1[is.na(hear$hear_dif_1) & is.na(hear$hear_difn_1)] <- NA
hear$hear_dif_both_1[is.na(hear$hear_dif_1) & hear$hear_difn_1 == 1] <- NA
hear$hear_dif_both_1[hear$hear_dif_1 == 1 & is.na(hear$hear_difn_1)] <- NA

hear$hear_dif_both_2 <- rowSums(hear[ , c('hear_dif_2', 'hear_difn_2')], na.rm = TRUE)
hear$hear_dif_both_2[is.na(hear$hear_dif_2) & is.na(hear$hear_difn_2)] <- NA
hear$hear_dif_both_2[is.na(hear$hear_dif_2) & hear$hear_difn_2 == 1] <- NA
hear$hear_dif_both_2[hear$hear_dif_2 == 1 & is.na(hear$hear_difn_2)] <- NA

hear$hear_dif_both_3 <- rowSums(hear[ , c('hear_dif_3', 'hear_difn_3')], na.rm = TRUE)
hear$hear_dif_both_3[is.na(hear$hear_dif_3) & is.na(hear$hear_difn_3)] <- NA
hear$hear_dif_both_3[is.na(hear$hear_dif_3) & hear$hear_difn_3 == 1] <- NA
hear$hear_dif_both_3[hear$hear_dif_3 == 1 & is.na(hear$hear_difn_3)] <- NA

# add the diagnoses identified by UKB 'first occurrences ID fields' but unidentified by my search of the EHR; get earliest of both UKB dates
hear$hear_loss_a <- 0; hear$hear_loss_a[!is.na(hear$date_hear_loss_a)] <- 1
hear$hear_loss_b <- 0; hear$hear_loss_b[!is.na(hear$date_hear_loss_b)] <- 1
hear$date_hear_loss_a[as.character(hear$date_hear_loss_a) %in% 
                        c('1900-01-01', '1901-01-01', '2037-07-07', 
                          '1902-02-02', '1903-03-03')] <- NA # invalid dates to NA
hear$date_hear_loss_b[as.character(hear$date_hear_loss_b) %in% 
                        c('1900-01-01', '1901-01-01', '2037-07-07', 
                          '1902-02-02', '1903-03-03')] <- NA
hear$date_hear_loss_a <- as.Date(hear$date_hear_loss_a, format = '%Y-%m-%d')
hear$date_hear_loss_b <- as.Date(hear$date_hear_loss_b, format = '%Y-%m-%d')
hear$hear_loss_a_nodate[hear$hear_loss_a == 1] <- 0 # create 'nodate' variable that indicates lack of date of diagnosis
hear$hear_loss_b_nodate[hear$hear_loss_b == 1] <- 0
hear$hear_loss_a_nodate[hear$hear_loss_a == 1 & is.na(hear$date_hear_loss_a)] <- 1
hear$hear_loss_b_nodate[hear$hear_loss_b == 1 & is.na(hear$date_hear_loss_b)] <- 1

# combine both types of hearing loss from first occurrences; determine if in those with loss at least one date is not missing
hear$hear_loss_ab_nodate[hear$hear_loss_a == 1 | hear$hear_loss_b == 1] <- 0 # if at least one of them has a date, that it's fine
hear$hear_loss_ab_nodate[(hear$hear_loss_a_nodate == 1 & is.na(hear$hear_loss_b_nodate)) | # if the one we have a diagnosis for doesn't have a date, that's not fine
                           (hear$hear_loss_b_nodate == 1 & is.na(hear$hear_loss_a_nodate)) |
                           (hear$hear_loss_a_nodate == 1 & hear$hear_loss_b_nodate == 1)] <- 1

# get earliest of both UKB dates and set that as the source of the diagnosis
hear <- hear %>%
  mutate(date_hear_loss_ab = pmin(date_hear_loss_a, date_hear_loss_b, na.rm = TRUE),
         source_hear_loss_ab = ifelse((date_hear_loss_a < date_hear_loss_b & !is.na(date_hear_loss_a)) | 
                                        is.na(date_hear_loss_b), source_hear_loss_a, source_hear_loss_b))

# now let's include the objective hearing assessment (SRT)
# create a new variable indicating SRT for 'better' ear for each visit (lower score means better hearing)
hear <- transform(hear, srt_min_0 = pmin(srt_r_0, srt_l_0, na.rm = FALSE))
hear <- transform(hear, srt_min_1 = pmin(srt_r_1, srt_l_1, na.rm = FALSE))
hear <- transform(hear, srt_min_2 = pmin(srt_r_2, srt_l_2, na.rm = FALSE))
hear <- transform(hear, srt_min_3 = pmin(srt_r_3, srt_l_3, na.rm = FALSE))

# new variable indicating hearing problems according to any of our criteria: (1) self-report (2 indicates affirmative answers to both questions, 
# 99 indicates deafness),(2) hearing loss acc. to our search of the EHR, (3) SRT, and (4) first-occurrences variables in UKB
# this is how we will define hearing loss for the RCT emulation part
hear <- hear %>%
  mutate(hear_loss_any = ifelse((!is.na(hear_dif_both_0) & hear_dif_both_0 %in% c(2, 99)) | 
                                  (!is.na(hear_dif_both_1) & hear_dif_both_1 %in% c(2, 99)) | 
                                  (!is.na(hear_dif_both_2) & hear_dif_both_2 %in% c(2, 99)) | 
                                  (!is.na(hear_dif_both_3) & hear_dif_both_3 %in% c(2, 99)) | 
                                  (!is.na(hear_loss) & hear_loss == 1) | 
                                  (!is.na(srt_min_0) & srt_min_0 > -5.5) | 
                                  (!is.na(srt_min_1) & srt_min_1 > -5.5) | 
                                  (!is.na(srt_min_2) & srt_min_2 > -5.5) | 
                                  (!is.na(srt_min_3) & srt_min_3 > -5.5) | 
                                  (!is.na(hear_loss_a) & hear_loss_a == 1) |
                                  (!is.na(hear_loss_b) & hear_loss_b == 1), 1, 0))

# assign date to hearing loss corresponding to date of assessment if hearing loss was established at that assessment (i.e., through self-report or SRT)
# assessment 0
hear$date_hear_loss_0[((hear$hear_dif_both_0 == 2 | hear$hear_dif_both_0 == 99) & 
                         !is.na(hear$hear_dif_both_0)) | (hear$srt_min_0 > -5.5  & !is.na(hear$srt_min_0))] <- 
  hear$date_0[((hear$hear_dif_both_0 == 2 | hear$hear_dif_both_0 == 99) & 
                 !is.na(hear$hear_dif_both_0)) | (hear$srt_min_0 > -5.5 & !is.na(hear$srt_min_0))]
hear$date_hear_loss_0 <- as.Date(hear$date_hear_loss_0)
# assessment 1
hear$date_hear_loss_1[((hear$hear_dif_both_1 == 2 | hear$hear_dif_both_1 == 99) & 
                         !is.na(hear$hear_dif_both_1)) | (hear$srt_min_1 > -5.5  & !is.na(hear$srt_min_1))] <- 
  hear$date_1[((hear$hear_dif_both_1 == 2 | hear$hear_dif_both_1 == 99) & 
                 !is.na(hear$hear_dif_both_1)) | (hear$srt_min_1 > -5.5 & !is.na(hear$srt_min_1))]
hear$date_hear_loss_1 <- as.Date(hear$date_hear_loss_1)
# assessment 2
hear$date_hear_loss_2[((hear$hear_dif_both_2 == 2 | hear$hear_dif_both_2 == 99) & 
                         !is.na(hear$hear_dif_both_2)) | (hear$srt_min_2 > -5.5  & !is.na(hear$srt_min_2))] <- 
  hear$date_2[((hear$hear_dif_both_2 == 2 | hear$hear_dif_both_2 == 99) & 
                 !is.na(hear$hear_dif_both_2)) | (hear$srt_min_2 > -5.5 & !is.na(hear$srt_min_2))]
hear$date_hear_loss_2 <- as.Date(hear$date_hear_loss_2)
# assessment 3 
hear$date_hear_loss_3[((hear$hear_dif_both_3 == 2 | hear$hear_dif_both_3 == 99) & 
                         !is.na(hear$hear_dif_both_3)) | (hear$srt_min_3 > -5.5  & !is.na(hear$srt_min_3))] <- 
  hear$date_3[((hear$hear_dif_both_3 == 2 | hear$hear_dif_both_3 == 99) & 
                 !is.na(hear$hear_dif_both_3)) | (hear$srt_min_3 > -5.5 & !is.na(hear$srt_min_3))]
hear$date_hear_loss_3 <- as.Date(hear$date_hear_loss_3)

# find earliest hearing loss date among all sources
hear <- transform(hear, date_hear_loss_any = 
                    pmin(date_hear_loss_0, date_hear_loss_1, date_hear_loss_2, 
                         date_hear_loss_3, hear_loss_date, date_hear_loss_ab, na.rm = TRUE)) 


# determine source of earliest hearing loss record; a single participant 
# may have several sources of hearing loss for the same date (e.g., they might 
# have indicated so via self-report and have been 'diagnosed' with the SRT); 
# we will not be saving all those sources, but will be prioritising 
# the most objective ones - EHR overwrites SRT overwrites self-report
hear$hear_loss_origin[hear$date_hear_loss_0 == hear$date_hear_loss_any | 
                        hear$date_hear_loss_1 == hear$date_hear_loss_any | # check for self-report
                        hear$date_hear_loss_2 == hear$date_hear_loss_any | 
                        hear$date_hear_loss_3 == hear$date_hear_loss_any] <- 'self_report'
hear$hear_loss_origin[(hear$date_hear_loss_0 == hear$date_hear_loss_any & 
                         hear$srt_min_0 >-5.5) | # check for SRT-based 'diagnosis'
                        (hear$date_hear_loss_1 == hear$date_hear_loss_any & 
                           hear$srt_min_1 >-5.5) |
                        (hear$date_hear_loss_2 == hear$date_hear_loss_any & 
                           hear$srt_min_2 >-5.5) |
                        (hear$date_hear_loss_3 == hear$date_hear_loss_any & 
                           hear$srt_min_3 >-5.5)] <- 'srt'
hear$hear_loss_origin[hear$hear_loss_date == hear$date_hear_loss_any & 
                        !is.na(hear$hear_loss_date)] <- # check for result of our EHR search
  hear$hear_loss_source[hear$hear_loss_date == hear$date_hear_loss_any & 
                          !is.na(hear$hear_loss_date)]
hear$hear_loss_origin[hear$date_hear_loss_ab == hear$date_hear_loss_any & 
                        !is.na(hear$date_hear_loss_ab)] <- # check for first occurrences in UKB 
  hear$source_hear_loss_ab[hear$date_hear_loss_ab == hear$date_hear_loss_any & 
                             !is.na(hear$date_hear_loss_ab)]

# harmonise coding from different sources
hear$hear_loss_origin[hear$hear_loss_origin == '30' | hear$hear_loss_origin == '31' |
                        hear$hear_loss_origin == 'read2' | 
                        hear$hear_loss_origin == 'read3'] <- 'GP'
hear$hear_loss_origin[hear$hear_loss_origin == '40' | hear$hear_loss_origin == '41' |
                        hear$hear_loss_origin == 'icd9' | 
                        hear$hear_loss_origin == 'icd10'] <- 'inpatient'

# simple one that doesn't distinguish between GP and hospital
hear$hear_loss_origin_simple <- hear$hear_loss_origin
hear$hear_loss_origin_simple[hear$hear_loss_origin %in% c('GP', 'inpatient')] <- 'EHR'


# earliest date of hearing aid (analogous to above process for hearing loss)
hear$date_hear_aid_0[hear$hear_aid_0 == 1 & !is.na(hear$hear_aid_0)] <- 
  hear$date_0[hear$hear_aid_0 == 1 & !is.na(hear$hear_aid_0)]
hear$date_hear_aid_1[hear$hear_aid_1 == 1 & !is.na(hear$hear_aid_1)] <- 
  hear$date_1[hear$hear_aid_1 == 1 & !is.na(hear$hear_aid_1)]
hear$date_hear_aid_2[hear$hear_aid_2 == 1 & !is.na(hear$hear_aid_2)] <- 
  hear$date_2[hear$hear_aid_2 == 1 & !is.na(hear$hear_aid_2)]
hear$date_hear_aid_3[hear$hear_aid_3 == 1 & !is.na(hear$hear_aid_3)] <- 
  hear$date_3[hear$hear_aid_3 == 1 & !is.na(hear$hear_aid_3)]
hear$date_hear_aid_0 <- as.Date(hear$date_hear_aid_0)
hear$date_hear_aid_1 <- as.Date(hear$date_hear_aid_1)
hear$date_hear_aid_2 <- as.Date(hear$date_hear_aid_2)
hear$date_hear_aid_3 <- as.Date(hear$date_hear_aid_3)

# earliest record of hearing aid
hear <- transform(hear, date_hear_aid_any = pmin(date_hear_aid_0, date_hear_aid_1, 
                                                 date_hear_aid_2, date_hear_aid_3, 
                                                 hear_aid_date, na.rm = TRUE))
hear$hear_aid_any <- 0
hear$hear_aid_any[!is.na(hear$date_hear_aid_any)] <- 1


# determine source of earliest hearing aid record
hear$hear_aid_origin[hear$date_hear_aid_0 == hear$date_hear_aid_any | 
                       hear$date_hear_aid_1 == hear$date_hear_aid_any | # check for self report
                       hear$date_hear_aid_2 == hear$date_hear_aid_any | 
                       hear$date_hear_aid_3 == hear$date_hear_aid_any] <- 'self_report'
hear$hear_aid_origin[hear$date_hear_aid_any == hear$hear_aid_date & 
                       !is.na(hear$date_hear_aid_any) & # check for result of our EHR search
                       !is.na(hear$hear_aid_date)] <- 'EHR'


# same for cochlear implant
hear$date_cochl_impl_0[hear$cochl_impl_0 == 1 & !is.na(hear$cochl_impl_0)] <-
  hear$date_0[hear$cochl_impl_0 == 1 & !is.na(hear$cochl_impl_0)]
hear$date_cochl_impl_1[hear$cochl_impl_1 == 1 & !is.na(hear$cochl_impl_1)] <- 
  hear$date_1[hear$cochl_impl_1 == 1 & !is.na(hear$cochl_impl_1)]
hear$date_cochl_impl_2[hear$cochl_impl_2 == 1 & !is.na(hear$cochl_impl_2)] <- 
  hear$date_2[hear$cochl_impl_2 == 1 & !is.na(hear$cochl_impl_2)]
hear$date_cochl_impl_3[hear$cochl_impl_3 == 1 & !is.na(hear$cochl_impl_3)] <- 
  hear$date_3[hear$cochl_impl_3 == 1 & !is.na(hear$cochl_impl_3)]
hear$date_cochl_impl_0 <- as.Date(hear$date_cochl_impl_0)
hear$date_cochl_impl_1 <- as.Date(hear$date_cochl_impl_1)
hear$date_cochl_impl_2 <- as.Date(hear$date_cochl_impl_2)
hear$date_cochl_impl_3 <- as.Date(hear$date_cochl_impl_3)

hear <- transform(hear, date_cochl_impl_any = pmin(date_cochl_impl_0, 
                                                   date_cochl_impl_1, 
                                                   date_cochl_impl_2, 
                                                   date_cochl_impl_3, 
                                                   cochl_impl_date, na.rm = TRUE))
hear$cochl_impl_any <- 0
hear$cochl_impl_any[!is.na(hear$date_cochl_impl_any)] <- 1


saveRDS(hear, file = 'hearing_masterfile_prepped.rds')
rm(list = ls()); gc()