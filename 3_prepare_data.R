library(tidyverse)
library(zoo)

data_all <- readRDS('W:/Raw data/UKB/main_vars_mm.Rds')
hear <- data_all %>%
  select(eid, starts_with(c('X2247.', 'X2257.', 'X3393.', 'X4792.', 
                            'X20019.', 'X20021.', 'X132460.', 
                            'X131258.', 'X131259.', 'X131260.', 'X131261.',
                            'X53.')))
hear$eid <- as.character(hear$eid)

# change colnames and code emtpy strings as NAs
colnames(hear) <- c('id', 'hear_dif_0', 'hear_dif_1', 'hear_dif_2', 'hear_dif_3', 
                    'hear_difn_0', 'hear_difn_1', 'hear_difn_2', 'hear_difn_3',
                    'hear_aid_0', 'hear_aid_1', 'hear_aid_2', 'hear_aid_3',
                    'cochl_impl_0', 'cochl_impl_1', 'cochl_impl_2', 'cochl_impl_3',
                    'srt_r_0', 'srt_r_1', 'srt_r_2', 'srt_r_3', 'srt_l_0', 
                    'srt_l_1', 'srt_l_2', 'srt_l_3', 'hear_congenital',
                    'date_hear_loss_a', 'source_hear_loss_a', 
                    'date_hear_loss_b', 'source_hear_loss_b',
                    'date_0', 'date_1', 'date_2', 'date_3')
hear[hear == ''] <- NA





# people who didn't know or didn't want to answer are coded as NA for all hearing-related self-report data
hear$hear_dif_0[hear$hear_dif_0 == -1 | hear$hear_dif_0 == -3] <- NA
hear$hear_dif_1[hear$hear_dif_1 == -1 | hear$hear_dif_1 == -3] <- NA
hear$hear_dif_2[hear$hear_dif_2 == -1 | hear$hear_dif_2 == -3] <- NA
hear$hear_dif_3[hear$hear_dif_3 == -1 | hear$hear_dif_3 == -3] <- NA

hear$hear_difn_0[hear$hear_difn_0 == -1 | hear$hear_difn_0 == -3] <- NA
hear$hear_difn_1[hear$hear_difn_1 == -1 | hear$hear_difn_1 == -3] <- NA
hear$hear_difn_2[hear$hear_difn_2 == -1 | hear$hear_difn_2 == -3] <- NA
hear$hear_difn_3[hear$hear_difn_3 == -1 | hear$hear_difn_3 == -3] <- NA

hear$hear_aid_0[hear$hear_aid_0 == -1 | hear$hear_aid_0 == -3] <- NA
hear$hear_aid_1[hear$hear_aid_1 == -1 | hear$hear_aid_1 == -3] <- NA
hear$hear_aid_2[hear$hear_aid_2 == -1 | hear$hear_aid_2 == -3] <- NA
hear$hear_aid_3[hear$hear_aid_3 == -1 | hear$hear_aid_3 == -3] <- NA

hear$cochl_impl_0[hear$cochl_impl_0 == -1 | hear$cochl_impl_0 == -3] <- NA
hear$cochl_impl_1[hear$cochl_impl_1 == -1 | hear$cochl_impl_1 == -3] <- NA
hear$cochl_impl_2[hear$cochl_impl_2 == -1 | hear$cochl_impl_2 == -3] <- NA
hear$cochl_impl_3[hear$cochl_impl_3 == -1 | hear$cochl_impl_3 == -3] <- NA

# This imports the medical hospital and primary-care data and searches 
# for the relevant codes in each. It then combines both sources to create 
# a single source of diagnoses, keeping just the earliest date of diagnosis.

# diagnosis codes
diagnosis_codes <- read.csv('hearing_codes.csv') # EHR codes for HL and HA
# remove duplicate codes within one source (so within ICD10, or ICD9, etc.)
diagnosis_codes <- distinct(diagnosis_codes, code, source, .keep_all = TRUE)

# hearing loss and hearing aid ascertainment
# inpatient diagnoses
inpatient <- readRDS('output_files/inpatient.Rds') # field IDs 41270, 41271, 41280, and 41281
colnames(inpatient)[colnames(inpatient) == 'diagnosis'] <- 'code'
# keep only relevant diagnoses
inpatient <- filter(inpatient, code %in% diagnosis_codes$code[diagnosis_codes$source == 'icd9'] | 
                      code %in% diagnosis_codes$code[diagnosis_codes$source == 'icd10']) %>%
  # replace invalid dates with NAs
  mutate(across(date, ~replace(., . %in% c('1900-01-01', '1901-01-01', 
                                           '1902-02-02', '1903-03-03', '2037-07-07'), NA))) 
inpatient$date <- as.Date(inpatient$date, format = '%Y-%m-%d')

# GP diagnoses
gp_diagnoses <- data.table::fread('gp_clinical.txt', sep='\t', header=TRUE, quote='') # field ID 42040

gp_diagnoses <- as.data.frame(gp_diagnoses)
gp_diagnoses <- subset(gp_diagnoses, 
                       select = c(eid, data_provider, event_dt, read_2, read_3))
colnames(gp_diagnoses) <- c('id', 'data_provider', 'date_primary', 'read2', 'read3')
gp_diagnoses <- filter(gp_diagnoses, 
                       (read2 %in% diagnosis_codes$code[diagnosis_codes$source == 'read2']) | 
                         (read3 %in% diagnosis_codes$code[diagnosis_codes$source == 'read3'])) %>%
  # replace invalid dates with NAs
  mutate(across(date_primary, ~replace(., . %in% c('1900-01-01', '1901-01-01', 
                                                   '1902-02-02', '1903-03-03', 
                                                   '2037-07-07'), NA))) 
gp_diagnoses$date_primary <- as.Date(gp_diagnoses$date_primary, format = '%Y-%m-%d')
gp_diagnoses$year <- as.numeric(format(as.Date(gp_diagnoses$date_primary), '%Y'))
gp_diagnoses[gp_diagnoses == ''] <- NA

# remove duplicate codes and match codes with descriptions
inpatient <- inpatient %>% arrange(date)
inpatient <- distinct(inpatient, id, code, version, .keep_all = TRUE)
for (d in c('icd9', 'icd10')){
  for (diagnosis in diagnosis_codes$code[diagnosis_codes$source == d]){
    inpatient$description[inpatient$version == d & inpatient$code == diagnosis] <-
      diagnosis_codes$description[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
    inpatient$diagnosis[inpatient$version == d & inpatient$code == diagnosis] <- 
      diagnosis_codes$simple[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
  }
}

# repeat for primary care
gp_diagnoses <- gp_diagnoses %>% arrange(date_primary)
gp_diagnoses <- distinct(gp_diagnoses, id, read2, .keep_all = TRUE)
gp_diagnoses <- distinct(gp_diagnoses, id, read3, .keep_all = TRUE)
for (d in c('read2', 'read3')){
  for (diagnosis in diagnosis_codes$code[diagnosis_codes$source == d]){
    gp_diagnoses$description[!is.na(gp_diagnoses[[d]]) & gp_diagnoses[[d]] == diagnosis] <- 
      diagnosis_codes$description[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
    gp_diagnoses$diagnosis[!is.na(gp_diagnoses[[d]]) & gp_diagnoses[[d]] == diagnosis] <- 
      diagnosis_codes$simple[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
  }
}

# combine inpatient and gp diagnoses and export
# create two separate files for read2 and read3 code from the GP record
read2 <- filter(gp_diagnoses, !is.na(read2)) 
read3 <- filter(gp_diagnoses, !is.na(read3))
read2 <- subset(read2, select = -c(read3))
read3 <- subset(read3, select = -c(read2))
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
diagnoses <- rbind(gp_diagnoses,
                   subset(inpatient, select = c(id, diag, diag_date, diag_year, diag_code, 
                                                diag_desc, diag_source, diag_data_provider)))

# add flag for congenital HL
congenital <- diagnosis_codes %>%
  filter(!is.na(congenital) & congenital == 1) %>%
  rename(diag_source = source, diag_code = code) %>%
  select(diag_code, diag_source)
congenital$congenital <- 1
diagnoses <- merge(diagnoses, congenital, by = c('diag_code', 'diag_source'), all.x = TRUE)
# remove duplicate diagnoses of the same type and check for multiple types for same participant
diagnoses <- arrange(diagnoses, diag_date)
diagnoses <- distinct(diagnoses, id, diag, .keep_all = TRUE)

# separate into data frames of distinct diagnoses, remove duplicates, and bind again; 
# then merge with main data frame; tag the ones without dates (they are going to be removed later)
# do this for hearing loss, hearing aid use, hearing aid use cessation, and cochlear implants
diagnoses_hl <- diagnoses %>% filter(diag == 'hl') %>%
  distinct(id, .keep_all = TRUE) %>%
  rename(hear_loss_code = diag_code, hear_loss_diag = diag, hear_loss_date = diag_date, 
         hear_loss_year = diag_year, hear_loss_desc = diag_desc,
         hear_loss_source = diag_source, hear_loss_data_provider = diag_data_provider)
diagnoses_hl$hear_loss <- 1
diagnoses_hl$hear_loss_nodate <- 0
diagnoses_hl$hear_loss_nodate[is.na(diagnoses_hl$hear_loss_date)] <- 1
diagnoses_hl$congenital[is.na(diagnoses_hl$congenital)] <- 0

diagnoses_ha <- diagnoses %>% filter(diag == 'ha') %>%
  distinct(id, .keep_all = TRUE) %>%
  rename(hear_aid_code = diag_code, hear_aid_diag = diag, hear_aid_date = diag_date, 
         hear_aid_year = diag_year, hear_aid_desc = diag_desc,
         hear_aid_source = diag_source, hear_aid_data_provider = diag_data_provider)
diagnoses_ha$hear_aid <- 1
diagnoses_ha$hear_aid_nodate <- 0
diagnoses_ha$hear_aid_nodate[is.na(diagnoses_ha$hear_aid_date)] <- 1
diagnoses_ha$congenital <- NULL

diagnoses_ci <- diagnoses %>% filter(diag == 'ci') %>%
  distinct(id, .keep_all = TRUE) %>%
  rename(cochl_impl_code = diag_code, cochl_impl_diag = diag, cochl_impl_date = diag_date, 
         cochl_impl_year = diag_year, cochl_impl_desc = diag_desc,
         cochl_impl_source = diag_source, cochl_impl_data_provider = diag_data_provider)
diagnoses_ci$cochl_impl <- 1
diagnoses_ci$cochl_impl_nodate <- 0
diagnoses_ci$cochl_impl_nodate[is.na(diagnoses_ci$cochl_impl_date)] <- 1
diagnoses_ci$congenital <- NULL

# merge with the hearing masterfile
hear$hear_congenital[!is.na(hear$hear_congenital)] <- 1
hear <- merge(hear, diagnoses_hl, by = 'id', all.x = TRUE)
hear <- merge(hear, diagnoses_ha, by = 'id', all.x = TRUE)
hear <- merge(hear, diagnoses_ci, by = 'id', all.x = TRUE)

# for now, all participants without an explicit diagnosis are considered undiagnosed; 
# diagnoses without dates will be removed later using the 'nodate' variables created above
hear$hear_loss[is.na(hear$hear_loss)] <- 0 
hear$hear_aid[is.na(hear$hear_aid)] <- 0 
hear$cochl_impl[is.na(hear$cochl_impl)] <- 0 

hear[, c('hear_dif_0', 'hear_dif_1', 'hear_dif_2', 'hear_dif_3',
         'hear_difn_0', 'hear_difn_1', 'hear_difn_2', 'hear_difn_3')] <-
  sapply(hear[, c('hear_dif_0', 'hear_dif_1', 'hear_dif_2', 'hear_dif_3',
                  'hear_difn_0', 'hear_difn_1', 'hear_difn_2', 'hear_difn_3')],
         as.integer)
hear$hear_dif_both_0 <- rowSums(hear[ , c('hear_dif_0', 'hear_difn_0')], na.rm = TRUE)
# NA if both of them are NA
hear$hear_dif_both_0[is.na(hear$hear_dif_0) & is.na(hear$hear_difn_0)] <- NA
# NA if one of them is 1 and the other one is NA 
# (because they would be classified as having hearing loss if the other one was 1)
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

# add the diagnoses identified by UKB 'first occurrences ID fields' 
# but unidentified by my search of the EHR; get earliest of both UKB dates
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
# create 'nodate' variable that indicates lack of date of diagnosis
hear$hear_loss_a_nodate[hear$hear_loss_a == 1] <- 0 
hear$hear_loss_b_nodate[hear$hear_loss_b == 1] <- 0
hear$hear_loss_a_nodate[hear$hear_loss_a == 1 & is.na(hear$date_hear_loss_a)] <- 1
hear$hear_loss_b_nodate[hear$hear_loss_b == 1 & is.na(hear$date_hear_loss_b)] <- 1

# combine both types of hearing loss from first occurrences; 
# determine if in those with loss at least one date is not missing
# if at least one of them has a date, that it's fine
hear$hear_loss_ab_nodate[hear$hear_loss_a == 1 | hear$hear_loss_b == 1] <- 0 
# if the one we have a diagnosis for doesn't have a date, that's not fine
hear$hear_loss_ab_nodate[(hear$hear_loss_a_nodate == 1 & is.na(hear$hear_loss_b_nodate)) | 
                           (hear$hear_loss_b_nodate == 1 & is.na(hear$hear_loss_a_nodate)) |
                           (hear$hear_loss_a_nodate == 1 & hear$hear_loss_b_nodate == 1)] <- 1

# get earliest of both UKB dates and set that as the source of the diagnosis
hear <- hear %>%
  mutate(date_hear_loss_ab = pmin(date_hear_loss_a, date_hear_loss_b, na.rm = TRUE),
         source_hear_loss_ab = ifelse((date_hear_loss_a < date_hear_loss_b & 
                                         !is.na(date_hear_loss_a)) | 
                                        is.na(date_hear_loss_b), 
                                      source_hear_loss_a, source_hear_loss_b))

# now let's include the objective hearing assessment (SRT)
# create a new variable indicating SRT for 'better' ear for each visit (lower score means better hearing)
hear <- transform(hear, srt_min_0 = pmin(srt_r_0, srt_l_0, na.rm = TRUE))
hear <- transform(hear, srt_min_1 = pmin(srt_r_1, srt_l_1, na.rm = TRUE))
hear <- transform(hear, srt_min_2 = pmin(srt_r_2, srt_l_2, na.rm = TRUE))
hear <- transform(hear, srt_min_3 = pmin(srt_r_3, srt_l_3, na.rm = TRUE))

# new variable indicating hearing problems according to any of our criteria: 
# (1) self-report (2 indicates affirmative answers to both questions, 
# 99 indicates deafness),(2) hearing loss acc. to our search of the EHR, 
# (3) SRT, and (4) first-occurrences variables in UKB
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

# analogous with hearing aids and cochlear implants
hear <- hear %>% 
  mutate(hear_aid_any = ifelse(((hear_aid == 1) |
                                  (!is.na(hear_aid_0) & hear_aid_0 == 1) |
                                  (!is.na(hear_aid_1) & hear_aid_1 == 1) |
                                  (!is.na(hear_aid_2) & hear_aid_2 == 1) |
                                  (!is.na(hear_aid_3) & hear_aid_3 == 1)), 1, 0))

hear <- hear %>% 
  mutate(cochl_impl_any = ifelse(((cochl_impl == 1) |
                                    (!is.na(cochl_impl_0) & cochl_impl_0 == 1) |
                                    (!is.na(cochl_impl_1) & cochl_impl_1 == 1) |
                                    (!is.na(cochl_impl_2) & cochl_impl_2 == 1) |
                                    (!is.na(cochl_impl_3) & cochl_impl_3 == 1)), 1, 0))

# assign date to hearing loss corresponding to date of assessment if hearing loss 
# was established at that assessment (i.e., through self-report or SRT)
# assessment 0
hear$date_hear_loss_0[((hear$hear_dif_both_0 == 2 | hear$hear_dif_both_0 == 99) & 
                         !is.na(hear$hear_dif_both_0)) | (hear$srt_min_0 > -5.5  
                                                          & !is.na(hear$srt_min_0))] <- 
  hear$date_0[((hear$hear_dif_both_0 == 2 | hear$hear_dif_both_0 == 99) & 
                 !is.na(hear$hear_dif_both_0)) | (hear$srt_min_0 > -5.5 
                                                  & !is.na(hear$srt_min_0))]
hear$date_hear_loss_0 <- as.Date(hear$date_hear_loss_0)

hear$date_hear_aid_0[(hear$hear_aid_0 == 1 & !is.na(hear$hear_aid_0))] <- 
  hear$date_0[(hear$hear_aid_0 == 1 & !is.na(hear$hear_aid_0))]
hear$date_hear_aid_0 <- as.Date(hear$date_hear_aid_0)

hear$date_cochl_impl_0[(hear$cochl_impl_0 == 1 & !is.na(hear$cochl_impl_0))] <- 
  hear$date_0[(hear$cochl_impl_0 == 1 & !is.na(hear$cochl_impl_0))]
hear$date_cochl_impl_0 <- as.Date(hear$date_cochl_impl_0)

# assessment 1
hear$date_hear_loss_1[((hear$hear_dif_both_1 == 2 | hear$hear_dif_both_1 == 99) & 
                         !is.na(hear$hear_dif_both_1)) | (hear$srt_min_1 > -5.5  
                                                          & !is.na(hear$srt_min_1))] <- 
  hear$date_1[((hear$hear_dif_both_1 == 2 | hear$hear_dif_both_1 == 99) & 
                 !is.na(hear$hear_dif_both_1)) | (hear$srt_min_1 > -5.5 & !is.na(hear$srt_min_1))]
hear$date_hear_loss_1 <- as.Date(hear$date_hear_loss_1)

hear$date_hear_aid_1[(hear$hear_aid_1 == 1 & !is.na(hear$hear_aid_1))] <- 
  hear$date_0[(hear$hear_aid_1 == 1 & !is.na(hear$hear_aid_1))]
hear$date_hear_aid_1 <- as.Date(hear$date_hear_aid_1)

hear$date_cochl_impl_1[(hear$cochl_impl_1 == 1 & !is.na(hear$cochl_impl_1))] <- 
  hear$date_1[(hear$cochl_impl_1 == 1 & !is.na(hear$cochl_impl_1))]
hear$date_cochl_impl_1 <- as.Date(hear$date_cochl_impl_1)

# assessment 2
hear$date_hear_loss_2[((hear$hear_dif_both_2 == 2 | hear$hear_dif_both_2 == 99) & 
                         !is.na(hear$hear_dif_both_2)) | (hear$srt_min_2 > -5.5  
                                                          & !is.na(hear$srt_min_2))] <- 
  hear$date_2[((hear$hear_dif_both_2 == 2 | hear$hear_dif_both_2 == 99) & 
                 !is.na(hear$hear_dif_both_2)) | (hear$srt_min_2 > -5.5 & !is.na(hear$srt_min_2))]
hear$date_hear_loss_2 <- as.Date(hear$date_hear_loss_2)

hear$date_hear_aid_2[(hear$hear_aid_2 == 1 & !is.na(hear$hear_aid_2))] <- 
  hear$date_0[(hear$hear_aid_2 == 1 & !is.na(hear$hear_aid_2))]
hear$date_hear_aid_2 <- as.Date(hear$date_hear_aid_2)

hear$date_cochl_impl_2[(hear$cochl_impl_2 == 1 & !is.na(hear$cochl_impl_2))] <- 
  hear$date_2[(hear$cochl_impl_2 == 1 & !is.na(hear$cochl_impl_2))]
hear$date_cochl_impl_2 <- as.Date(hear$date_cochl_impl_2)

# assessment 3 
hear$date_hear_loss_3[((hear$hear_dif_both_3 == 2 | hear$hear_dif_both_3 == 99) & 
                         !is.na(hear$hear_dif_both_3)) | (hear$srt_min_3 > -5.5  
                                                          & !is.na(hear$srt_min_3))] <- 
  hear$date_3[((hear$hear_dif_both_3 == 2 | hear$hear_dif_both_3 == 99) & 
                 !is.na(hear$hear_dif_both_3)) | (hear$srt_min_3 > -5.5 & !is.na(hear$srt_min_3))]
hear$date_hear_loss_3 <- as.Date(hear$date_hear_loss_3)

hear$date_hear_aid_3[(hear$hear_aid_3 == 1 & !is.na(hear$hear_aid_3))] <- 
  hear$date_0[(hear$hear_aid_3 == 1 & !is.na(hear$hear_aid_3))]
hear$date_hear_aid_3 <- as.Date(hear$date_hear_aid_3)

hear$date_cochl_impl_3[(hear$cochl_impl_3 == 1 & !is.na(hear$cochl_impl_3))] <- 
  hear$date_3[(hear$cochl_impl_3 == 1 & !is.na(hear$cochl_impl_3))]
hear$date_cochl_impl_3 <- as.Date(hear$date_cochl_impl_3)


# find earliest hearing loss, hearing aid and cochl. implant date among all sources
hear <- hear %>%
  transform(date_hear_loss_any = 
              pmin(date_hear_loss_0, date_hear_loss_1, date_hear_loss_2, 
                   date_hear_loss_3, hear_loss_date, date_hear_loss_ab, na.rm = TRUE)) %>%
  transform(date_hear_aid_any = 
              pmin(date_hear_aid_0, date_hear_aid_1, date_hear_aid_2, 
                   date_hear_aid_3, hear_aid_date, na.rm = TRUE)) %>%
  transform(date_cochl_impl_any = 
              pmin(date_cochl_impl_0, date_cochl_impl_1, date_cochl_impl_2, 
                   date_cochl_impl_3, cochl_impl_date, na.rm = TRUE))

rm(diagnoses, diagnosis_codes, gp_diagnoses, read2, read3, inpatient)
gc()

# merge with covariates
covariates <- readRDS('covariates_prepped.Rds')
hear <- merge(hear, covariates, by = 'id', all = TRUE)

saveRDS(hear, 'hearing_masterfile_prepped.rds')