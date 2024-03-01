library(tidyverse)
library(zoo)
library(purrr)
library(lavaan)
library(semPlot)


source('0_helper_functions.R')


invalid_dates <- as.Date(c('01/01/1900', '01/01/1901', '02/02/1902', '03/03/1903', '07/07/2037'), format = '%d/%m/%Y')

# main dataset
data_all <- readRDS('main_vars.Rds')

## dementia
dementia <- data_all %>% select(c(eid, starts_with(c('X42018.'))))
colnames(dementia) <- c('id', 'dementia_date')
dementia$dementia_date <- as.Date(dementia$dementia_date, format <- '%Y-%m-%d')
dementia$dementia <- 0; dementia$dementia[!is.na(dementia$dementia_date)] <- 1
dementia$dementia[dementia$dementia_date == as.Date('1900-01-01', format = '%Y-%m-%d')] <- NA
dementia$dementia_date[dementia$dementia_date == as.Date('1900-01-01', format = '%Y-%m-%d')] <- NA



## assessment dates, age at assessments and sex
dems <- data_all %>% select(c(eid, starts_with(c('X31.', 'X53.', 'X34.', 'X52.'))))
dems <- dems %>%
  rename(sex = X31.0.0, date_0 = X53.0.0, date_1 = X53.1.0, date_2 = X53.2.0, date_3 = X53.3.0,
         birth_year = X34.0.0, birth_month = X52.0.0)
dems$birth_year <- as.character(dems$birth_year); dems$birth_month <- as.character(dems$birth_month)
dems$birth_date <- as.Date(paste0('01/', dems$birth_month, '/' ,dems$birth_year), format = '%d/%m/%Y')
dems[, c('date_0', 'date_1', 'date_2', 'date_3')] <- lapply(dems[, c('date_0', 'date_1', 'date_2', 'date_3')], as.Date, format = '%Y-%m-%d')
dems$age_0 <- as.numeric(difftime(dems$date_0, dems$birth_date, units='days'))/365.25
dems$age_1 <- as.numeric(difftime(dems$date_1, dems$birth_date, units='days'))/365.25
dems$age_2 <- as.numeric(difftime(dems$date_2, dems$birth_date, units='days'))/365.25
dems$age_3 <- as.numeric(difftime(dems$date_3, dems$birth_date, units='days'))/365.25
dems <- dems %>%
  select(-c('birth_year', 'birth_month')) %>%
  rename(id = eid)



## education
# create subsets of the education frame for each assessment; change -7 to NA, and set to 1 if graduate degree present
education <- data_all %>% select(eid, starts_with('X6138.')) %>% rename(id = eid)

education[education == -3] <- NA

education_0 <- education %>%
  select(id, starts_with('X6138.0')) %>% filter(rowSums(is.na(select(., -id))) != ncol(.) - 1) %>%
  mutate(education_0 = as.integer(rowSums(select(., starts_with('X6138')) == 1, na.rm = TRUE) > 0))

education_1 <- education %>%
  select(id, starts_with('X6138.1')) %>% filter(rowSums(is.na(select(., -id))) != ncol(.) - 1) %>%
  mutate(education_1 = as.integer(rowSums(select(., starts_with('X6138')) == 1, na.rm = TRUE) > 0))

education_2 <- education %>%
  select(id, starts_with('X6138.2')) %>% filter(rowSums(is.na(select(., -id))) != ncol(.) - 1) %>%
  mutate(education_2 = as.integer(rowSums(select(., starts_with('X6138')) == 1, na.rm = TRUE) > 0))

education_3 <- education %>%
  select(id, starts_with('X6138.3')) %>% filter(rowSums(is.na(select(., -id))) != ncol(.) - 1) %>%
  mutate(education_3 = as.integer(rowSums(select(., starts_with('X6138')) == 1, na.rm = TRUE) > 0))

education <- merge(education_0, education_1, by = 'id', all = TRUE)
education <- merge(education, education_2, by = 'id', all = TRUE)
education <- merge(education, education_3, by = 'id', all = TRUE) %>%
  select(id, education_0, education_1, education_2, education_3)
rm(education_0, education_1, education_2, education_3)


## cognition
cognition <- data_all %>% select(c(eid, starts_with(c('X20016.', 'X20018.', 'X20023.', 'X399.', 'X6351.', 'X6373.', 'X23324.', 'X4282.', 'X21004.'))))
cognition <- subset(cognition, select=-c(X399.0.3, X399.1.3, X399.2.3, X399.3.3, X399.0.1, X399.1.1, X399.2.1, X399.3.1)) # use only second round
cognition <- subset(cognition, select=-c(X4282.1.0)) # numerical memory was not tested then, and the columns has all NAs
colnames(cognition) <- c('id', 'VNR_0', 'VNR_1', 'VNR_2', 'VNR_3', 'ProsMem_0', 'ProsMem_1', 'ProsMem_2', 'ProsMem_3', 'RT_0', 'RT_1', 'RT_2', 'RT_3',
                         'VisMem_0', 'VisMem_1', 'VisMem_2', 'VisMem_3',
                         'TMTb_2', 'TMTb_3', 'MR_2', 'MR_3', 'DSS_2', 'DSS_3', 'NM_0', 'NM_2', 'NM_3', 'TR_2', 'TR_3')
# remove outliers 
cognition[, c('VNR_0', 'VNR_1', 'VNR_2', 'VNR_3', 'RT_0', 'RT_1', 'RT_2', 'RT_3', 'VisMem_0', 'VisMem_1', 'VisMem_2', 'VisMem_3', 
               'ProsMem_0', 'ProsMem_1', 'ProsMem_2', 'ProsMem_3', 'DSS_2', 'DSS_3', 'MR_2', 'MR_3', 
               'TMTb_2', 'TMTb_3', 'NM_0', 'NM_2', 'NM_3', 'TR_2', 'TR_3')] <- 
  lapply(cognition[, c('VNR_0', 'VNR_1', 'VNR_2', 'VNR_3', 'RT_0', 'RT_1', 'RT_2', 'RT_3', 'VisMem_0', 'VisMem_1', 'VisMem_2', 'VisMem_3', 
                        'ProsMem_0', 'ProsMem_1', 'ProsMem_2', 'ProsMem_3', 'DSS_2', 'DSS_3', 'MR_2', 'MR_3', 
                        'TMTb_2', 'TMTb_3', 'NM_0', 'NM_2', 'NM_3', 'TR_2', 'TR_3')], outliers, var_metric=4, method='SD')

# Calculate the latent G

# assessment 0
cognition$RT_0 = log(cognition$RT_0)
cognition$VisMem_0 = log(cognition$VisMem_0+1)
cognition$ProsMem_0[which(cognition$ProsMem_0 != 1)] = 0
cognition$ProsMem_0[which(cognition$ProsMem_0 == 1)] = 1
model_0 <- '
            # Structural relation
            g =~ VNR_0 + RT_0 + VisMem_0 + ProsMem_0 + NM_0
            
'
fit_0 <- sem(model_0, data=cognition, missing='fiml.x')
cognition$g_0 <- as.vector(predict(fit_0, cognition)) # extract the g-values

# assessment 1
cognition$RT_1 = log(cognition$RT_1)
cognition$VisMem_1 = log(cognition$VisMem_1+1)
cognition$ProsMem_1[which(cognition$ProsMem_1 != 1)] = 0
cognition$ProsMem_1[which(cognition$ProsMem_1 == 1)] = 1
model_1 <- '
            # Structural relation
            g =~ VNR_1 + RT_1 + VisMem_1 + ProsMem_1
            
'
fit_1 <- sem(model_1, data=cognition, missing='fiml.x')
cognition$g_1 <- as.vector(predict(fit_1, cognition))

# assessment 2
cognition$RT_2 = log(cognition$RT_2)
cognition$VisMem_2 = log(cognition$VisMem_2+1)
cognition$ProsMem_2[which(cognition$ProsMem_2 != 1)] = 0
cognition$ProsMem_2[which(cognition$ProsMem_2 == 1)] = 1
# remove those that didn't complete trails B
cognition$TMTb_2[cognition$TMTb_2==0] <- NA   
cognition$TMTb_2 = log(cognition$TMTb_2+10)
model_2<- '
      G =~ MR_2 + DSS_2 + VNR_2 + TMTb_2 + RT_2 + VisMem_2 + ProsMem_2 + NM_2 + TR_2
      MR_2 ~~ VNR_2               # include residual correlations here - these two tests are more similar than they are to other tests in the battery, also below.
      RT_2 ~~ DSS_2
      '
fit_2 <- sem(model_2, data=cognition, missing='fiml.x')
cognition$g_2 <- as.vector(predict(fit_2, cognition))

# assessment 3
cognition$RT_3 = log(cognition$RT_3)
cognition$VisMem_3 = log(cognition$VisMem_3+1)
cognition$ProsMem_3[which(cognition$ProsMem_3 != 1)] = 0
cognition$ProsMem_3[which(cognition$ProsMem_3 == 1)] = 1

# remove those that didn't complete trails B
cognition$TMTb_3[cognition$TMTb_3==0] <- NA   
cognition$TMTb_3 = log(cognition$TMTb_3+10)

# SEM
model_3<- '
      G =~ MR_3 + DSS_3 + VNR_3 + TMTb_3 + RT_3 + VisMem_3 + ProsMem_3 + NM_3 + TR_3
      MR_3 ~~ VNR_3               # include residual correlations here - these two tests are more similar than they are to other tests in the battery, also below.
      RT_3 ~~ DSS_3
      '
fit_3 <- sem(model_3, data=cognition, missing='fiml.x')
cognition$g_3 <- as.vector(predict(fit_3, cognition))
cognition <- cognition %>% 
  select(id, g_0, g_1, g_2, g_3)
rm(fit_0, fit_1, fit_2, fit_3)


# social isolation, loneliness
social <- data_all %>% 
  select(c(eid, starts_with(c('X709.', 'X1031.', 'X6160.')))) %>%
  rename(id = eid)
# assessment 0
social$X709.0.0[social$X709.0.0 == -3 | social$X709.0.0 == -1] <- NA
social$X709.0.0[social$X709.0.0 > 1] <- 0
social$X1031.0.0[social$X1031.0.0 == -3 | social$X1031.0.0 == -1] <- NA
social$X1031.0.0[social$X1031.0.0 <= 4] <- 0
social$X1031.0.0[social$X1031.0.0 > 4] <- 1
social$X6160.0.0[social$X6160.0.0 == -3] <- NA
social$X6160.0.0[social$X6160.0.0 > 0] <- 0
social$X6160.0.0[social$X6160.0.0 == -7] <- 1
# assessment 1
social$X709.1.0[social$X709.1.0 == -3 | social$X709.1.0 == -1] <- NA
social$X709.1.0[social$X709.1.0 > 1] <- 0
social$X1031.1.0[social$X1031.1.0 == -3 | social$X1031.1.0 == -1] <- NA
social$X1031.1.0[social$X1031.1.0 <= 4] <- 0
social$X1031.1.0[social$X1031.1.0 > 4] <- 1
social$X6160.1.0[social$X6160.1.0 == -3] <- NA
social$X6160.1.0[social$X6160.1.0 > 0] <- 0
social$X6160.1.0[social$X6160.1.0 == -7] <- 1
# assessment 2
social$X709.2.0[social$X709.2.0 == -3 | social$X709.2.0 == -1] <- NA
social$X709.2.0[social$X709.2.0 > 1] <- 0
social$X1031.2.0[social$X1031.2.0 == -3 | social$X1031.2.0 == -1] <- NA
social$X1031.2.0[social$X1031.2.0 <= 4] <- 0
social$X1031.2.0[social$X1031.2.0 > 4] <- 1
social$X6160.2.0[social$X6160.2.0 == -3] <- NA
social$X6160.2.0[social$X6160.2.0 > 0] <- 0
social$X6160.2.0[social$X6160.2.0 == -7] <- 1
# assessment 3
social$X709.3.0[social$X709.3.0 == -3 | social$X709.3.0 == -1] <- NA
social$X709.3.0[social$X709.3.0 > 1] <- 0
social$X1031.3.0[social$X1031.3.0 == -3 | social$X1031.3.0 == -1] <- NA
social$X1031.3.0[social$X1031.3.0 <= 4] <- 0
social$X1031.3.0[social$X1031.3.0 > 4] <- 1
social$X6160.3.0[social$X6160.3.0 == -3] <- NA
social$X6160.3.0[social$X6160.3.0 > 0] <- 0
social$X6160.3.0[social$X6160.3.0 == -7] <- 1
social <- social %>% mutate(soc_isol_0 = rowSums(across(c(X709.0.0 , X1031.0.0, X6160.0.0)), na.rm = TRUE),
                            soc_isol_1 = rowSums(across(c(X709.1.0 , X1031.1.0, X6160.1.0)), na.rm = TRUE),
                            soc_isol_2 = rowSums(across(c(X709.2.0 , X1031.2.0, X6160.2.0)), na.rm = TRUE),
                            soc_isol_3 = rowSums(across(c(X709.3.0 , X1031.3.0, X6160.3.0)), na.rm = TRUE))
social$soc_isol_0[social$soc_isol_0 >= 2] <- 1
social$soc_isol_1[social$soc_isol_1 >= 2] <- 1
social$soc_isol_2[social$soc_isol_2 >= 2] <- 1
social$soc_isol_3[social$soc_isol_3 >= 2] <- 1
# among those that have no social isolation, check for NAs in any of the three questions
social <- social %>% mutate(na_count_0 = rowSums(across(c(X709.0.0, X1031.0.0, X6160.0.0), is.na)),
                            na_count_1 = rowSums(across(c(X709.1.0, X1031.1.0, X6160.1.0), is.na)),
                            na_count_2 = rowSums(across(c(X709.2.0, X1031.2.0, X6160.2.0), is.na)),
                            na_count_3 = rowSums(across(c(X709.3.0, X1031.3.0, X6160.3.0), is.na)))
# those with a score of 0 or 1 could have such a low score because of NAs, so we have to make sure
# if only one is NA and the other two are 0, keep categorised as not isolated
# if two or more are missing, categorise as NA
social$soc_isol_0[social$na_count_0 >= 2] <- NA
social$soc_isol_1[social$na_count_1 >= 2] <- NA
social$soc_isol_2[social$na_count_2 >= 2] <- NA
social$soc_isol_3[social$na_count_3 >= 2] <- NA
social <- social %>% select(id, starts_with('soc_isol'))




## death
death <- data_all %>% 
  select(c(eid, X40000.0.0))
colnames(death) <- c('id', 'death_date')
death$death_date <- as.Date(death$death_date, format = '%Y-%m-%d')
death$death <- 0; death$death[!is.na(death$death_date)] <- 1






## mood disorders
mood_ado <- data_all %>% 
  select(c(eid, starts_with(c('X130890.', 'X130892.', 'X130894.', 'X130896.', 'X130898.', 'X130900.', 'X130902.')))) %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d'))) %>%
  mutate(mood_dis_date = reduce(across(starts_with('X')), pmin, na.rm = TRUE)) %>%
  rename(id = eid) %>%
  select(id, mood_dis_date)
mood_ado$mood_dis <- 0; mood_ado$mood_dis[!is.na(mood_ado$mood_dis_date)] <- 1
mood_ado$mood_dis[mood_ado$mood_dis_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA
mood_ado$mood_dis_date[mood_ado$mood_dis_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA



## deprivation
deprivation <- data_all %>% 
  select(c(eid, starts_with(c('X189')))) %>%
  rename(id = eid, deprivation = X189.0.0)


# various other disorders
outcomes <- data_all %>% 
  select(c(eid, starts_with(c('X131484.', 'X131486.', 'X131488.', 'X131490.', 'X131492.', 'X131494.', 'X131496.', # respiratory disease
                              'X131498.', 'X131658.', 'X131660.', 'X131662.', 'X131664.', 'X131666.', 'X131668.', 'X131670.', # liver disease
                              'X131438.', 'X131440.', 'X131442.', 'X131444.', 'X131446.', 'X131448.', 'X131450.', 'X131452.', 'X131454.', 'X131456.', # influenza
                              'X131296.', 'X131298.', 'X131300.', 'X131302.', 'X131304.', 'X131306.'))))
outcomes <- outcomes %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d')))  %>%
  mutate(respiratory_date = reduce(across(c('X131484.0.0', 'X131486.0.0', 'X131488.0.0', 'X131490.0.0', 'X131492.0.0', 'X131494.0.0', 'X131496.0.0', 
                                            'X131498.0.0')), pmin, na.rm = TRUE)) %>%
  mutate(hepatic_date = reduce(across(c('X131658.0.0', 'X131660.0.0', 'X131662.0.0', 'X131664.0.0', 'X131666.0.0', 'X131668.0.0', 'X131670.0.0')), 
                               pmin, na.rm = TRUE)) %>%
  mutate(flu_date = reduce(across(c('X131438.0.0', 'X131440.0.0', 'X131442.0.0', 'X131444.0.0', 'X131446.0.0', 'X131448.0.0', 'X131450.0.0',
                                    'X131452.0.0', 'X131454.0.0', 'X131456.0.0')), pmin, na.rm = TRUE)) %>%
  mutate(heart_date = reduce(across(c('X131296.0.0', 'X131298.0.0', 'X131300.0.0', 'X131302.0.0', 'X131304.0.0', 'X131306.0.0')), pmin, na.rm = TRUE)) %>%
  rename(id = eid) %>%
  select(id, respiratory_date, hepatic_date, flu_date, heart_date) %>%  
  filter(rowSums(is.na(select(., -id))) != ncol(.) - 1)
outcomes$respiratory <- 0; outcomes$respiratory[!is.na(outcomes$respiratory_date)] <- 1
outcomes$respiratory[outcomes$respiratory_date %in% invalid_dates] <- 99
outcomes$respiratory_date[outcomes$respiratory_date %in% invalid_dates] <- NA
outcomes$hepatic <- 0; outcomes$hepatic[!is.na(outcomes$hepatic_date)] <- 1
outcomes$hepatic[outcomes$hepatic_date %in% invalid_dates] <- 99
outcomes$hepatic_date[outcomes$hepatic_date %in% invalid_dates] <- NA
outcomes$flu <- 0; outcomes$flu[!is.na(outcomes$flu_date)] <- 1
outcomes$flu[outcomes$flu_date %in% invalid_dates] <- 99
outcomes$flu_date[outcomes$flu_date %in% invalid_dates] <- NA
outcomes$heart <- 0; outcomes$heart[!is.na(outcomes$heart_date)] <- 1
outcomes$heart[outcomes$heart_date %in% invalid_dates] <- 99
outcomes$heart_date[outcomes$heart_date %in% invalid_dates] <- NA




## asthma
asthma <- data_all %>% 
  select(c(eid, starts_with(c('X42014.')))) %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d'))) %>%
  rename(id = eid, asthma_date = X42014.0.0) %>%
  select(id, asthma_date)
asthma$asthma <- 0; asthma$asthma[!is.na(asthma$asthma_date)] <- 1
asthma$asthma[asthma$asthma_date == as.Date('1900-01-01', format = '%Y-%m-%d')] <- NA
asthma$asthma_date[asthma$asthma_date == as.Date('1900-01-01', format = '%Y-%m-%d')] <- NA

## skin disorders
skin <- data_all %>% 
  select(c(eid, starts_with(c(FO_skin_fields)))) %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d'))) %>%
  mutate(skin_dis_date = reduce(across(starts_with('X')), pmin, na.rm = TRUE)) %>%
  rename(id = eid) %>%
  select(id, skin_dis_date)
skin$skin_dis <- 0; skin$skin_dis[!is.na(skin$skin_dis_date)] <- 1
skin$skin_dis[skin$skin_dis_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA
skin$skin_dis_date[skin$skin_dis_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA
skin$skin_dis[skin$skin_dis_date == as.Date('1903-03-03', format = '%Y-%m-%d')] <- NA
skin$skin_dis_date[skin$skin_dis_date == as.Date('1903-03-03', format = '%Y-%m-%d')] <- NA

## infections
infect <- data_all %>% 
  select(c(eid, starts_with(c(FO_infect_fields)))) %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d'))) %>%
  mutate(infect_date = reduce(across(starts_with('X')), pmin, na.rm = TRUE)) %>%
  rename(id = eid) %>%
  select(id, infect_date)
infect$infect <- 0; infect$infect[!is.na(infect$infect_date)] <- 1
infect$infect[infect$infect_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA
infect$infect_date[infect$infect_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA
infect$infect[infect$infect_date == as.Date('1903-03-03', format = '%Y-%m-%d')] <- NA
infect$infect_date[infect$infect_date == as.Date('1903-03-03', format = '%Y-%m-%d')] <- NA





## source of inpatient diagnosis

# get source from uk field ID 40022 (for those that have been to hospital and have just one data provider, this is the default)
inpatient_source <- data_all %>% 
  select(c(eid, starts_with(c('X40022.'))))
inpatient_source[inpatient_source == ''] <- NA

# set aside those with several sources
multi_source <- filter(inpatient_source, !is.na(X40022.0.1)) %>% select(eid)

# get most common source of hospital diagnoses for those with several records
diagnoses_dates <- read.csv('hesin.txt', sep='\t')  # UKB category 2006
diagnoses_dates$epistart <- as.Date(diagnoses_dates$epistart, format = '%d/%m/%Y')

# those with just one data provider throughout the entire period
inpatient_constant <- inpatient_source %>%
  filter(!eid %in% multi_source$eid) %>%
  rename(id = eid, data_provider = X40022.0.0) %>%
  select(id, data_provider)

# those with several data providers
inpatient_flux_freq <- diagnoses_dates %>%
  filter(eid %in% multi_source$eid) %>% # just resolve those with several data providers
  group_by(eid, dsource) %>% # group by data provider and id
  summarise(count = n()) %>% # count number of instances for id-source combo
  ungroup() %>%
  arrange(desc(count)) %>% # arrange descending
  distinct(eid, .keep_all = TRUE) %>% # just keep first (more frequent) instance
  rename(id = eid, data_provider = dsource) %>%
  select(id, data_provider)

# combine all most frequent inpatient data providers (i.e., for those with just one plus those with several)
inpatient_freq <- rbind(inpatient_constant, inpatient_flux_freq) %>%
  filter(!is.na(data_provider)) # remove those that never went to hospital

# get latest date of hospital diagnoses for those with multiple data providers (to determine censoring later on) 
diagnoses_dates_dt <- diagnoses_dates %>%
  filter(eid %in% multi_source$eid) %>%
  filter(!is.na(epistart)) %>%
  data.table::as.data.table(diagnoses_dates)
inpatient_flux_last <- as.data.frame(diagnoses_dates_dt[, .(epistart = max(epistart, na.rm = TRUE)), by = eid])
inpatient_flux_last <- merge(inpatient_flux_last, subset(diagnoses_dates, select = c(eid, epistart, dsource)), 
                             by = c('eid', 'epistart'), all.x = TRUE) %>%
  rename(id = eid, data_provider = dsource) %>%
  select(id, data_provider) %>%
  distinct(id, .keep_all = TRUE)

# combine all latest inpatient data providers
inpatient_last <- rbind(inpatient_constant, inpatient_flux_last) %>%
  filter(!is.na(data_provider))


### Now we have a data frame with the most frequently occurring and latest data providers for each participant.
### We still have missing data for those that were never admitted to hospital; for those, we will use primary
### care data to impute.


# For those that do not have inpatient data providers, we will first use GP registrations to fill the gaps
gp_reg <- read.csv('gp_registrations.txt', sep='\t', header=TRUE, quote='') %>% rename(id = eid)
gp_reg[gp_reg == ''] <- NA
gp_reg <- gp_reg %>%
  select(id, data_provider, reg_date, deduct_date) %>%
  mutate(across(c(reg_date, deduct_date), ~as.Date(., format = '%d/%m/%Y')))

# set aside those that were always registered with just one data provider
gp_reg_constant <- gp_reg %>%
  group_by(id, data_provider) %>%
  summarise(count = n()) %>%
  ungroup()
gp_reg_constant <- gp_reg_constant %>%
  filter(!id %in% gp_reg_constant$id[duplicated(gp_reg_constant$id)]) %>%
  select(id, data_provider)

# focus on those that changed primary care data providers
gp_reg_flux <- gp_reg %>%
  filter(!id %in% gp_reg_constant$id)
gp_reg_flux$reg_date[gp_reg_flux$reg_date %in% invalid_dates] <- NA
gp_reg_flux$deduct_date[gp_reg_flux$deduct_date %in% invalid_dates] <- NA
gp_reg_flux <- gp_reg_flux %>% 
  filter(!is.na(reg_date) | !is.na(deduct_date)) # remove observations without both registration and de-registration date

# people with registrations but without de-registrations were still registered 
# with their latest GP at time of data fetch, so get dates of data fetch
# (1= England(Vision), 2= Scotland, 3 = England (TPP), 4 = Wales)
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '1'] <- as.Date('30/06/2017', format = '%d/%m/%Y') # England Vision
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '2'] <- as.Date('31/05/2017', format = '%d/%m/%Y') # Scotland
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '3'] <- as.Date('31/08/2016', format = '%d/%m/%Y') # England TPP
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '4'] <- as.Date('30/09/2017', format = '%d/%m/%Y') # Wales
gp_reg_flux$total_time <- as.numeric((difftime(gp_reg_flux$deduct_date, gp_reg_flux$reg_date, units = 'days')))/365.25

# for registrations of people that changed data providers, calculate the length of the period of registration with each data provider
gp_reg_flux_freq <- gp_reg_flux %>%
  group_by(id, data_provider) %>%
  summarise(total_time = sum(total_time)) %>%
  arrange(desc(total_time)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, data_provider) %>%
  ungroup()

# get the latest registrations (for censoring)
gp_reg_flux_last <- gp_reg_flux %>%
  group_by(id, data_provider) %>%
  arrange(desc(deduct_date)) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, data_provider)

# combine most frequent and latest primary care data providers
gp_reg_freq <- rbind(gp_reg_constant, gp_reg_flux_freq)
gp_reg_last <- rbind(gp_reg_constant, gp_reg_flux_last)



# for people without good registration data, we will use primary care diagnosis data
gp_diagnoses <- data.table::fread('gp_clinical.txt', sep='\t', header=TRUE, quote='') %>%
  as.data.frame() %>%
  filter(!eid %in% gp_reg_freq$id) %>%
  select(eid, data_provider, event_dt) %>%
  mutate(across(event_dt, ~as.Date(., format = '%d/%m/%Y')))

# all that are left were always diagnosed within just one data provider
gp_diagnoses_constant <- gp_diagnoses %>%
  group_by(eid, data_provider) %>%
  summarise(count = n()) %>%
  ungroup()
gp_diagnoses_constant <- gp_diagnoses_constant %>%
  filter(!eid %in% gp_diagnoses_constant$eid[duplicated(gp_diagnoses_constant$eid)]) %>%
  select(eid, data_provider) %>%
  rename(id = eid)

# add to the ones identified using registration data
gp_reg_freq <- rbind(gp_reg_freq, gp_diagnoses_constant)
gp_reg_last <- rbind(gp_reg_last, gp_diagnoses_constant)


### We now additionally have data frames with the most frequently occurring primary care data provider and the latest
### data provider per participant. We now have to use this latest data for imputation.


#### check how good the imputation is (OPTIONAL) ####
# this is done on the subsample of people with inpatient data that also have primary care records
check_df_freq <- merge(inpatient_freq, gp_reg_freq, by = 'id')
table(check_df_freq$data_provider.x, check_df_freq$data_provider.y)

check_df_last <- merge(inpatient_last, gp_reg_last, by = 'id')
table(check_df_last$data_provider.x, check_df_last$data_provider.y)
#### check how good the imputation is (OPTIONAL) ####



# use the primary care data providers to supplement missing inpatient data providers 

# add primary care data providers for participants without inpatient data providers
data_provider_freq <- rbind(inpatient_freq, filter(gp_reg_freq, !id %in% inpatient_freq$id)) %>%
  rename(data_provider_freq = data_provider)
# repeat for latest data provider
data_provider_last <- rbind(inpatient_last, filter(gp_reg_last, !id %in% inpatient_last$id)) %>%
  rename(data_provider_last = data_provider)

# harmonise naming of data providers
data_provider_freq$data_provider_freq[data_provider_freq$data_provider_freq == 1 | data_provider_freq$data_provider_freq == 3] <- 'HES'
data_provider_freq$data_provider_freq[data_provider_freq$data_provider_freq == 2] <- 'SMR'
data_provider_freq$data_provider_freq[data_provider_freq$data_provider_freq == 4] <- 'PEDW'
data_provider_last$data_provider_last[data_provider_last$data_provider_last == 1 | data_provider_last$data_provider_last == 3] <- 'HES'
data_provider_last$data_provider_last[data_provider_last$data_provider_last == 2] <- 'SMR'
data_provider_last$data_provider_last[data_provider_last$data_provider_last == 4] <- 'PEDW'










# This code combines data fields 41270, 41271, 41280, and 41281, which refer to inpatient diagnoses and dates of admission.
# For 6 participants, array indexing when combining diagnoses codes and dates fails, so I used the record-level data to access
# hospital addmissions for those 6 participants.
diagnoses <- data_all %>%
  select(eid, starts_with(c('X41270.', 'X41280.', 'X41271.', 'X41281.'))) %>%
  rename(id = eid)

diagnoses[diagnoses=='']  <- NA 
diagnoses <- as.data.frame(sapply(diagnoses, as.character))

# separate sources of diagnoses (ICD9 vs 10) and dates vs. diagnosis codes
icd9 <- diagnoses %>% select(c('id', starts_with('X41271')))
icd9_date <- diagnoses %>% select(c('id', starts_with('X41281')))
icd10 <- diagnoses %>% select(c('id', starts_with('X41270')))
icd10_date <- diagnoses %>% select(c('id', starts_with('X41280')))

# keep only rows without NAs
icd9 <- icd9[rowSums(is.na(icd9))!=ncol(icd9)-1,]
icd9_date <- icd9_date[rowSums(is.na(icd9_date))!=ncol(icd9_date)-1,]
icd10 <- icd10[rowSums(is.na(icd10))!=ncol(icd10)-1,]
icd10_date <- icd10_date[rowSums(is.na(icd10_date))!=ncol(icd10_date)-1,]

# transform to long-type format
icd9_long <- icd9 %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE); colnames(icd9_long) <- c('id', 'column', 'diagnosis'); icd9_long$column <- sub('X41271.', '', icd9_long$column)
icd9_date_long <- icd9_date %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE); colnames(icd9_date_long) <- c('id', 'column', 'date'); icd9_date_long$column <- sub('X41281.', '', icd9_date_long$column)
icd10_long <- icd10 %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE); colnames(icd10_long) <- c('id', 'column', 'diagnosis');  icd10_long$column <- sub('X41270.', '', icd10_long$column)
icd10_date_long <- icd10_date %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE); colnames(icd10_date_long) <- c('id', 'column', 'date');  icd10_date_long$column <- sub('X41280.', '', icd10_date_long$column)

# combine all diagnoses
icd9 <- merge(icd9_long, icd9_date_long, by = c('id', 'column')); icd9$column <- NULL; icd9$version <- 'icd9'
icd10 <- merge(icd10_long, icd10_date_long, by = c('id', 'column')); icd10$column <- NULL; icd10$version <- 'icd10'
inpatient <- rbind(icd9, icd10)
saveRDS(inpatient, 'inpatient_diagnoses.rds') 


# NAs to 0s
outcomes <- outcomes %>%
  mutate(across(c(respiratory, hepatic, flu, heart), 
                ~ replace_na(., 0)))
# 99 dummy value to NA
outcomes[outcomes == 99] <- NA


# merge all data frames in the list
covariates <- Reduce(function(x, y) merge(x, y, by = 'id', all = TRUE), 
                    list(dementia, dems, education, deprivation, cognition, social, death, mood_ado, outcomes, 
                         asthma, skin, infect, data_provider_last, data_provider_freq))

covariates$id <- as.character(covariates$id)

# export and clear environment
saveRDS(covariates, 'covariates_prepped.Rds')
rm(list = ls())