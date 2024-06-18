library(tidyverse)
library(zoo)
library(purrr)
library(lavaan)
library(semPlot)


source('0_helper_functions.R')


invalid_dates <- as.Date(c('01/01/1900', '01/01/1901', '02/02/1902', '03/03/1903', 
                           '07/07/2037'), format = '%d/%m/%Y')

# main dataset
data_all <- readRDS('main_vars.Rds')

# remove people who have opted out of the study
opt_outs <- read.csv('participant_opt_out.csv')
opt_outs$id <- as.character(opt_outs$id)
data_all <- filter(data_all, !eid %in% opt_outs$id)



## dementia
dementia <- data_all %>% select(c(eid, starts_with(c('X42018.'))))
colnames(dementia) <- c('id', 'dementia_date')
dementia$dementia_date <- as.Date(dementia$dementia_date, format <- '%Y-%m-%d')
dementia$dementia <- 0; dementia$dementia[!is.na(dementia$dementia_date)] <- 1
dementia$dementia[dementia$dementia_date == as.Date('1900-01-01', format = '%Y-%m-%d')] <- NA
dementia$dementia_date[dementia$dementia_date == as.Date('1900-01-01', format = '%Y-%m-%d')] <- NA



## assessment dates, age at assessments and sex
dems <- data_all %>% select(c(eid, starts_with(c('X31.', 'X53.', 'X34.', 
                                                 'X52.'))))
dems <- dems %>%
  rename(sex = X31.0.0, date_0 = X53.0.0, date_1 = X53.1.0, 
         date_2 = X53.2.0, date_3 = X53.3.0,
         birth_year = X34.0.0, birth_month = X52.0.0)
dems$birth_year <- as.character(dems$birth_year); dems$birth_month <- 
  as.character(dems$birth_month)
dems$birth_date <- as.Date(paste0('01/', dems$birth_month, '/' ,dems$birth_year), 
                           format = '%d/%m/%Y')
dems[, c('date_0', 'date_1', 'date_2', 'date_3')] <- 
  lapply(dems[, c('date_0', 'date_1', 'date_2', 'date_3')], as.Date, format = '%Y-%m-%d')
dems$age_0 <- as.numeric(difftime(dems$date_0, dems$birth_date, units='days'))/365.25
dems$age_1 <- as.numeric(difftime(dems$date_1, dems$birth_date, units='days'))/365.25
dems$age_2 <- as.numeric(difftime(dems$date_2, dems$birth_date, units='days'))/365.25
dems$age_3 <- as.numeric(difftime(dems$date_3, dems$birth_date, units='days'))/365.25
dems <- dems %>%
  select(-c('birth_year', 'birth_month')) %>%
  rename(id = eid)


## ethnicity
ethnicity <- data_all %>% 
  select(c(eid, X21000.0.0)) %>%
  rename(id = eid, ethnicity = X21000.0.0)
ethnicity$ethnicity_cat <- ethnicity$ethnicity
ethnicity$ethnicity_cat[ethnicity$ethnicity %in% c(1, 1001, 1002, 1003)] <- 1 # white
ethnicity$ethnicity_cat[ethnicity$ethnicity %in% c(4, 4001, 4002, 4003)] <- 2 # black
ethnicity$ethnicity_cat[ethnicity$ethnicity %in% c(3, 3001, 3002, 3003, 3004, 5)] <- 3 # Asian
ethnicity$ethnicity_cat[ethnicity$ethnicity %in% c(2, 2001, 2002, 2003, 2004, 6)] <- 4 # other
ethnicity$ethnicity_cat[ethnicity$ethnicity %in% c(-1, -3)] <- NA
ethnicity$ethnicity <- NULL
ethnicity <- rename(ethnicity, ethnicity = ethnicity_cat)



## education
# create subsets of the education frame for each assessment; change -3 to NA;
# distinguish higher, secondary/vocational, and none
# 1,6: higher
# 2,3,4,5: secondary/vocational
# -7: none
education <- data_all %>% select(eid, starts_with('X6138.')) %>% rename(id = eid)
# add maximum qualification per participant per assessment visit
education <- education %>%
  mutate(education_0 = apply(select(., starts_with('X6138.0')), 1, function(x) education_classify(x)),
         education_1 = apply(select(., starts_with('X6138.1')), 1, function(x) education_classify(x)),
         education_2 = apply(select(., starts_with('X6138.2')), 1, function(x) education_classify(x)),
         education_3 = apply(select(., starts_with('X6138.3')), 1, function(x) education_classify(x))) %>%
  select(id, education_0, education_1, education_2, education_3)



## cognition
cognition <- data_all %>% 
  select(c(eid, starts_with(c('X20016.', 'X20018.', 'X20023.', 'X399.', 'X6351.', 
                              'X6373.', 'X23324.', 'X4282.', 'X21004.'))))
# use only second round
cognition <- subset(cognition, select=-c(X399.0.3, X399.1.3, X399.2.3, X399.3.3, 
                                         X399.0.1, X399.1.1, X399.2.1, X399.3.1))
# numerical memory was not tested then, and the columns has all NAs
cognition <- subset(cognition, select=-c(X4282.1.0)) 
colnames(cognition) <- c('id', 'VNR_0', 'VNR_1', 'VNR_2', 'VNR_3', 'ProsMem_0', 
                         'ProsMem_1', 'ProsMem_2', 'ProsMem_3', 'RT_0', 'RT_1', 
                         'RT_2', 'RT_3', 'VisMem_0', 'VisMem_1', 'VisMem_2', 
                         'VisMem_3', 'TMTb_2', 'TMTb_3', 'MR_2', 'MR_3', 'DSS_2', 
                         'DSS_3', 'NM_0', 'NM_2', 'NM_3', 'TR_2', 'TR_3')
# remove outliers 
cognition[, c('VNR_0', 'VNR_1', 'VNR_2', 'VNR_3', 'RT_0', 'RT_1', 'RT_2', 'RT_3', 
              'VisMem_0', 'VisMem_1', 'VisMem_2', 'VisMem_3', 'ProsMem_0', 
              'ProsMem_1', 'ProsMem_2', 'ProsMem_3', 'DSS_2', 'DSS_3', 'MR_2', 
              'MR_3', 'TMTb_2', 'TMTb_3', 'NM_0', 'NM_2', 'NM_3', 'TR_2', 'TR_3')] <- 
  lapply(cognition[, c('VNR_0', 'VNR_1', 'VNR_2', 'VNR_3', 'RT_0', 'RT_1', 'RT_2', 
                       'RT_3', 'VisMem_0', 'VisMem_1', 'VisMem_2', 'VisMem_3', 
                        'ProsMem_0', 'ProsMem_1', 'ProsMem_2', 'ProsMem_3', 
                       'DSS_2', 'DSS_3', 'MR_2', 'MR_3', 'TMTb_2', 'TMTb_3', 
                       'NM_0', 'NM_2', 'NM_3', 'TR_2', 'TR_3')], 
         outliers, var_metric=4, method='SD')

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
      MR_2 ~~ VNR_2               
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
      MR_3 ~~ VNR_3               
      RT_3 ~~ DSS_3
      '
fit_3 <- sem(model_3, data=cognition, missing='fiml.x')
cognition$g_3 <- as.vector(predict(fit_3, cognition))
cognition <- cognition %>% 
  select(id, g_0, g_1, g_2, g_3)
rm(fit_0, fit_1, fit_2, fit_3)


# social isolation
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
social <- social %>% 
  mutate(soc_isol_0 = rowSums(across(c(X709.0.0 , X1031.0.0, X6160.0.0)), na.rm = TRUE),
         soc_isol_1 = rowSums(across(c(X709.1.0 , X1031.1.0, X6160.1.0)), na.rm = TRUE),
         soc_isol_2 = rowSums(across(c(X709.2.0 , X1031.2.0, X6160.2.0)), na.rm = TRUE),
         soc_isol_3 = rowSums(across(c(X709.3.0 , X1031.3.0, X6160.3.0)), na.rm = TRUE))
social$soc_isol_0[social$soc_isol_0 >= 2] <- 1
social$soc_isol_1[social$soc_isol_1 >= 2] <- 1
social$soc_isol_2[social$soc_isol_2 >= 2] <- 1
social$soc_isol_3[social$soc_isol_3 >= 2] <- 1
# among those that have no social isolation, check for NAs in any of the three questions
social <- social %>% 
  mutate(na_count_0 = rowSums(across(c(X709.0.0, X1031.0.0, X6160.0.0), is.na)),
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
# include loss to follow-up
follow_loss <- data_all %>% 
  select(c(eid, X191.0.0))
colnames(follow_loss) <- c('id', 'follow_loss_date')
follow_loss$follow_loss_date <- as.Date(follow_loss$follow_loss_date, format = '%Y-%m-%d')
follow_loss$follow_loss <- 0; follow_loss$follow_loss[!is.na(follow_loss$follow_loss_date)] <- 1
early_cens <- merge(death, follow_loss, by = 'id', all = TRUE)





## mood disorders
mood_ado <- data_all %>% 
  select(c(eid, starts_with(c('X130890.', 'X130892.', 'X130894.', 'X130896.', 
                              'X130898.', 'X130900.', 'X130902.')))) %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d'))) %>%
  mutate(mood_dis_date = reduce(across(starts_with('X')), pmin, na.rm = TRUE)) %>%
  rename(id = eid) %>%
  select(id, mood_dis_date)
mood_ado$mood_dis <- 0; mood_ado$mood_dis[!is.na(mood_ado$mood_dis_date)] <- 1
mood_ado$mood_dis[mood_ado$mood_dis_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA
mood_ado$mood_dis_date[mood_ado$mood_dis_date == as.Date('1902-02-02', format = '%Y-%m-%d')] <- NA



## deprivation
deprivation <- read.csv('D://Job/Raw data/deprivation.csv')
#deprivation <- data_all %>% 
#  select(c(eid, starts_with(c('X189')))) %>%
#  rename(id = eid, deprivation = X189.0.0)
# TODO: remove below two lines when id confusion clarifies
deprivation <- filter(deprivation, !id %in%opt_outs$id)



# various other disorders
outcomes <- data_all %>% 
  # respiratory disease
  select(c(eid, starts_with(c('X131484.', 'X131486.', 'X131488.', 'X131490.', 
                              'X131492.', 'X131494.', 'X131496.',
                              # liver disease
                              'X131498.', 'X131658.', 'X131660.', 'X131662.', 
                              'X131664.', 'X131666.', 'X131668.', 'X131670.',
                              # influenza
                              'X131438.', 'X131440.', 'X131442.', 'X131444.', 
                              'X131446.', 'X131448.', 'X131450.', 'X131452.', 
                              'X131454.', 'X131456.', 'X131296.', 'X131298.', 
                              'X131300.', 'X131302.', 'X131304.', 'X131306.')))) %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d')))  %>%
  mutate(respiratory_date = 
           reduce(across(c('X131484.0.0', 'X131486.0.0', 'X131488.0.0', 
                           'X131490.0.0', 'X131492.0.0', 'X131494.0.0', 
                           'X131496.0.0', 'X131498.0.0')), 
                  pmin, na.rm = TRUE)) %>%
  mutate(hepatic_date = reduce(across(c('X131658.0.0', 'X131660.0.0', 'X131662.0.0', 
                                        'X131664.0.0', 'X131666.0.0', 'X131668.0.0', 
                                        'X131670.0.0')), 
                               pmin, na.rm = TRUE)) %>%
  mutate(flu_date = reduce(across(c('X131438.0.0', 'X131440.0.0', 'X131442.0.0', 
                                    'X131444.0.0', 'X131446.0.0', 'X131448.0.0', 
                                    'X131450.0.0', 'X131452.0.0', 'X131454.0.0', 
                                    'X131456.0.0')), pmin, na.rm = TRUE)) %>%
  mutate(heart_date = reduce(across(c('X131296.0.0', 'X131298.0.0', 'X131300.0.0', 
                                      'X131302.0.0', 'X131304.0.0', 'X131306.0.0')), 
                             pmin, na.rm = TRUE)) %>%
  rename(id = eid) %>%
  select(id, respiratory_date, hepatic_date, flu_date, heart_date)
outcomes$respiratory <- 0; outcomes$respiratory[!is.na(outcomes$respiratory_date)] <- 1
outcomes$respiratory[outcomes$respiratory_date %in% invalid_dates] <- 999
outcomes$respiratory_date[outcomes$respiratory_date %in% invalid_dates] <- NA
outcomes$hepatic <- 0; outcomes$hepatic[!is.na(outcomes$hepatic_date)] <- 1
outcomes$hepatic[outcomes$hepatic_date %in% invalid_dates] <- 999
outcomes$hepatic_date[outcomes$hepatic_date %in% invalid_dates] <- NA
outcomes$flu <- 0; outcomes$flu[!is.na(outcomes$flu_date)] <- 1
outcomes$flu[outcomes$flu_date %in% invalid_dates] <- 999
outcomes$flu_date[outcomes$flu_date %in% invalid_dates] <- NA
outcomes$heart <- 0; outcomes$heart[!is.na(outcomes$heart_date)] <- 1
outcomes$heart[outcomes$heart_date %in% invalid_dates] <- 999
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



## appendicitis
appendicitis <- data_all %>% 
  select(c(eid, starts_with(c('X131604.', 'X131606.', 'X131608.')))) %>%
  mutate(across(starts_with('X'), ~as.Date(., format = '%Y-%m-%d'))) %>%
  mutate(appendicitis_date = reduce(across(starts_with('X')), pmin, na.rm = TRUE)) %>%
  rename(id = eid) %>%
  select(id, appendicitis_date)
appendicitis$appendicitis <- 0
appendicitis$appendicitis[!is.na(appendicitis$appendicitis_date)] <- 1







# This code combines data fields 41270, 41271, 41280, and 41281, 
# which refer to inpatient diagnoses and dates of admission.
# For 6 participants, array indexing when combining diagnoses codes and dates fails, 
# so I used the record-level data to access hospital addmissions for those 6 participants.
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
icd9_long <- icd9 %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE)
colnames(icd9_long) <- c('id', 'column', 'diagnosis')
icd9_long$column <- sub('X41271.', '', icd9_long$column)

icd9_date_long <- icd9_date %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE)
colnames(icd9_date_long) <- c('id', 'column', 'date')
icd9_date_long$column <- sub('X41281.', '', icd9_date_long$column)

icd10_long <- icd10 %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE)
colnames(icd10_long) <- c('id', 'column', 'diagnosis')
icd10_long$column <- sub('X41270.', '', icd10_long$column)

icd10_date_long <- icd10_date %>%  pivot_longer(-id, names_to = 'diagnosis', values_drop_na=TRUE)
colnames(icd10_date_long) <- c('id', 'column', 'date')
icd10_date_long$column <- sub('X41280.', '', icd10_date_long$column)

# combine all diagnoses
icd9 <- merge(icd9_long, icd9_date_long, by = c('id', 'column'))
icd9$column <- NULL; icd9$version <- 'icd9'
icd10 <- merge(icd10_long, icd10_date_long, by = c('id', 'column'))
icd10$column <- NULL; icd10$version <- 'icd10'

inpatient <- rbind(icd9, icd10)
saveRDS(inpatient, 'inpatient_diagnoses.rds')









### Diagnoses by manual search of hospital or GP records
# (tinnitus, primary care dementia, fracture, and head injury)

# inpatient diagnoses
colnames(inpatient)[colnames(inpatient) == 'diagnosis'] <- 'code'
inpatient$date <- as.Date(inpatient$date, format = '%Y-%m-%d')
inpatient$diagnosis <- NA

# GP diagnoses
gp_diagnoses <- data.table::fread('gp_clinical.txt', 
                                  sep='\t', header=TRUE, quote='') %>%
  as.data.frame() %>%
  select(eid, data_provider, event_dt, read_2, read_3)

colnames(gp_diagnoses) <- c('id', 'data_provider', 'date_primary', 'read2', 'read3')
gp_diagnoses$date_primary <- as.Date(gp_diagnoses$date_primary, format = '%d/%m/%Y')
gp_diagnoses[gp_diagnoses == ''] <- NA
gp_diagnoses$diagnosis <- NA





# diagnosis codes
accident_codes <- unique(inpatient[(grep('^V', inpatient$code)), 'code'])

diagnosis_codes <- data.frame(
  disorder = c('tinnitus', 'tinnitus',
               rep('hip_fract', 31),
               rep('head_inj', 56),
               rep('accident', length(accident_codes))),
  code = c(c('H931', '3883'),
           '820', '8200', '8202', '8208', '821', '8210', '8211', '8212', 'S72', 
           'S720', 'S7200', 'S7201', 'S721', 'S7210', 'S7211', 'S722', 'S7220', 
           'S7221', 'S723', 'S7230', 'S7231', 'S724', 'S7240', 'S7241', 'S727', 
           'S7270', 'S728', 'S7280', 'S729', 'S7290', 'S7291', 
           'S06', 'S060', 'S0600', 'S0601', 'S061', 'S0610', 
           'S062', 'S0620',
           'S0621', 'S063', 'S0630', 'S0631', 'S064', 'S0640', 'S0641', 'S065', 
           'S0650', 'S0651', 'S066', 'S0660', 'S0661', 'S067', 'S0670', 'S068', 
           'S0680', 'S0681', 'S069', 'S0690', 'S0691', 'S020', 'S0200', 'S0201',
           'S0210', 'S0211', 'S023', 'S0230', 'S0231', '850', '8509', '851', 
           '8510', '852', '8520', '853', '8530', '854', '8540', '8541',
           '800', '8000', '8001', '8002', '8003', '801', '8010', '8011',
           accident_codes),
  source = c('icd10', 'icd9', rep('icd9', 8), rep('icd10', 23),
             rep('icd10', 37), rep('icd9', 8),
             rep('icd9', 11), rep('read2', 93), rep('read3', 202),
             rep('icd10', 95), rep('icd10', length(accident_codes))))

diagnosis_codes$n <- NA
diagnosis_codes <- diagnosis_codes %>%
  distinct(disorder, code, .keep_all = TRUE)

# match codes with descriptions
inpatient <- inpatient %>% arrange(date)
for (d in c('icd9', 'icd10')){
  for (diagnosis in diagnosis_codes$code[diagnosis_codes$source == d]){
    
    inpatient$diagnosis[inpatient$version == d & inpatient$code == diagnosis] <- 
      diagnosis_codes$disorder[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis]
    
    diagnosis_codes$n[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis] <- 
      length(inpatient$diagnosis[inpatient$version == d & inpatient$code == diagnosis])
  }
}




# repeat for primary care (just for dementia)
gp_diagnoses <- gp_diagnoses %>% arrange(date_primary)
for (d in c('read2', 'read3')){
  for (diagnosis in diagnosis_codes$code[diagnosis_codes$source == d]){
    
    gp_diagnoses$diagnosis[!is.na(gp_diagnoses[[d]]) & gp_diagnoses[[d]] == diagnosis] <- 
      diagnosis_codes$disorder[diagnosis_codes$source == d & 
                                 diagnosis_codes$code == diagnosis]
    
    diagnosis_codes$n[diagnosis_codes$source == d & diagnosis_codes$code == diagnosis] <- 
      length(gp_diagnoses$diagnosis[!is.na(gp_diagnoses[[d]]) & 
                                      gp_diagnoses[[d]] == diagnosis])
  }
}



  


## tinnitus
# inpatient data
tinnitus <- inpatient %>%
  filter(diagnosis == 'tinnitus') %>%
  select(id, date) %>%
  rename(tinnitus_date = date) %>%
  distinct(id, .keep_all = TRUE)

# self-report data
tinnitus_sr <- data_all %>%
  select(eid, starts_with(c('X4803.', 'X53.'))) %>%
  rename(id = eid) %>%
  merge(., tinnitus, by = 'id', all = TRUE)

# for each assessment, classify into binary
for (col in c('0.0', '1.0', '2.0', '3.0')){
  tin_col <- paste0('X4803.', col)
  date_col <- paste0('X53.', col)
  # specify category
  tinnitus_sr[[tin_col]][tinnitus_sr[[tin_col]] %in% c(11, 12, 13, 14)] <- 1
  tinnitus_sr[[tin_col]][tinnitus_sr[[tin_col]] %in% c(0)] <- 0
  tinnitus_sr[[tin_col]][tinnitus_sr[[tin_col]] %in% c(-3, -1)] <- 999
  # assume that if answer left blank, answer is 0
  tinnitus_sr[[tin_col]][is.na(tinnitus_sr[[tin_col]]) & !is.na(tinnitus_sr[[date_col]])] <- 0
}

# all those that indicated tinnitus previously get 1 at later assessments
# (because we're interested in history of tinnitus)
tinnitus_sr$X4803.0.0[!is.na(tinnitus_sr$tinnitus_date) & 
                        tinnitus_sr$tinnitus_date < tinnitus_sr$X53.0] <- 1
tinnitus_sr$X4803.1.0[(tinnitus_sr$X4803.0.0 == 1) |
                        (!is.na(tinnitus_sr$tinnitus_date) & 
                        tinnitus_sr$tinnitus_date < tinnitus_sr$X53.1)] <- 1
tinnitus_sr$X4803.2.0[(tinnitus_sr$X4803.1.0 == 1) |
                        (!is.na(tinnitus_sr$tinnitus_date) & 
                           tinnitus_sr$tinnitus_date < tinnitus_sr$X53.2)] <- 1
tinnitus_sr$X4803.3.0[(tinnitus_sr$X4803.2.0 == 1) |
                        (!is.na(tinnitus_sr$tinnitus_date) & 
                           tinnitus_sr$tinnitus_date < tinnitus_sr$X53.3)] <- 1
tinnitus_sr <- tinnitus_sr %>%
  select(id, starts_with('X4803.'))
colnames(tinnitus_sr) <- c('id', 'tinnitus_sr_0', 'tinnitus_sr_1', 'tinnitus_sr_2',
                           'tinnitus_sr_3')
tinnitus_sr[is.na(tinnitus_sr)] <- 0




## hip fracture
hip_fract <- inpatient %>%
  filter(diagnosis == 'hip_fract') %>%
  select(id, date) %>%
  rename(hip_fract_date = date) %>%
  distinct(id, .keep_all = TRUE)
hip_fract$hip_fract <- 1
hip_fract <- merge(hip_fract, select(dems, id), 
                   by = 'id', all.y = TRUE)
hip_fract$hip_fract[is.na(hip_fract$hip_fract_date)] <- 0



## transport accident
trans_acc <- inpatient %>%
  filter(diagnosis == 'accident') %>%
  select(id, date) %>%
  rename(trans_acc_date = date) %>%
  distinct(id, .keep_all = TRUE)
trans_acc$trans_acc <- 1
trans_acc <- merge(trans_acc, select(dems, id), 
                   by = 'id', all.y = TRUE)
trans_acc$trans_acc[is.na(trans_acc$trans_acc_date)] <- 0




## head injury
head_inj <- inpatient %>%
  filter(diagnosis == 'head_inj') %>%
  select(id, date) %>%
  rename(head_inj_date = date) %>%
  distinct(id, .keep_all = TRUE)
head_inj$head_inj <- 1
head_inj <- merge(head_inj, select(dems, id), 
                   by = 'id', all.y = TRUE)
head_inj$head_inj[is.na(head_inj$head_inj_date)] <- 0










## source of inpatient diagnosis

# get source from uk field ID 40022 
# (the default for those that have been to hospital and have just one data provider)
inpatient_source <- data_all %>% 
  select(c(eid, starts_with(c('X40022.'))))
inpatient_source[inpatient_source == ''] <- NA

# set aside those with several sources
multi_source <- filter(inpatient_source, !is.na(X40022.0.1)) %>% select(eid)

# set aside those without any source
no_source <- filter(inpatient_source, is.na(X40022.0.0)) %>% select(eid)

# get most common source of hospital diagnoses for those with several records
diagnoses_dates <- data.table::fread('hesin.txt', sep='\t') %>%
  as.data.frame() %>%
  filter(!eid %in% opt_outs$id)
diagnoses_dates$epistart <- as.Date(diagnoses_dates$epistart, format = '%d/%m/%Y')

# first, those with just one data provider throughout the entire period
inpatient_constant <- inpatient_source %>%
  filter(!eid %in% multi_source$eid) %>%
  rename(id = eid, data_provider = X40022.0.0) %>%
  select(id, data_provider)

# second, those with several data providers
inpatient_flux_freq <- diagnoses_dates %>%
  # just resolve those with several data providers
  filter(eid %in% multi_source$eid | eid %in% no_source$eid) %>% 
  # group by data provider and id
  group_by(eid, dsource) %>% 
  # count number of instances for id-source combo
  summarise(count = n()) %>% 
  ungroup() %>%
  # arrange descending
  arrange(desc(count)) %>% 
  # just keep first (more frequent) instance
  distinct(eid, .keep_all = TRUE) %>% 
  rename(id = eid, data_provider = dsource) %>%
  select(id, data_provider)

# combine all most frequent inpatient data providers 
# (i.e., for those with just one plus those with several)
inpatient_freq <- rbind(inpatient_constant, inpatient_flux_freq) %>%
  filter(!is.na(data_provider)) # remove those that never went to hospital

# get latest date of hospital diagnoses for those with multiple data providers 
# or no data provider (to determine censoring later on) 
diagnoses_dates_dt <- diagnoses_dates %>%
  filter(eid %in% multi_source$eid | eid %in% no_source$eid) %>%
  filter(!is.na(epistart)) %>%
  data.table::as.data.table(diagnoses_dates)
inpatient_flux_last <- as.data.frame(diagnoses_dates_dt[, .(epistart = 
                                                              max(epistart, na.rm = TRUE)), by = eid])
inpatient_flux_last <- merge(inpatient_flux_last, subset(diagnoses_dates, select = 
                                                           c(eid, epistart, dsource)), 
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
gp_reg <- read.csv('gp_registrations.txt', sep='\t', header=TRUE, quote='') %>%
  rename(id = eid) %>%
  filter(!id %in% opt_outs$id)
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
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '1'] <- 
  as.Date('30/06/2017', format = '%d/%m/%Y') # England Vision
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '2'] <- 
  as.Date('31/05/2017', format = '%d/%m/%Y') # Scotland
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '3'] <- 
  as.Date('31/08/2016', format = '%d/%m/%Y') # England TPP
gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '4'] <- 
  as.Date('30/09/2017', format = '%d/%m/%Y') # Wales
gp_reg_flux$total_time <- as.numeric((difftime(gp_reg_flux$deduct_date, gp_reg_flux$reg_date, units = 'days')))/365.25

# for registrations of people that changed data providers, 
# calculate the length of the period of registration with each data provider
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
  filter(!eid %in% gp_reg_freq$id & !eid %in% opt_outs$id) %>%
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

# change naming of old columns since we will use them later
gp_reg_freq <- gp_reg_freq %>%
  rename(data_provider_freq_gp = data_provider)
gp_reg_last <- gp_reg_last %>%
  rename(data_provider_last_gp = data_provider)
data_provider_freq$data_provider_freq[data_provider_freq$data_provider_freq == 1 | 
                                        data_provider_freq$data_provider_freq == 3] <- 'HES'
data_provider_freq$data_provider_freq[data_provider_freq$data_provider_freq == 2] <- 'SMR'
data_provider_freq$data_provider_freq[data_provider_freq$data_provider_freq == 4] <- 'PEDW'
data_provider_last$data_provider_last[data_provider_last$data_provider_last == 1 |
                                        data_provider_last$data_provider_last == 3] <- 'HES'
data_provider_last$data_provider_last[data_provider_last$data_provider_last == 2] <- 'SMR'
data_provider_last$data_provider_last[data_provider_last$data_provider_last == 4] <- 'PEDW'







# merge all data frames in the list
covariates <- Reduce(function(x, y) merge(x, y, by = 'id', all = TRUE), 
                     list(dementia, dementia_gp, dems, education, deprivation, 
                          cognition, social, early_cens, mood_ado, outcomes, asthma, 
                          skin, infect, tinnitus_sr, head_inj, ethnicity, fract_any,
                          trans_acc, appendicitis, data_provider_last, data_provider_freq,
                          gp_reg_freq, gp_reg_last))

# 999 dummy value to NA
covariates[covariates == 999] <- NA

covariates$id <- as.character(covariates$id)

# export and clear environment
saveRDS(covariates, 'covariates_prepped.Rds')
rm(list = ls()); gc()