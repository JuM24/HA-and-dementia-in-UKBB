# the same as `4_clean_data.R` but for negative control outcomes and without
# the per-protocol approach (so only intention-to-treat)

outcomes <- c('hepatic', 'heart', 'respiratory', 'asthma', 'skin_dis', 'infect', 
              'flu', 'appendicitis', 'hip_fract', 'trans_acc')

library(tidyverse)
source('0_helper_functions.R')


for (outcome in outcomes){
  
  # convert the strings to symbols 
  # (the '!!' is then later used within `dplyr` to unquote the symbol)
  outcome_date <- paste0(outcome, '_date')
  outcome_sym <- rlang::sym(outcome)
  outcome_date_sym <- rlang::sym(outcome_date)
  
  hear <- readRDS('hearing_masterfile_prepped.rds')
  
  # file with censoring dates
  cens_dates <- readxl::read_excel('censoring_dates.xlsx')
  cens_dates$date <- as.Date(cens_dates$date, format = '%d.%m.%Y')
  
  # we are interested in only those participants that have hearing loss; remove those without
  # we do not keep those with congenital hearing loss
  # we also remove those with cochlear implants, as they cannot remove them before the SiN
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
  
  
  # remove those with outcome before or at the same date as hearing loss
  hear <- filter(hear, !!outcome_sym == 0 | (date_hear_loss_any < !!outcome_date_sym))
  
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
  
  # Those without imputed censoring date get the value '0' for data provider value
  hear$data_provider_last[is.na(hear$data_provider_last)] <- '0'
  hear$data_provider_last <- as.factor(hear$data_provider_last)
  # Same for data provider used as confounder
  hear$data_provider_freq[is.na(hear$data_provider_freq)] <- '0'
  hear$data_provider_freq <- as.factor(hear$data_provider_freq)
  
  # set the censoring dates to the date of dementia/death, whichever occurs earlier.
  hear$data_cens_date <- NA
  hear$data_cens_date[hear$data_provider_last == 'HES'] <- 
    cens_dates$date[cens_dates$disorder == outcome & cens_dates$data_provider == 'HES']
  hear$data_cens_date[hear$data_provider_last == 'SMR'] <- 
    cens_dates$date[cens_dates$disorder == outcome & cens_dates$data_provider == 'SMR']
  hear$data_cens_date[hear$data_provider_last == 'PEDW'] <- 
    cens_dates$date[cens_dates$disorder == outcome & cens_dates$data_provider == 'PEDW']
  hear$data_cens_date[hear$data_provider_last == '0'] <- 
    min(cens_dates$date[cens_dates$disorder == outcome])
  hear$data_cens_date <- zoo::as.Date(hear$data_cens_date)
  
  # set the censoring dates to the date that occurs earlier.
  hear <- hear %>%
    mutate(censor_date = pmin(!!outcome_date_sym, death_date, 
                              follow_loss_date, data_cens_date, na.rm = TRUE))
  
  # for those that are labelled as having experienced the outcome after the censoring date, set the outcome to 0
  hear[[outcome_sym]][hear$censor_date < hear[[outcome_date_sym]]] <- 0
  hear$death[hear$censor_date < hear$death_date] <- 0
  
  # some will have experienced HL only after the censoring date; remove those
  hear <- filter(hear, date_hear_loss_any < censor_date)
  
  # age at HL
  hear$age_USE <- as.numeric(difftime(hear$date_hear_loss_any, 
                                      hear$birth_date, units = 'days'))/365.25
  
  # define the 'grace period'. This is the period from HL in which start of 
  # use of HA will still lead to the classification as 'exposed'
  hear$hear_loss_time <- as.numeric(difftime(hear$date_hear_aid_any, 
                                             hear$date_hear_loss_any, units='days'))/365.25
  hear$grace_period[hear$hear_loss_time <= 1 | hear$hear_aid_any == 0] <- 1
  hear$grace_period[hear$hear_loss_time > 1] <- 0
  # for those with HA beyond the grace period, set HA status to 0
  hear$hear_aid_any[hear$grace_period == 0] <- 0
  
  # just keep non-missing rows (hear_loss_time_USE, gp_time only for sensitivity analyses)
  # just keep non-missing rows and individuals with assessment data <=5 years removed from the date of HL
  hear <- hear %>%
    filter(if_all(c(age_USE, sex, education_USE, deprivation, g_USE, srt_min_USE, 
                    data_provider_freq, tinnitus_sr_USE, ethnicity, !!outcome_sym), 
                  ~ !is.na(.)) &
             min_diff_education <= 5*365.25 & 
             min_diff_g <= 5*365.25 & 
             min_diff_srt_min <= 5*365.25 &
             min_diff_tinnitus_sr <= 5*365.25)
  
  # To factors.
  hear <- hear %>% mutate(across(c(education_USE, sex, data_provider_freq, 
                                   soc_isol_USE, mood_dis, tinnitus_sr_USE,
                                   ethnicity), as.factor))
  # Some participants might have been censored due to death or dementia 
  # after HL but within the grace period. Thus, within the period between 
  # HL and censoring, those participants were at the same time exposed and unexposed.
  # To avoid this, we will - among those that are censored before the grace period 
  # ends - randomly assign participants to either treatment or non-treatment group.
  hear$early_cens <- 0
  hear$early_cens[difftime(hear$censor_date, hear$date_hear_loss_any) < 365.25] <- 1
  # if early_cens == 1, randomly choose 0 or 1 and assign to hear_aid_any; 
  # otherwise (i.e., if early_cens = 0), keep old value the random sampling is biased 
  # so that there is only a chance of being assigned to the treatment group that corresponds with
  # the prevalence of hearing aid use in the rest of the sample
  prop_ha <- round(as.numeric(prop.table(table(filter(hear, early_cens == 0)$hear_aid_any))[2]), 2)
  prop_han <- round(as.numeric(prop.table(table(filter(hear, early_cens == 0)$hear_aid_any))[1]), 2)
  set.seed(24)
  hear <- hear %>%
    rowwise %>%
    mutate(hear_aid_any = ifelse(early_cens == 1, 
                                 sample(c(0, 1), 1, 
                                        replace = TRUE, 
                                        prob = c(prop_han, prop_ha)), hear_aid_any)) %>%
    ungroup()
  
  # due to low numbers of non-white participants, let's set ethnicity to binary
  hear$ethnicity_simple <- as.character(hear$ethnicity)
  hear$ethnicity_simple[hear$ethnicity != '1'] <- '2'
  hear$ethnicity_simple <- as.factor(hear$ethnicity_simple)
  
  # calculate follow-up
  hear$follow_up <- as.numeric(difftime(hear$censor_date, 
                                        hear$date_hear_loss_any, units = 'days'))/365.25
  
  saveRDS(hear, file = paste0('hearing_masterfile_', outcome, '_ITT.rds'))
}
rm(list = ls()); gc()