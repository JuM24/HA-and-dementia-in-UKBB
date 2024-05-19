# This code matches the two exposure groups, performs some checks about the quality of the matching, and estimates the effects.
# The default parameters below are for the main analysis; for sensitivity analyses,
# the following should be altered (see Suppl. material for which number refers
# to which analysis):

## analysis 2:
  # - addition of covariates `soc_isol_USE`, `mood_dis` to matching and effect estimation

## analysis 3:
  # - before matching, removal of participants with `early_cens == 1`

## analysis 4:
  # - re-run `4_clean_data.R` with `find_closest_non_missing_before_0` in all cases
  #   where `find_closest_non_missing` is used; that new `hearing_masterfile_ITT/PP` 
  #   is then used in this script instead of the default one

## analysis 5:
  # - addition of covariate `hear_loss_origin_simple` to matching and effect estimation

## analysis 6:
  # - addition of covariate `inpatient_contact_cat` to matching and effect estimation

## analysis 7:
  # - addition of covariates `inpatient_contact_cat`, `presc_contact`, `diag_contact` 
  #   to matching and effect estimation

## analysis 8
  # - replace the variable `dementia` with `flu`

## analysis 9
  # - replace the variable `dementia` with `hepatic`

## analysis 10
  # - replace the variable `dementia` with `heart`

## analysis 11
  # - replace the variable `dementia` with `respiratory`

## analysis 12
  # - replace the variable `dementia` with `asthma`

## analysis 13
  # - replace the variable `dementia` with `skin`

## analysis 14
  # - replace the variable `dementia` with `infect`


library(tidyverse)
library(MatchIt)
library(survey)

source('0_helper_functions.R')

# set file name
file_name <- 'hearing_masterfile_subsequent_healthcare_ITT.rds'

hear <- readRDS(file_name)

set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                 srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity, 
                 data = hear,
                 method = 'quick', distance = 'bart', estimand = 'ATE',
                 distance.options = list(seed = 6))
           
table(hear$hear_aid_any)
table(hear$dementia)


# extract matched data and check a few basic matching stats
md <- match.data(m.out)
model_summary <- summary(m.out, interactions = TRUE, un = TRUE)
plot(model_summary)


# further scrutinise matching by looking at density plots
plot(m.out, type = 'density', interactive = FALSE,
     which.xs = ~ hear_aid_any ~ age_USE + sex + education_USE + deprivation + 
       g_USE + srt_min_USE + data_provider_freq)

# approach to calculate CIs using `syvglm()`
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any + age_USE + education_USE + sex + deprivation + 
               g_USE + srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity,
              design = dsn, family = quasibinomial())
marginaleffects::comparisons(fit,
                                 variables = c('hear_aid_any'),
                                 wts = '(weights)',
                                 comparison = 'lnratioavg',
                                 transform = 'exp')

# survival curves
surv_weighted <- survival::survfit(survival::Surv(follow_up, dementia) ~ 
                                     hear_aid_any, data = md, weights = md$weights)
df_surv <- broom::tidy(surv_weighted)

ggplot(df_surv, aes(x = time, y = estimate, color = strata, linetype = strata)) +
  geom_step() +
  scale_color_brewer(palette='Set1') + 
  scale_fill_brewer(palette='Set1') +
  labs(x = 'Years', y = 'dementia-free') +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), limits = c(0.963, 1), breaks = seq(0.96, 1, by = 0.005)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 16.1), breaks = seq(0, 16, by = 1)) +
  theme(axis.text.x = element_text(face = "plain", size = 11, color = "grey15", angle=0),
        axis.title.x = element_text(face = "bold", size = 12.5, color = "grey15"),
        axis.text.y = element_text(face = "plain", size = 11, color = "grey15"),
        axis.title.y = element_text(face = "bold", size = 12.5, color = "grey15"), legend.position = 'none')

# HRs
fit_hr <- survival::coxph(survival::Surv(follow_up, dementia) ~ hear_aid_any + age_USE + education_USE + sex + 
                            deprivation + g_USE + srt_min_USE + data_provider_freq +
                            + tinnitus_sr_USE + ethnicity, robust = TRUE,
                          data = md, weights = weights)
summary(fit_hr)$coefficients['hear_aid_any', 2] # point estimate
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]








## Effect of emulated randomisation to HA on subsequent healthcare contact
## - re-run `4_clean_data.R`, but change the following:

# 1. run `filter(date > date_hear_loss_any & date < censor_date_any)`
# instead of `filter(date < date_hear_loss_any &
#               date > date_hear_loss_any-5*365.25)`

# 2. run `filter(event_dt > date_hear_loss_any & event_dt < cansor_date)`
# instead of `filter(event_dt < date_hear_loss_any & event_dt > date_hear_loss_any-5*365.25)`

# 3. do not run `gp_contact$gp_contact[gp_contact$change_to_na == 1] <- NA`

## keep only participants with at least one year of follow-up
hear_inpatient <- hear %>%
  filter(difftime(censor_date, date_hear_loss_any) >= 365.25)
hear_gp <- hear %>%
  filter(!is.na(gp_contact) & (censor_date_any - date_hear_loss_any) >= 365.25)

# average number of inpatient visits per year of follow-up
hear_inpatient$inpatient_contact_avg <- 
  hear_inpatient$inpatient_contact/hear_inpatient$follow_up

hear_gp$follow_up_any <- as.numeric(difftime(hear_gp$censor_date_any, 
                                             hear_gp$date_hear_loss_any, 
                                             units = 'days'))/365.25
hear_gp$gp_contact_avg <-
  hear_gp$gp_contact/hear_gp$follow_up_any

# categorisation of inpatient visits
hear_inpatient$inpatient_contact_cat <- NA
hear_inpatient$inpatient_contact_cat[hear_inpatient$inpatient_contact_avg == 0] <- '0'
hear_inpatient$inpatient_contact_cat[hear_inpatient$inpatient_contact_avg > 0 & 
                             hear_inpatient$inpatient_contact_avg <= 0.25] <- '1'
hear_inpatient$inpatient_contact_cat[hear_inpatient$inpatient_contact_avg > 0.25 & 
                           hear_inpatient$inpatient_contact_avg <= 0.5] <- '2'
hear_inpatient$inpatient_contact_cat[hear_inpatient$inpatient_contact_avg > 0.5 & 
                           hear_inpatient$inpatient_contact_avg <= 1] <- '3'
hear_inpatient$inpatient_contact_cat[hear_inpatient$inpatient_contact_avg > 1] <- '4'


hear$inpatient_contact_cat <- as.factor(hear$inpatient_contact_cat)

hear_gp$gp_contact_cat <- NA
hear_gp$gp_contact_cat[hear_gp$gp_contact_avg >= 0 & 
                           hear_gp$gp_contact_avg <= 3] <- '0'
hear_gp$gp_contact_cat[hear_gp$gp_contact_avg > 3 & 
                           hear_gp$gp_contact_avg <= 12] <- '1'
hear_gp$gp_contact_cat[hear_gp$gp_contact_avg > 12 & 
                           hear_gp$gp_contact_avg <= 24] <- '3'
hear_gp$gp_contact_cat[hear_gp$gp_contact_avg > 24] <- '4'
hear_gp$gp_contact_cat <- as.factor(hear_gp$gp_contact_cat)
hear_gp$gp_contact_cat_binary <- 0
hear_gp$gp_contact_cat_binary[hear_gp$gp_contact_cat != '0'] <- 1
table(hear_gp$gp_contact_cat)


# comparisons of each group with zero group for inpatient contact
m.inpatient <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                         srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity, 
                       data = hear_inpatient,
                       method = 'quick', distance = 'bart', estimand = 'ATE',
                       distance.options = list(seed = 6))
md_inpatient <- match.data(m.inpatient)

for (group in as.character(seq(1, 4))){
  subset <- filter(md_inpatient, inpatient_contact_cat %in% c('0', group))
  subset$inpatient_contact_cat <- as.factor(as.character(subset$inpatient_contact_cat))
  dsn = svydesign(ids =~subclass, weights=~weights, data=subset)
  fit = svyglm(inpatient_contact_cat ~ hear_aid_any + age_USE + education_USE + sex + deprivation + 
                 g_USE + srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity,
               design = dsn, family = quasibinomial())
  subset_comparison <- marginaleffects::comparisons(fit,
                                                    variables = c('hear_aid_any'),
                                                    wts = '(weights)',
                                                    comparison = 'lnratioavg',
                                                    transform = 'exp')
  print(subset_comparison)
}


# comparisons of each group with zero group for GP contact
m.gp <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                         srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity, 
                       data = hear_inpatient,
                       method = 'quick', distance = 'bart', estimand = 'ATE',
                       distance.options = list(seed = 6))
md_gp <- match.data(m.gp)

for (group in as.character(seq(1, 4))){
  subset <- filter(md_gp, gp_contact_cat %in% c('0', group))
  subset$gp_contact_cat <- as.factor(as.character(subset$gp_contact_cat))
  dsn = svydesign(ids =~subclass, weights=~weights, data=subset)
  fit = svyglm(gp_contact_cat ~ hear_aid_any + age_USE + education_USE + sex + deprivation + 
                 g_USE + srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity,
               design = dsn, family = quasibinomial())
  subset_comparison <- marginaleffects::comparisons(fit,
                                                    variables = c('hear_aid_any'),
                                                    wts = '(weights)',
                                                    comparison = 'lnratioavg',
                                                    transform = 'exp')
  print(subset_comparison)
}



## TODO: Effect of pre-randomisation healthcare contact on dementia

## primary care
hear <- hear %>%
  # remove participants with GP censoring before time 0
  filter(!is.na(gp_contact) & censor_date_gp >= date_hear_loss_any)

# determine date one year before time zero
hear$start_gp <- hear$date_hear_loss_any - 365






## Effect of confounders on dementia
hear$prop_score <- scale(m.out$distance)[,1]
fit <- glm(dementia ~ prop_score + hear_aid_any,
           data = hear, family = binomial())
summary(fit)

cov_robust <- sandwich::vcovHC(fit, type = 'HC3')
se <- sqrt(cov_robust[2,2])
critical_value <- qt(0.975, df = df.residual(fit))
lower_bound <- exp(coef(fit)[2] - critical_value * se)
upper_bound <- exp(coef(fit)[2] + critical_value * se)


dsn = svydesign(ids =~subclass, weights=~1, data=md)
fit = svyglm(dementia ~ distance + hear_aid_any,
             design = dsn, family = quasibinomial())

marginaleffects::comparisons(fit,
                             variables = 'prop_score',
                             wts = NULL,
                             comparison = 'lnratioavg',
                             transform = 'exp')




## Analysis on imputed dataset (as opposed to removing missing values)
library(future)

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



# before determining the assessment to use, set dummy values to NAs
# for participants that partook in the assessments (only those without
# assessments are "truly" missing)

# get info on SiN administration
data_all <- readRDS('main_vars.Rds') %>%
  select(eid, starts_with(c('X4268.', 'X4275.'))) %>%
  rename(id = eid)

hear <- merge(hear, data_all, by = 'id', all.x = TRUE)


# education, g, srt_min, tinnitus_sr
suffixes <- c('_0', '_1', '_2', '_3')
variables <- c('education', 'g', 'tinnitus_sr') 

# if they participated at the assessment, but have NA, then they will be imputed
for (suf in suffixes){
  for (var in variables){
    var_suf <- paste0(var, suf)
    date_suf <- paste0('date', suf)
    hear[(!is.na(hear[[date_suf]]) & is.na(hear[[var_suf]])), var_suf] <- 999
  }
}

# the SiN is a special case, since not everybody was given the test
# those that were given the test but refused or did not complete it,
# will get a dummy value
hear$srt_min_0[hear$X4268.0.0 %in% c(-1, 9) & hear$X4275.0.0 %in% c(-1, 9)] <- 999
hear$srt_min_1[hear$X4268.1.0 %in% c(-1, 9) & hear$X4275.1.0 %in% c(-1, 9)] <- 999
hear$srt_min_2[hear$X4268.2.0 %in% c(-1, 9) & hear$X4275.2.0 %in% c(-1, 9)] <- 999
hear$srt_min_3[hear$X4268.3.0 %in% c(-1, 9) & hear$X4275.3.0 %in% c(-1, 9)] <- 999

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

# just keep individuals with assessment data <=5 years removed from the date of HL
hear <- hear %>% 
  filter(min_diff_education <= 5*365.25 & 
           min_diff_g <= 5*365.25 & 
           min_diff_srt_min <= 5*365.25 &
           min_diff_tinnitus_sr <= 5*365.25)
hear[hear == 999] <- NA

# calculate follow-up
hear$follow_up <- as.numeric(difftime(hear$censor_date, 
                                      hear$date_hear_loss_any, units = 'days'))/365.25


# some lifestyle factors used for imputation
other_predictors <- readRDS('D://Job/Projects/Pseudo-scales/pseudoscales GitHub/test_folder_pseudoscales/output_files/covariates.Rds') %>%
  select(id, alc_freq_0, smoking_0, phys_act_0, waist_0)
hear <- merge(hear, other_predictors, by = 'id', all.x = TRUE)

# subset and choose the variables to use for imputation
subsample <- hear %>%
  # remove those that were not given the SiN or did not participate
  select(id, age_USE, sex, education_USE, deprivation, g_USE, srt_min_USE,
         data_provider_freq, tinnitus_sr_USE, ethnicity, data_provider_freq,
         alc_freq_0, smoking_0, phys_act_0, waist_0, hear_aid_any,
         follow_up, dementia)
# change rownames to be able to later merge back
rownames(subsample) <- subsample$id
subsample$id <- NULL

# to factors
subsample <- subsample %>% 
  mutate(across(c(sex, education_USE, data_provider_freq, ethnicity, 
                  tinnitus_sr_USE, dementia), as.factor))
subsample$dementia <- as.numeric(subsample$dementia)


## perform MICE
perc_missing_0 <- 100*sum(is.na(subsample))/(nrow(subsample)*ncol(subsample))
plan(multicore, workers = 15)
options(future.globals.maxSize= 1048576000)
imp <- mice::futuremice(subsample, 
                        m = ifelse(perc_missing_0 < 10, 10, perc_missing_0),
                        parallelseed = 24,
                        method = 'rf', 
                        maxit = 20)


## match the imputed datasets
library(MatchThem)
covariates = c('age_USE', 'sex', 'education_USE', 
               'deprivation', 'g_USE', 'srt_min_USE', 
               'data_provider_freq', 'tinnitus_sr_USE',
               'ethnicity') 
formula <- as.formula(paste0('hear_aid_any ~ ', paste(covariates, collapse = '+')))
m.out <- matchthem(datasets = imp, 
                   formula = formula,
                   method = 'quick', 
                   distance = 'bart', 
                   estimand = 'ATE',
                   distance.options = list(seed = 6))


## check matching quality
cobalt::love.plot(m.out, stars = 'raw')



## run the analysis on each imputed dataset
fit_glm <- with(data = m.out, svyglm(dementia ~ hear_aid_any * (age_USE + education_USE + sex + deprivation + 
                                                                  g_USE + srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity), 
                                     family = quasibinomial()), cluster = TRUE)

marginaleffects::avg_comparisons(fit_glm,
                                 vcov = 'HC3',
                                 variables = c('hear_aid_any'),
                                 wts = '(weights)',
                                 comparison = 'lnratioavg')
# INSERT CODE FROM LOCAL IMPUTATION





## Analysis of effects of potential confounding
# The following attempts to answer how big confounding would there have to be 
# for it to flip the effect to that observed in our study? The below code assumes 
# that there exist binary (for simplicity of interpretation) unmeasured confounders 
# that biased our results and are the cause of the deviation from the true value.

# The code uses the HR observed in our study ("effect_observed") and allows you 
# to also set the presumed prevalence of the supposed unmeasured confounder in 
# the exposed ("exposed_confounder_prev") and in the unexposed ("exposed_confounder_prev"). 
# It also allows you to set the presumed total effect of the confounder 
# ("confounder_outcome_effect"). With these four values, the function 
# `adjust_hr_with_binary` estimates for the given prevalences and strength of 
# confounding, the magnitude of the true HR.

# A simulation is run where in each iteration, the prevalence of the confounder 
# in the exposed and the effect of the confounder a randomly sample from 
# [0.1, 1] and [1, 5], respectively. The resulting data frame is then reduced 
# to just those estimated HRs that fall within a pre-specified window. 
# This window is either [0.75, 0.86], where (a) we assume that previous studies 
# on the association between use of HA and dementia are correct in estimating 
# and HR of ~0.81 (10.1001/jamaneurol.2022.4427), or (b), we assume that the true 
# HR is 0 and the window is set to [0.95, 1.05].

# For each of the two scenarios (given a prevalence of the confounder in 
# the unexposed), the new data frame will contain a range of prevalences of 
# the confounder in the exposed and effect sizes (HRs) of the confounder 
# that would allow for a true effect within the pre-specified window.


library(tipr)
# constant values for the simulation
estimate_low <- 0.95
estimate_high <- 1.05
effect_observed <- 1.55
unexposed_confounder_prev <- 0.1
new_effect <- data.frame(matrix(ncol = 4, nrow = 0)); colnames(new_effect) <- 
  c('effect_observed', 'exposed_confounder_prev', 
    'unexposed_confounder_prev', 'confounder_outcome_effect')


# varying values
set.seed(24)
for (i in seq(1, 100000)){
  exposed_confounder_prev <- runif(n=1, min=0.1, max=1)
  confounder_outcome_effect <- runif(n=1, min=1, max=20)
  
  new_effect <- rbind(new_effect, adjust_hr_with_binary(effect_observed = effect_observed, 
                                                        exposed_confounder_prev = exposed_confounder_prev, 
                                                        unexposed_confounder_prev = unexposed_confounder_prev, 
                                                        confounder_outcome_effect = confounder_outcome_effect,
                                                        verbose = FALSE))
}

# reduce to assumed range
new_effect <- filter(new_effect, hr_adjusted > estimate_low & hr_adjusted < estimate_high)
min(new_effect$exposed_confounder_prev); min(new_effect$confounder_outcome_effect)

# plot possible prevalence in the exposed and confounders effects in the assumed range
plot(new_effect$exposed_confounder_prev, new_effect$confounder_outcome_effect)
