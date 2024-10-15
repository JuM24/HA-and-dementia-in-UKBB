# This code matches the two exposure groups, performs some checks about the 
# quality of the matching, and estimates the effects.
# The default parameters below are for the main analysis; for sensitivity analyses,
# some aspects need to be altered; (see Suppl. material for which number refers
# to which analysis):

## analysis 2:
# - use the per-protocol dataset

## analysis 3:
  # - before matching, removal of participants with `early_cens == 1`

## analysis 4:
# - re-run `4_clean_data.R` with `find_closest_non_missing_before_0` in all cases
#   where `find_closest_non_missing` is used; that new `hearing_masterfile_ITT/PP` 
#   is then used in this script instead of the default one

## analysis 5:
# - different data cleaning to prepare for analysis using imputed data;
#   SEE SCRIPT BELOW

## analysis 6:
# - addition of covariates `soc_isol_USE`, `mood_dis` to matching and effect estimation

## analysis 7:
# - addition of covariate `head_inj` to matching and effect estimation

## analysis 8:
  # - addition of covariate `hear_loss_origin_simple` to matching and effect estimation

## analysis 9:
  # - addition of covariate `inpatient_contact_cat` to matching and effect estimation

## analysis 10:
  # - removal of participants without primary care data (NAs for `gp_contact_cat`)
  # - addition of covariate `gp_contact_cat` to matching and effect estimation

## analysis 11:
# - removal of participants without primary care data (NAs for `gp_contact_cat`)
# - addition of covariates `inpatient_contact_cat` and `gp_contact_cat` 
#   to matching and effect estimation

## analysis 12
# - import the dataset cleaned to analyse flu
# - replace the variable `dementia` with `flu`

## analysis 13
# - import the dataset cleaned to analyse hepatic disorders
# - replace the variable `dementia` with `hepatic`

## analysis 14
# - import the dataset cleaned to analyse respiratory disorders
# - replace the variable `dementia` with `respiratory`

## analysis 15
# - import the dataset cleaned to analyse asthma
# - replace the variable `dementia` with `asthma`

## analysis 16
# - import the dataset cleaned to analyse cutaneous disorders
# - replace the variable `dementia` with `skin`

## analysis 17
# - import the dataset cleaned to analyse infectious disorders
# - replace the variable `dementia` with `infect`

## analysis 18
# - import the dataset cleaned to analyse appendicitis
# - replace the variable `dementia` with `appendicitis`

## analysis 19
# - import the dataset cleaned to analyse hip fractures
# - replace the variable `dementia` with `hip_fract`

## analysis 20
# - import the dataset cleaned to analyse transport accidents
# - replace the variable `dementia` with `trans_acc`

## SEE BELOW for other sensitivity analyses






### 1. Main analysis for dementia using intention-to-treat ###

library(tidyverse)
library(MatchIt)
library(WeightIt)
library(survey)

source('0_helper_functions.R')

# set file name
file_name <- 'hearing_masterfile_ITT.rds'

hear <- readRDS(file_name)

set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                 srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple, 
                 data = hear,
                 method = 'quick', distance = 'bart', estimand = 'ATE',
                 distance.options = list(seed = 6))

table(hear$hear_aid_any)
table(hear$dementia)

# extract matched data and check a few basic matching stats
md <- match.data(m.out)
cobalt::love.plot(m.out, stars = 'raw')


# further scrutinise matching by looking at density plots
plot(m.out, type = 'density', interactive = FALSE,
     which.xs = ~ hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
       srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple)

# approach to calculate robust CIs using `syvglm()`
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any * 
               (age_USE + education_USE + sex + deprivation + g_USE + srt_min_USE + 
                  data_provider_freq + tinnitus_sr_USE + ethnicity_simple),
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
fit_hr <- survival::coxph(survival::Surv(follow_up, dementia) ~ hear_aid_any, robust = TRUE,
                          data = md, weights = weights, cluster = subclass)
summary(fit_hr)$coefficients['hear_aid_any', 2] # point estimate
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]













### ANALYSIS 5 ###

## Analysis on imputed dataset (as opposed to complete-case analysis)
library(future)

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


# Remove those with dementia before or at the same date as hearing loss.
hear <- filter(hear, dementia == 0 | (date_hear_loss_any < dementia_date))

# eemove those without dates of hearing loss or hearing aid because it 
# prevents us from ascertaining the timeline
no_aid_date <- filter(hear, hear_aid_any == 0 & hear_aid_nodate == 1)
hear <- filter(hear, !id %in% no_aid_date$id) %>%
  filter(hear_loss_any == 0 | !is.na(date_hear_loss_any))

# select only those that were diagnosed with HL before getting HA
hear <- filter(hear, hear_aid_any == 0 | (date_hear_loss_any <= date_hear_aid_any))



# before determining the assessment to use, set dummy values to NAs
# for participants that partook in the assessments (only those that 
# assessments are "truly" missing)
# education, g, srt_min, tinnitus_sr: these variables were measured during
# assessments, so they have dates associated with them, but no variable values
# in case they are missing;
suffixes <- c('_0', '_1', '_2', '_3')
variables <- c('education', 'g', 'tinnitus_sr') 

# since the default function uses non-missing covariate measurement dates 
# that occur as close as possible to time 0, we must use a dummy values so that
# dates with missing values do not get discarded
for (suf in suffixes){
  for (var in variables){
    var_suf <- paste0(var, suf)
    date_suf <- paste0('date', suf)
    hear[(!is.na(hear[[date_suf]]) & is.na(hear[[var_suf]])), var_suf] <- 999
  }
}

# the SiN is a special case, since not everybody was given the test
# those that were given the test but refused or did not complete it,
# will get the dummy value as well
hear$srt_min_0[!is.na(hear$hear_test_0) & is.na(hear$srt_min_0)] <- 999
hear$srt_min_1[!is.na(hear$hear_test_1) & is.na(hear$srt_min_1)] <- 999
hear$srt_min_2[!is.na(hear$hear_test_2) & is.na(hear$srt_min_2)] <- 999
hear$srt_min_3[!is.na(hear$hear_test_3) & is.na(hear$srt_min_3)] <- 999

# determine the assessment that is closest to HL start and which will be used for covariate measurement 
hear <- find_closest_non_missing(hear, 'education', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'g', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'srt_min', date_hear_loss_any = 'date_hear_loss_any')
hear <- find_closest_non_missing(hear, 'tinnitus_sr', date_hear_loss_any = 'date_hear_loss_any')

# those without censoring date get the value '0' for data provider value
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

# set the censoring dates to the date that occurs earliest
hear <- hear %>%
  mutate(censor_date = reduce(across(c('dementia_date', 'death_date', 'follow_loss_date',
                                       'data_cens_date')), pmin, na.rm = TRUE))

# for those that are labelled as having experienced the outcome after the 
# censoring date, set the outcome to 0
hear$dementia[hear$censor_date < hear$dementia_date] <- 0
hear$death[hear$censor_date < hear$death_date] <- 0

# some will have experienced HL only after the censoring date; remove those
hear <- filter(hear, date_hear_loss_any < censor_date)

# age at HL
hear$age_USE <- as.numeric(difftime(hear$date_hear_loss_any, 
                                    hear$birth_date, units = 'days'))/365.25

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

# set dummy value back to NA
hear[hear == 999] <- NA

# calculate follow-up
hear$follow_up <- as.numeric(difftime(hear$censor_date, 
                                      hear$date_hear_loss_any, units = 'days'))/365.25

# due to low numbers of non-white participants, let's set ethnicity to binary
hear$ethnicity_simple <- as.character(hear$ethnicity)
hear$ethnicity_simple[hear$ethnicity != '1'] <- '2'
hear$ethnicity_simple <- as.factor(hear$ethnicity_simple)




## some lifestyle factors used for imputation
other_predictors <- readRDS('main_vars.Rds') %>%
  select(c(eid, starts_with(c('X1558.', 'X6164.', 'X20116.', 'X48.'))))

# alcohol
alcohol <- other_predictors %>%
  select(eid, X1558.0.0) %>%
  rename(id = eid, alc_freq_0 = X1558.0.0)
alcohol[alcohol == -3] <- NA

# level of physical activity in the past 4 weeks:
phys_act <- other_predictors %>%
  select(eid, X6164.0.0) %>%
  mutate(phys_act_0 = apply(select(., X6164.0.0), 1, 
                            function(x) phys_act_classify(x))) %>%
  rename(id = eid) %>%
  select(id, phys_act_0)

# smoking
smoking <- other_predictors %>%
  select(eid, X20116.0.0)
colnames(smoking) <- c('id', 'smoking_0')
smoking[smoking == -3] <- NA

# waist circumference
waist <- other_predictors %>% 
  select(eid, X48.0.0) %>%
  rename(id = eid, waist_0 = 'X48.0.0')

other_predictors <- Reduce(function(x, y) merge(x, y, by = 'id', all = TRUE), 
                     list(alcohol, phys_act, smoking, waist))

# merge the auxiliary variables with the master data frame
hear <- merge(hear, other_predictors, by = 'id', all.x = TRUE)

# subset and choose the variables to use for imputation
subsample <- hear %>%
  # remove those that were not given the SiN or did not participate
  select(id, age_USE, sex, education_USE, deprivation, g_USE, srt_min_USE,
         data_provider_freq, tinnitus_sr_USE, ethnicity_simple,
         alc_freq_0, smoking_0, phys_act_0, waist_0, hear_aid_any,
         follow_up, dementia)
# change rownames to be able to later merge back
rownames(subsample) <- subsample$id
subsample$id <- NULL

# change type if necessary
subsample <- subsample %>% 
  mutate(across(c(sex, education_USE, data_provider_freq, ethnicity_simple, 
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
               'ethnicity_simple') 
formula <- as.formula(paste0('hear_aid_any ~ ', paste(covariates, collapse = '+')))
set.seed(6)
m.out <- matchthem(datasets = imp, 
                   formula = formula,
                   method = 'quick', 
                   distance = 'bart', 
                   estimand = 'ATE',
                   distance.options = list(seed = 6))


## check matching quality
cobalt::love.plot(m.out, stars = 'raw')



## run the analysis on each imputed dataset
fit_glm <- with(data = m.out, 
                svyglm(as.factor(dementia) ~ hear_aid_any * (age_USE + sex + education_USE + deprivation + 
                                                               g_USE + srt_min_USE + data_provider_freq + 
                                                               tinnitus_sr_USE + ethnicity_simple), 
                       family = quasibinomial()), cluster = TRUE)
marginaleffects::avg_comparisons(fit_glm,
                                 vcov = 'HC3',
                                 variables = c('hear_aid_any'),
                                 wts = '(weights)',
                                 comparison = 'lnratioavg')

fit_hr <- with(data = m.out, 
               survival::coxph(survival::Surv(follow_up, dementia) ~ 
                                 hear_aid_any, robust = TRUE), cluster = TRUE)
summary(pool(fit_hr), conf.int = TRUE)


# approach to produce survival plot of estimations based on imputed data
results_list <- lapply(complete(m.out, 'all'), analyse_imputed_data)
combined_results <- bind_rows(results_list, .id = 'imputation')

combined_results <- combined_results %>%
  group_by(time, strata) %>%
  summarize(
    Q_bar = mean(cloglog, na.rm = TRUE),
    U_bar = mean((1 / (log(1 - estimate)) * (1 - estimate))^2 * std.error^2, na.rm = TRUE),
    B = var(cloglog, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    T = U_bar + (1 + 1 / 5) * B,  # 5 = M - adjust accordingly
    CI_lower = Q_bar - 1.96 * sqrt(T),
    CI_upper = Q_bar + 1.96 * sqrt(T),
    pooled_surv = 1 - exp(-exp(Q_bar)),
    CI95_lower = 1 - exp(-exp(CI_lower)),
    CI95_upper = 1 - exp(-exp(CI_upper))
  )

ggplot(combined_results, aes(x = time, y = pooled_surv, color = strata, linetype = strata)) +
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









## Effect of emulated randomisation to HA on subsequent healthcare contact
# read in correct file
hear_inpatient <- readRDS('hearing_masterfile_subsequent_healthcare_ITT.rds')

hear_gp <- hear_inpatient %>%
  filter(!is.na(gp_contact_cat))

# comparisons of each group with zero group for inpatient contact
set.seed(6)
m.inpatient <- matchit(formula = hear_aid_any ~ age_USE + sex + education_USE + 
                         deprivation + g_USE + srt_min_USE + data_provider_freq + 
                         tinnitus_sr_USE + ethnicity_simple, 
                       data = hear_inpatient,
                       method = 'quick', distance = 'nnet', estimand = 'ATE',
                       distance.options = list(size = 250, maxit = 200, MaxNWts = 8000, seed = 6))
md_inpatient <- match.data(m.inpatient)

dsn = svydesign(ids =~subclass, weights=~weights, data=md_inpatient)

for (group in as.character(seq(2, 4))){
  subset <- filter(md_inpatient, inpatient_contact_cat %in% c('1', group))
  subset$inpatient_contact_cat <- as.factor(as.character(subset$inpatient_contact_cat))
  dsn = svydesign(ids =~subclass, weights=~weights, data=subset)

  fit = svyglm(inpatient_contact_cat ~ hear_aid_any * (age_USE + education_USE + sex + deprivation + 
                 g_USE + srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple),
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
                         srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple, 
                       data = hear_gp,
                       method = 'quick', distance = 'bart', estimand = 'ATE',
                       distance.options = list(seed = 6))
md_gp <- match.data(m.gp)

for (group in as.character(seq(2, 3))){
  subset <- filter(md_gp, gp_contact_cat %in% c('1', group))
  subset$gp_contact_cat <- as.factor(as.character(subset$gp_contact_cat))
  dsn = svydesign(ids =~subclass, weights=~weights, data=subset)
  fit = svyglm(gp_contact_cat ~ hear_aid_any + age_USE + education_USE + sex + deprivation + 
                 g_USE + srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple,
               design = dsn, family = quasibinomial())
  subset_comparison <- marginaleffects::comparisons(fit,
                                                    variables = c('hear_aid_any'),
                                                    wts = '(weights)',
                                                    comparison = 'lnratioavg',
                                                    transform = 'exp')
  print(subset_comparison)
}







### Effect of pre-randomisation healthcare contact on dementia

hear <- readRDS(file_name)

## inpatient hospital
# estimate the weights
w_inpatient <- weightit(inpatient_contact_cat ~ age_USE + sex + education_USE + 
                          deprivation + g_USE + srt_min_USE + data_provider_freq + 
                          tinnitus_sr_USE + ethnicity_simple, 
                        data = hear, 
                        method = 'glm', 
                        estimand = 'ATE')
# fit the model
fit_inpatient <- glm_weightit(dementia ~ inpatient_contact_cat + age_USE + sex + education_USE + 
                                deprivation + g_USE + srt_min_USE + data_provider_freq + 
                                tinnitus_sr_USE + ethnicity_simple,
                              data = hear, 
                              weightit = w_inpatient, 
                              family = 'binomial', 
                              vcov = 'HC0')
# estimate the effect
est_inpatient <- marginaleffects::avg_comparisons(fit_inpatient,
                                                  comparison = 'lnratioavg',
                                                  transform = 'exp',
                                                  variables = 'inpatient_contact_cat')

## primary care
hear_gp <- hear %>%
  # remove participants with GP censoring before time 0
  filter(!is.na(gp_contact_cat))

w_gp <- weightit(gp_contact_cat ~ age_USE + sex + education_USE + 
                   deprivation + g_USE + srt_min_USE + data_provider_freq + 
                   tinnitus_sr_USE + ethnicity_simple, 
                 data = hear_gp, 
                 method = 'cbps', 
                 estimand = 'ATE')
# fit the model
fit_gp <- glm_weightit(dementia ~ gp_contact_cat + age_USE + sex + education_USE + 
                         deprivation + g_USE + srt_min_USE + data_provider_freq + 
                         tinnitus_sr_USE + ethnicity_simple,
                       data = hear_gp, 
                       weightit = w_gp, 
                       family = 'binomial', 
                       vcov = 'HC0')
# estimate the effect
est_gp <- marginaleffects::avg_comparisons(fit_gp,
                                       comparison = 'lnratioavg',
                                       transform = 'exp',
                                       variables = 'gp_contact_cat')





## Effect of confounders on dementia
hear$prop_score <- scale(m.out$distance)[, 1]
fit <- glm(dementia ~ prop_score + hear_aid_any,
           data = hear, family = binomial())
summary(fit)

cov_robust <- sandwich::vcovHC(fit, type = 'HC3')
se <- sqrt(cov_robust[2,2])
critical_value <- qt(0.975, df = df.residual(fit))
lower_bound <- exp(coef(fit)[2] - critical_value * se)
upper_bound <- exp(coef(fit)[2] + critical_value * se)



## GP subset with vanilla adjustment (i.e., no healthcare adjustment)
file_name <- 'hearing_masterfile_ITT.rds'
hear <- readRDS(file_name) %>%
  filter(!is.na(gp_contact_cat))
set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                   srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple, 
                 data = hear,
                 method = 'quick', distance = 'cbps', estimand = 'ATE',
                 distance.options = list(seed = 6))
md <- match.data(m.out)
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any * 
               (age_USE + education_USE + sex + deprivation + g_USE + srt_min_USE + 
                  data_provider_freq + tinnitus_sr_USE + ethnicity_simple),
             design = dsn, family = quasibinomial())
marginaleffects::comparisons(fit,
                             variables = c('hear_aid_any'),
                             wts = '(weights)',
                             comparison = 'lnratioavg',
                             transform = 'exp')
fit_hr <- survival::coxph(survival::Surv(follow_up, dementia) ~ hear_aid_any, robust = TRUE,
                          data = md, weights = weights, cluster = subclass)
summary(fit_hr)$coefficients['hear_aid_any', 2]
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]








## Repeat of vanilla- and healthcare-adjusted analyses while retaining pre-baseline HL

# vanilla analysis whole sample
file_name <- 'hearing_masterfile_ITT_ALT.rds'
hear <- readRDS(file_name)
set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                   srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple, 
                 data = hear,
                 method = 'quick', distance = 'bart', estimand = 'ATE',
                 distance.options = list(seed = 6))
md <- match.data(m.out)
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any * 
               (age_USE + education_USE + sex + deprivation + g_USE + srt_min_USE + 
                  data_provider_freq + tinnitus_sr_USE + ethnicity_simple),
             design = dsn, family = quasibinomial())
marginaleffects::comparisons(fit,
                             variables = c('hear_aid_any'),
                             wts = '(weights)',
                             comparison = 'lnratioavg',
                             transform = 'exp')
fit_hr <- survival::coxph(survival::Surv(follow_up, dementia) ~ hear_aid_any, robust = TRUE,
                          data = md, weights = weights, cluster = subclass)
summary(fit_hr)$coefficients['hear_aid_any', 2]
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]

# vanilla analysis GP subsample
file_name <- 'hearing_masterfile_ITT_ALT.rds'
hear <- readRDS(file_name) %>%
  filter(!is.na(gp_contact_cat))
set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                   srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple, 
                 data = hear,
                 method = 'quick', distance = 'bart', estimand = 'ATE',
                 distance.options = list(seed = 6))
md <- match.data(m.out)
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any * 
               (age_USE + education_USE + sex + deprivation + g_USE + srt_min_USE + 
                  data_provider_freq + tinnitus_sr_USE + ethnicity_simple),
             design = dsn, family = quasibinomial())
marginaleffects::comparisons(fit,
                             variables = c('hear_aid_any'),
                             wts = '(weights)',
                             comparison = 'lnratioavg',
                             transform = 'exp')
fit_hr <- survival::coxph(survival::Surv(follow_up, dementia) ~ hear_aid_any, robust = TRUE,
                          data = md, weights = weights, cluster = subclass)
summary(fit_hr)$coefficients['hear_aid_any', 2]
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]

# primary healthcare adjustment
file_name <- 'hearing_masterfile_ITT_ALT.rds'
hear <- readRDS(file_name) %>%
  filter(!is.na(gp_contact_cat))
set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                   srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple + 
                   gp_contact_cat, 
                 data = hear,
                 method = 'quick', distance = 'bart', estimand = 'ATE',
                 distance.options = list(seed = 6))
md <- match.data(m.out)
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any * 
               (age_USE + education_USE + sex + deprivation + g_USE + srt_min_USE + 
                  data_provider_freq + tinnitus_sr_USE + ethnicity_simple + 
                  gp_contact_cat),
             design = dsn, family = quasibinomial())
marginaleffects::comparisons(fit,
                             variables = c('hear_aid_any'),
                             wts = '(weights)',
                             comparison = 'lnratioavg',
                             transform = 'exp')
fit_hr <- survival::coxph(survival::Surv(follow_up, dementia) ~ hear_aid_any, robust = TRUE,
                          data = md, weights = weights, cluster = subclass)
summary(fit_hr)$coefficients['hear_aid_any', 2]
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]

# primary and secondary healthcare adjustment
file_name <- 'hearing_masterfile_ITT_ALT.rds'
hear <- readRDS(file_name) %>%
  filter(!is.na(gp_contact_cat))
set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                   srt_min_USE + data_provider_freq + tinnitus_sr_USE + ethnicity_simple + 
                   gp_contact_cat + inpatient_contact_cat, 
                 data = hear,
                 method = 'quick', distance = 'bart', estimand = 'ATE',
                 distance.options = list(seed = 6))
md <- match.data(m.out)
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any * 
               (age_USE + education_USE + sex + deprivation + g_USE + srt_min_USE + 
                  data_provider_freq + tinnitus_sr_USE + ethnicity_simple + 
                  gp_contact_cat + inpatient_contact_cat),
             design = dsn, family = quasibinomial())
marginaleffects::comparisons(fit,
                             variables = c('hear_aid_any'),
                             wts = '(weights)',
                             comparison = 'lnratioavg',
                             transform = 'exp')
fit_hr <- survival::coxph(survival::Surv(follow_up, dementia) ~ hear_aid_any, robust = TRUE,
                          data = md, weights = weights, cluster = subclass)
summary(fit_hr)$coefficients['hear_aid_any', 2]
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]