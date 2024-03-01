# This code matches the two exposure groups, performs some checks about the quality of the matching, and estimates the effects.

library(tidyverse)
library(MatchIt)
library(survey)

source('0_helper_functions.R')
setwd('files/')

hear <- readRDS('hearing_masterfile_ITT.rds')

set.seed(6)
m.out <- matchit(hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE +
                 srt_min_USE + data_provider_freq, data = hear,
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
     which.xs = ~ hear_aid_any ~ age_USE + sex + education_USE + deprivation + g_USE + srt_min_USE + data_provider_freq)

# approach to calculate CIs using `syvglm()`
dsn = svydesign(ids =~subclass, weights=~weights, data=md)
fit = svyglm(dementia ~ hear_aid_any + age_USE + education_USE + sex + deprivation + g_USE + srt_min_USE + data_provider_freq,
              design = dsn, family = quasibinomial())
marginaleffects::comparisons(fit,
                                 variables = c('hear_aid_any'),
                                 vcov = TRUE,
                                 wts = '(weights)',
                                 comparison = 'lnratioavg',
                                 transform = 'exp')

# survival curves
surv_weighted <- survival::survfit(survival::Surv(follow_up, dementia) ~ hear_aid_any, data = md, weights = md$weights)
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
                            deprivation + g_USE + srt_min_USE + data_provider_freq, 
                          data = md, weights = weights)
summary(fit_hr)$coefficients['hear_aid_any', 2] # point estimate
broom::tidy(fit_hr, exponentiate = T, conf.int = T)[1,]




## Analysis of effects of potential confounding
# The following attempts to answer how big confounding would there have to be for it to flip the effect to that observed in our study?
# The below code assumes that there exist binary (for simplicity of interpretation) unmeasured confounders that biased our results
# and are the cause of the deviation from the true value.

# The code uses the HR observed in our study ("effect_observed") and allows you to also set the presumed prevalence of 
# the supposed unmeasured confounder in the exposed ("exposed_confounder_prev") and in the unexposed ("exposed_confounder_prev"). 
# It also allows you to set the presumed total effect of the confounder ("confounder_outcome_effect"). With these four values,
# the function `adjust_hr_with_binary` estimates for the given prevalences and strength of confounding, the magnitude of the true HR.

# A simulation is run where in each iteration, the prevalence of the confounder in the exposed and the effect of the confounder
# a randomly sample from [0.1, 1] and [1, 5], respectively. The resulting data frame is then reduced to just those estimated HRs
# that fall within a pre-specified window. This window is either [0.75, 0.86], where (a) we assume that previous studies on the
# association between use of HA and dementia are correct in estimating and HR of ~0.81 (10.1001/jamaneurol.2022.4427),
# or (b), we assume that the true HR is 0 and the window is set to [0.95, 1.05].

# For each of the two scenarios (given a prevalence of the confounder in the unexposed), the new data frame will contain a range
# of prevalences of the confounder in the exposed and effect sizes (HRs) of the confounder that would allow for a true effect
# within the pre-specified window.


library(tipr)
# constant values for the simulation
estimate_low <- 0.95
estimate_high <- 1.05
effect_observed <- 1.55
unexposed_confounder_prev <- 0.1
new_effect <- data.frame(matrix(ncol = 4, nrow = 0)); colnames(new_effect) <- c('effect_observed', 'exposed_confounder_prev', 
                                                                                'unexposed_confounder_prev', 'confounder_outcome_effect')


# varying values
set.seed(24)
for (i in seq(1, 100000)){
  exposed_confounder_prev <- runif(n=1, min=0.1, max=1)
  confounder_outcome_effect <- runif(n=1, min=1, max=20)
  
  new_effect <- rbind(new_effect, adjust_hr_with_binary(effect_observed = effect_observed, exposed_confounder_prev = exposed_confounder_prev, 
                                                        unexposed_confounder_prev = unexposed_confounder_prev, confounder_outcome_effect = confounder_outcome_effect,
                                                        verbose = FALSE))
}

# reduce to assumed range
new_effect <- filter(new_effect, hr_adjusted > estimate_low & hr_adjusted < estimate_high)
min(new_effect$exposed_confounder_prev); min(new_effect$confounder_outcome_effect)

# plot possible prevalence in the exposed and confounders effects in the assumed range
plot(new_effect$exposed_confounder_prev, new_effect$confounder_outcome_effect)
