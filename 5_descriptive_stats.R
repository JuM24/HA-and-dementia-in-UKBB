library(tidyverse)

source('0_helper_functions.R')

# file name
file_name <- 'hearing_masterfile_ITT.rds'

hear <- readRDS(file_name)


median(hear$follow_up)
median(hear$follow_up[hear$hear_aid_any == 1])
median(hear$follow_up[hear$hear_aid_any == 0])



# hearing aid use and dementia prevalence
table(hear$hear_aid_any); prop.table(table(hear$hear_aid_any))*100
table(hear$dementia); prop.table(table(hear$dementia))*100

## outcomes (causes of censoring) for HA and non-HA groups; 
# this is by default made for per-protocol; it has to be modified (see below)
# to display numbers for the intention-to-treat analysis
ha <- filter(hear, hear_aid_any == 1)
han <- filter(hear, hear_aid_any == 0)

# dementia
nrow(filter(ha, dementia == 1 & dementia_date == censor_date))
nrow(filter(han, dementia == 1 & dementia_date == censor_date))
# death
nrow(filter(ha, death == 1 & dementia != 1 & death_date == censor_date))
nrow(filter(han, death == 1 & dementia != 1 & death_date == censor_date))
# loss to follow-up
nrow(filter(ha, death != 1 & dementia != 1 & follow_loss_date == censor_date))
nrow(filter(han, death != 1 & dementia != 1 & follow_loss_date == censor_date))
# others
nrow(filter(ha, death != 1 & dementia != 1 & follow_loss != 1))
nrow(filter(han, death != 1 & dementia != 1 & follow_loss != 1))


#### only for per-protocol! ####

# hearing aid use start among non-HA
han_switchers <- han %>%
  filter(!is.na(date_hear_aid_any) & censor_date == date_hear_aid_any)
han_nonswitchers <- han %>%
  filter(is.na(date_hear_aid_any) | (censor_date != date_hear_aid_any)) 

ha_switchers <- ha %>%
  filter(ha_cease == 1)
ha_nonswitchers <- ha %>%
  filter(ha_cease != 1)

# dementia
nrow(filter(han_nonswitchers, dementia == 1 & dementia_date == censor_date))
nrow(filter(ha_nonswitchers, dementia == 1 & dementia_date == censor_date))
# death
nrow(filter(han_nonswitchers, death == 1 & dementia != 1 & death_date == censor_date))
nrow(filter(ha_nonswitchers, death == 1 & dementia != 1 & death_date == censor_date))
# loss to follow-up
nrow(filter(han_nonswitchers, death != 1 & dementia != 1 & follow_loss_date == censor_date))
nrow(filter(ha_nonswitchers, death != 1 & dementia != 1 & follow_loss_date == censor_date))
# others
nrow(filter(han_nonswitchers, death != 1 & dementia != 1 & follow_loss != 1))
nrow(filter(ha_nonswitchers, death != 1 & dementia != 1 & follow_loss != 1))

#### only for per-protocol! ####


  

### sources of hearing loss and hearing aid ascertainment


## hearing loss
table(hear$hear_loss_origin_simple)
round(100*prop.table(table(hear$hear_loss_origin_simple)),1)

# self-report
hear$hl_self_report <- 0
hear$hl_self_report[hear$hear_dif_both_0 %in% c(2, 99) |
                      hear$hear_dif_both_1 %in% c(2, 99) |
                      hear$hear_dif_both_2 %in% c(2, 99) |
                      hear$hear_dif_both_3 %in% c(2, 99)] <- 1
table(hear$hl_self_report)
round(100*prop.table(table(hear$hl_self_report)),1)

# SRT
hear$hl_srt <- 0
hear$hl_srt[(!is.na(hear$srt_min_0) & hear$srt_min_0 >= -5.5) |
              (!is.na(hear$srt_min_1) & hear$srt_min_1 >= -5.5) |
              (!is.na(hear$srt_min_2) & hear$srt_min_2 >= -5.5) |
              (!is.na(hear$srt_min_3) & hear$srt_min_3 >= -5.5)] <- 1
table(hear$hl_srt)
round(100*prop.table(table(hear$hl_srt)),1)


# EHR
hear$hl_ehr <- 0
hear$hl_ehr[(!is.na(hear$hear_loss_a) & hear$hear_loss_a == 1) |
              (!is.na(hear$hear_loss_b) & hear$hear_loss_b == 1) |
              (!is.na(hear$hear_loss) & hear$hear_loss == 1)] <- 1
table(hear$hl_ehr)
round(100*prop.table(table(hear$hl_ehr)),1)



## different combinations of HL sources
nrow(filter(hear, hl_self_report == 1 & hl_ehr == 0 & hl_srt == 0))
nrow(filter(hear, hl_self_report == 0 & hl_ehr == 0 & hl_srt == 1))
nrow(filter(hear, hl_self_report == 0 & hl_ehr == 1 & hl_srt == 0))

nrow(filter(hear, hl_self_report == 1 & hl_srt == 1))
nrow(filter(hear, hl_self_report == 1 & hl_ehr == 1))
nrow(filter(hear, hl_ehr == 1 & hl_srt == 1))

nrow(filter(hear, hl_self_report == 1 & hl_ehr == 1 & hl_srt == 1))







## hearing aid use

# subset to just HA users
hear_ha <- filter(hear, hear_aid_any == 1)
table(hear_ha$hear_aid_origin, useNA = c('ifany'))
round(100*prop.table(table(hear_ha$hear_aid_origin, useNA = c('ifany'))),1)

# self-report
hear_ha$ha_self_report <- 0
hear_ha$ha_self_report[hear_ha$hear_aid_0 == 1 |
                         hear_ha$hear_aid_1 == 1 |
                         hear_ha$hear_aid_2 == 1 |
                         hear_ha$hear_aid_3 == 1] <- 1
table(hear_ha$ha_self_report)
round(100*prop.table(table(hear_ha$ha_self_report)),1)

# inpatient and GP
hear_ha$ha_ehr <- 0
hear_ha$ha_ehr[hear_ha$hear_aid == 1] <- 1
table(hear_ha$ha_ehr)
round(100*prop.table(table(hear_ha$ha_ehr)),1)


# combinations
nrow(filter(hear_ha, ha_self_report == 1 & ha_ehr == 0))
nrow(filter(hear_ha, ha_self_report == 0 & ha_ehr == 1))
nrow(filter(hear_ha, ha_self_report == 1 & ha_ehr == 1))



## Table 1
median(hear$age_USE); IQR(hear$age_USE); hist(hear$age_USE)
median(filter(hear, hear_aid_any == 1)$age_USE); IQR(filter(hear, hear_aid_any == 1)$age_USE); hist(filter(hear, hear_aid_any == 1)$age_USE)
median(filter(hear, hear_aid_any == 0)$age_USE); IQR(filter(hear, hear_aid_any == 0)$age_USE); hist(filter(hear, hear_aid_any == 0)$age_USE)
median(filter(hear, dementia == 1)$age_USE); IQR(filter(hear, dementia == 1)$age_USE); hist(filter(hear, dementia == 1)$age_USE)
median(filter(hear, dementia == 0)$age_USE); IQR(filter(hear, dementia == 0)$age_USE); hist(filter(hear, dementia == 0)$age_USE)
wilcox.test(hear$age_USE[hear$hear_aid_any == 1], hear$age_USE[hear$hear_aid_any == 0])


table(hear$sex); prop.table(table(hear$sex))*100
table(filter(hear, hear_aid_any == 1)$sex); prop.table(table(filter(hear, hear_aid_any == 1)$sex))*100
table(filter(hear, hear_aid_any == 0)$sex); prop.table(table(filter(hear, hear_aid_any == 0)$sex))*100
table(filter(hear, dementia == 1)$sex); prop.table(table(filter(hear, dementia == 1)$sex))*100
table(filter(hear, dementia == 0)$sex); prop.table(table(filter(hear, dementia == 0)$sex))*100
chisq.test(hear$hear_aid_any, hear$sex)

table(hear$education_USE); prop.table(table(hear$education_USE))*100
table(filter(hear, hear_aid_any == 1)$education_USE); prop.table(table(filter(hear, hear_aid_any == 1)$education_USE))*100
table(filter(hear, hear_aid_any == 0)$education_USE); prop.table(table(filter(hear, hear_aid_any == 0)$education_USE))*100
table(filter(hear, dementia == 1)$education_USE); prop.table(table(filter(hear, dementia == 1)$education_USE))*100
table(filter(hear, dementia == 0)$education_USE); prop.table(table(filter(hear, dementia == 0)$education_USE))*100
chisq.test(hear$hear_aid_any, hear$education_USE)

table(hear$tinnitus_sr_USE); prop.table(table(hear$tinnitus_sr_USE))*100
table(filter(hear, hear_aid_any == 1)$tinnitus_sr_USE); prop.table(table(filter(hear, hear_aid_any == 1)$tinnitus_sr_USE))*100
table(filter(hear, hear_aid_any == 0)$tinnitus_sr_USE); prop.table(table(filter(hear, hear_aid_any == 0)$tinnitus_sr_USE))*100
table(filter(hear, dementia == 1)$tinnitus_sr_USE); prop.table(table(filter(hear, dementia == 1)$tinnitus_sr_USE))*100
table(filter(hear, dementia == 0)$tinnitus_sr_USE); prop.table(table(filter(hear, dementia == 0)$tinnitus_sr_USE))*100
chisq.test(hear$hear_aid_any, hear$tinnitus_sr_USE)

table(hear$head_inj); prop.table(table(hear$head_inj))*100
table(filter(hear, hear_aid_any == 1)$head_inj); prop.table(table(filter(hear, hear_aid_any == 1)$head_inj))*100
table(filter(hear, hear_aid_any == 0)$head_inj); prop.table(table(filter(hear, hear_aid_any == 0)$head_inj))*100
table(filter(hear, dementia == 1)$head_inj); prop.table(table(filter(hear, dementia == 1)$head_inj))*100
table(filter(hear, dementia == 0)$head_inj); prop.table(table(filter(hear, dementia == 0)$head_inj))*100
chisq.test(hear$hear_aid_any, hear$head_inj)

table(hear$ethnicity_simple); prop.table(table(hear$ethnicity_simple))*100
table(filter(hear, hear_aid_any == 1)$ethnicity_simple); prop.table(table(filter(hear, hear_aid_any == 1)$ethnicity_simple))*100
table(filter(hear, hear_aid_any == 0)$ethnicity_simple); prop.table(table(filter(hear, hear_aid_any == 0)$ethnicity_simple))*100
table(filter(hear, dementia == 1)$ethnicity_simple); prop.table(table(filter(hear, dementia == 1)$ethnicity_simple))*100
table(filter(hear, dementia == 0)$ethnicity_simple); prop.table(table(filter(hear, dementia == 0)$ethnicity_simple))*100
chisq.test(hear$hear_aid_any, hear$ethnicity_simple)

median(hear$deprivation); IQR(hear$deprivation); hist(hear$deprivation)
median(filter(hear, hear_aid_any == 1)$deprivation); IQR(filter(hear, hear_aid_any == 1)$deprivation); hist(filter(hear, hear_aid_any == 1)$deprivation)
median(filter(hear, hear_aid_any == 0)$deprivation); IQR(filter(hear, hear_aid_any == 0)$deprivation); hist(filter(hear, hear_aid_any == 0)$deprivation)
median(filter(hear, dementia == 1)$deprivation); IQR(filter(hear, dementia == 1)$deprivation); hist(filter(hear, dementia == 1)$deprivation)
median(filter(hear, dementia == 0)$deprivation); IQR(filter(hear, dementia == 0)$deprivation); hist(filter(hear, dementia == 0)$deprivation)
wilcox.test(hear$deprivation[hear$hear_aid_any == 1], hear$age_USE[hear$hear_aid_any == 0])

median(hear$g_USE); IQR(hear$g_USE); hist(hear$g_USE)
median(filter(hear, hear_aid_any == 1)$g_USE); IQR(filter(hear, hear_aid_any == 1)$g_USE); hist(filter(hear, hear_aid_any == 1)$g_USE)
median(filter(hear, hear_aid_any == 0)$g_USE); IQR(filter(hear, hear_aid_any == 0)$g_USE); hist(filter(hear, hear_aid_any == 0)$g_USE)
median(filter(hear, dementia == 1)$g_USE); IQR(filter(hear, dementia == 1)$g_USE); hist(filter(hear, dementia == 1)$g_USE)
median(filter(hear, dementia == 0)$g_USE); IQR(filter(hear, dementia == 0)$g_USE); hist(filter(hear, dementia == 0)$g_USE)
t.test(hear$g_USE[hear$hear_aid_any == 1], hear$age_USE[hear$hear_aid_any == 0])

median(hear$srt_min_USE); IQR(hear$srt_min_USE); hist(hear$srt_min_USE)
median(filter(hear, hear_aid_any == 1)$srt_min_USE); IQR(filter(hear, hear_aid_any == 1)$srt_min_USE); hist(filter(hear, hear_aid_any == 1)$srt_min_USE)
median(filter(hear, hear_aid_any == 0)$srt_min_USE); IQR(filter(hear, hear_aid_any == 0)$srt_min_USE); hist(filter(hear, hear_aid_any == 0)$srt_min_USE)
median(filter(hear, dementia == 1)$srt_min_USE); IQR(filter(hear, dementia == 1)$srt_min_USE); hist(filter(hear, dementia == 1)$srt_min_USE)
median(filter(hear, dementia == 0)$srt_min_USE); IQR(filter(hear, dementia == 0)$srt_min_USE); hist(filter(hear, dementia == 0)$srt_min_USE)
wilcox.test(hear$srt_min_USE[hear$hear_aid_any == 1], hear$age_USE[hear$hear_aid_any == 0])

mean(hear$srt_min_USE); sd(hear$srt_min_USE); hist(hear$srt_min_USE)
mean(filter(hear, hear_aid_any == 1)$srt_min_USE); sd(filter(hear, hear_aid_any == 1)$srt_min_USE); hist(filter(hear, hear_aid_any == 1)$srt_min_USE)
mean(filter(hear, hear_aid_any == 0)$srt_min_USE); sd(filter(hear, hear_aid_any == 0)$srt_min_USE); hist(filter(hear, hear_aid_any == 0)$srt_min_USE)
mean(filter(hear, dementia == 1)$srt_min_USE); sd(filter(hear, dementia == 1)$srt_min_USE); hist(filter(hear, dementia == 1)$srt_min_USE)
mean(filter(hear, dementia == 0)$srt_min_USE); sd(filter(hear, dementia == 0)$srt_min_USE); hist(filter(hear, dementia == 0)$srt_min_USE)

table(hear$data_provider_freq); prop.table(table(hear$data_provider_freq))*100
table(filter(hear, hear_aid_any == 1)$data_provider_freq); prop.table(table(filter(hear, hear_aid_any == 1)$data_provider_freq))*100
table(filter(hear, hear_aid_any == 0)$data_provider_freq); prop.table(table(filter(hear, hear_aid_any == 0)$data_provider_freq))*100
table(filter(hear, dementia == 1)$data_provider_freq); prop.table(table(filter(hear, dementia == 1)$data_provider_freq))*100
table(filter(hear, dementia == 0)$data_provider_freq); prop.table(table(filter(hear, dementia == 0)$data_provider_freq))*100
chisq.test(hear$hear_aid_any, hear$data_provider_freq)

median(hear$follow_up); IQR(hear$follow_up); hist(hear$follow_up)
median(filter(hear, hear_aid_any == 1)$follow_up); IQR(filter(hear, hear_aid_any == 1)$follow_up); hist(filter(hear, hear_aid_any == 1)$follow_up)
median(filter(hear, hear_aid_any == 0)$follow_up); IQR(filter(hear, hear_aid_any == 0)$follow_up); hist(filter(hear, hear_aid_any == 0)$follow_up)
median(filter(hear, dementia == 1)$follow_up); IQR(filter(hear, dementia == 1)$follow_up); hist(filter(hear, dementia == 1)$follow_up)
median(filter(hear, dementia == 0)$follow_up); IQR(filter(hear, dementia == 0)$follow_up); hist(filter(hear, dementia == 0)$follow_up)

table(hear$soc_isol_USE); prop.table(table(hear$soc_isol_USE))*100
table(filter(hear, hear_aid_any == 1)$soc_isol_USE); prop.table(table(filter(hear, hear_aid_any == 1)$soc_isol_USE))*100
table(filter(hear, hear_aid_any == 0)$soc_isol_USE); prop.table(table(filter(hear, hear_aid_any == 0)$soc_isol_USE))*100
table(filter(hear, dementia == 1)$soc_isol_USE); prop.table(table(filter(hear, dementia == 1)$soc_isol_USE))*100
table(filter(hear, dementia == 0)$soc_isol_USE); prop.table(table(filter(hear, dementia == 0)$soc_isol_USE))*100
chisq.test(hear$hear_aid_any, hear$soc_isol_USE)

table(hear$mood_dis); prop.table(table(hear$mood_dis))*100
table(filter(hear, hear_aid_any == 1)$mood_dis); prop.table(table(filter(hear, hear_aid_any == 1)$mood_dis))*100
table(filter(hear, hear_aid_any == 0)$mood_dis); prop.table(table(filter(hear, hear_aid_any == 0)$mood_dis))*100
table(filter(hear, dementia == 1)$mood_dis); prop.table(table(filter(hear, dementia == 1)$mood_dis))*100
table(filter(hear, dementia == 0)$mood_dis); prop.table(table(filter(hear, dementia == 0)$mood_dis))*100
chisq.test(hear$hear_aid_any, hear$mood_dis)

table(hear$inpatient_contact_cat); prop.table(table(hear$inpatient_contact_cat))*100
table(filter(hear, hear_aid_any == 1)$inpatient_contact_cat); prop.table(table(filter(hear, hear_aid_any == 1)$inpatient_contact_cat))*100
table(filter(hear, hear_aid_any == 0)$inpatient_contact_cat); prop.table(table(filter(hear, hear_aid_any == 0)$inpatient_contact_cat))*100
table(filter(hear, dementia == 1)$inpatient_contact_cat); prop.table(table(filter(hear, dementia == 1)$inpatient_contact_cat))*100
table(filter(hear, dementia == 0)$inpatient_contact_cat); prop.table(table(filter(hear, dementia == 0)$inpatient_contact_cat))*100
chisq.test(hear$hear_aid_any, hear$inpatient_contact_cat)

table(hear$gp_contact_cat); prop.table(table(hear$gp_contact_cat))*100
table(filter(hear, hear_aid_any == 1)$gp_contact_cat); prop.table(table(filter(hear, hear_aid_any == 1)$gp_contact_cat))*100
table(filter(hear, hear_aid_any == 0)$gp_contact_cat); prop.table(table(filter(hear, hear_aid_any == 0)$gp_contact_cat))*100
table(filter(hear, dementia == 1)$gp_contact_cat); prop.table(table(filter(hear, dementia == 1)$gp_contact_cat))*100
table(filter(hear, dementia == 0)$gp_contact_cat); prop.table(table(filter(hear, dementia == 0)$gp_contact_cat))*100
chisq.test(hear$hear_aid_any, hear$gp_contact_cat)
