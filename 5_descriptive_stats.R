library(tidyverse)

source('0_helper_functions.R')
setwd('files/')

hear <- readRDS('hearing_masterfile_ITT.rds')


# plots of follow-up times
ggplot(hear, aes(x=follow_up, fill = dementia)) +
  geom_histogram() +
  scale_fill_brewer(type = "qual", palette = 6) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15), breaks = seq(0, 20, by = 2)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Year") +
  ylab("Count") +
  theme_light() +
  theme(axis.text.x = element_text(face = "plain", size = 12, color = "grey15", angle=0),
        axis.title.x = element_text(face = "bold", size = 11.5, color = "grey15"),
        axis.text.y = element_text(face = "plain", size = 12, color = "grey15"),
        axis.title.y = element_text(face = "bold", size = 12.5, color = "grey15"), legend.position = 'none') 

no_aids <- ggplot(filter(hear, hear_aid_any == 0), aes(x=follow_up)) +
  geom_histogram(fill = '#e41a1c') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15), breaks = seq(0, 20, by = 2)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Year") +
  ylab("Count") +
  theme_light() +
  theme(axis.text.x = element_text(face = "plain", size = 12, color = "grey15", angle=0),
        axis.title.x = element_text(face = "bold", size = 11.5, color = "grey15"),
        axis.text.y = element_text(face = "plain", size = 12, color = "grey15"),
        axis.title.y = element_text(face = "bold", size = 12.5, color = "grey15"), legend.position = 'none') 

aids <- ggplot(filter(hear, hear_aid_any == 1), aes(x=follow_up)) +
  geom_histogram(fill = '#377eb8') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15), breaks = seq(0, 20, by = 2)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Year") +
  ylab("Count") +
  theme_light() +
  theme(axis.text.x = element_text(face = "plain", size = 12, color = "grey15", angle=0),
        axis.title.x = element_text(face = "bold", size = 11.5, color = "grey15"),
        axis.text.y = element_text(face = "plain", size = 12, color = "grey15"),
        axis.title.y = element_text(face = "bold", size = 12.5, color = "grey15"), legend.position = 'none') 
gridExtra::grid.arrange(no_aids, aids, ncol=2)
median(hear$follow_up)
median(hear$follow_up[hear$hear_aid_any == 1])
median(hear$follow_up[hear$hear_aid_any == 0])



# hearing aid use and dementia prevalence
table(hear$hear_aid_any); prop.table(table(hear$hear_aid_any))*100
table(hear$dementia); prop.table(table(hear$dementia))*100

## outcomes (causes of censoring) for HA and non-HA groups; this is by default for per-protocol; it has to be modified
# to display numbers for the intention-to-treat analysis
ha <- filter(hear, hear_aid_any == 1)
han <- filter(hear, hear_aid_any == 0)

# dementia
nrow(filter(ha, dementia == 1 & dementia_date == censor_date))
nrow(filter(han, dementia == 1 & dementia_date == censor_date))
# death
nrow(filter(ha, death == 1 & dementia == 0 & death_date == censor_date))
nrow(filter(han, death == 1 & dementia == 0 & death_date == censor_date))

# hearing aid use start among non-HA
han$ha_switch <- 0
han$ha_switch[han$hear_aid_any == 0 & !is.na(han$date_hear_aid_any) & han$date_hear_aid_any > han$censor_date] <- 1
nrow(filter(han, ha_switch == 1 & dementia == 0 & death == 0))

# hearing aid cessation among HA
nrow(filter(ha, ha_cease == 1 & dementia == 0 & death == 0))

# "regular" censoring date (no death, no dementia, no switching from non-HA to HA)
nrow(filter(ha, death == 0 & dementia == 0 & ha_cease == 0))
nrow(filter(han, death == 0 & dementia == 0 & ha_switch == 0))

# those with several outcomes on same date?
nrow(filter(ha, dementia_date == death_date)) # 5
nrow(filter(han, dementia_date == death_date)) # 30
nrow(filter(han, dementia_date == date_hear_aid_any)) # 3
nrow(filter(han, death_date == date_hear_aid_any)) # 0

### sources of hearing loss and hearing aid ascertainment


## hearing loss
table(hear$hear_loss_origin)


# self-report
hear$hl_self_report <- 0
hear$hl_self_report[hear$hear_dif_both_0 %in% c(2, 99) |
                      hear$hear_dif_both_1 %in% c(2, 99) |
                      hear$hear_dif_both_2 %in% c(2, 99) |
                      hear$hear_dif_both_3 %in% c(2, 99)] <- 1
table(hear$hl_self_report)


# SRT
hear$hl_srt <- 0
hear$hl_srt[(!is.na(hear$srt_min_0) & hear$srt_min_0 >= -5.5) |
              (!is.na(hear$srt_min_1) & hear$srt_min_1 >= -5.5) |
              (!is.na(hear$srt_min_2) & hear$srt_min_2 >= -5.5) |
              (!is.na(hear$srt_min_3) & hear$srt_min_3 >= -5.5)] <- 1
table(hear$hl_srt)


# EHR
hear$hl_ehr <- 0
hear$hl_ehr[(!is.na(hear$hear_loss_a) & hear$hear_loss_a == 1) |
              (!is.na(hear$hear_loss_b) & hear$hear_loss_b == 1) |
              (!is.na(hear$hear_loss) & hear$hear_loss == 1)] <- 1
table(hear$hl_ehr)



## different combinations of HL source
nrow(filter(hear, hl_self_report == 1 & hl_ehr == 0 & hl_srt == 0))
nrow(filter(hear, hl_self_report == 0 & hl_ehr == 0 & hl_srt == 1))
nrow(filter(hear, hl_self_report == 0 & hl_ehr == 1 & hl_srt == 0))

nrow(filter(hear, hl_self_report == 1 & hl_srt == 1))
nrow(filter(hear, hl_self_report == 1 & hl_ehr == 1))
nrow(filter(hear, hl_ehr == 1 & hl_srt == 1))

nrow(filter(hear, hl_self_report == 1 & hl_ehr == 1 & hl_srt == 1))







## hearing aid use

# leave out the ones that received HA after the grace period
hear_ha <- filter(hear, grace_period == 1)
table(hear_ha$hear_aid_origin)

# self-report
hear_ha$ha_self_report <- 0
hear_ha$ha_self_report[hear_ha$hear_aid_0 == 1 |
                         hear_ha$hear_aid_1 == 1 |
                         hear_ha$hear_aid_2 == 1 |
                         hear_ha$hear_aid_3 == 1] <- 1
table(hear_ha$ha_self_report)

# inpatient and GP
hear_ha$ha_ehr <- 0
hear_ha$ha_ehr[hear_ha$hear_aid == 1] <- 1
table(hear_ha$ha_ehr)

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

table(hear$sex); prop.table(table(hear$sex))*100
table(filter(hear, hear_aid_any == 1)$sex); prop.table(table(filter(hear, hear_aid_any == 1)$sex))*100
table(filter(hear, hear_aid_any == 0)$sex); prop.table(table(filter(hear, hear_aid_any == 0)$sex))*100
table(filter(hear, dementia == 1)$sex); prop.table(table(filter(hear, dementia == 1)$sex))*100
table(filter(hear, dementia == 0)$sex); prop.table(table(filter(hear, dementia == 0)$sex))*100

table(hear$education_USE); prop.table(table(hear$education_USE))*100
table(filter(hear, hear_aid_any == 1)$education_USE); prop.table(table(filter(hear, hear_aid_any == 1)$education_USE))*100
table(filter(hear, hear_aid_any == 0)$education_USE); prop.table(table(filter(hear, hear_aid_any == 0)$education_USE))*100
table(filter(hear, dementia == 1)$education_USE); prop.table(table(filter(hear, dementia == 1)$education_USE))*100
table(filter(hear, dementia == 0)$education_USE); prop.table(table(filter(hear, dementia == 0)$education_USE))*100

median(hear$deprivation); IQR(hear$deprivation); hist(hear$deprivation)
median(filter(hear, hear_aid_any == 1)$deprivation); IQR(filter(hear, hear_aid_any == 1)$deprivation); hist(filter(hear, hear_aid_any == 1)$deprivation)
median(filter(hear, hear_aid_any == 0)$deprivation); IQR(filter(hear, hear_aid_any == 0)$deprivation); hist(filter(hear, hear_aid_any == 0)$deprivation)
median(filter(hear, dementia == 1)$deprivation); IQR(filter(hear, dementia == 1)$deprivation); hist(filter(hear, dementia == 1)$deprivation)
median(filter(hear, dementia == 0)$deprivation); IQR(filter(hear, dementia == 0)$deprivation); hist(filter(hear, dementia == 0)$deprivation)

median(hear$g_USE); IQR(hear$g_USE); hist(hear$g_USE)
median(filter(hear, hear_aid_any == 1)$g_USE); IQR(filter(hear, hear_aid_any == 1)$g_USE); hist(filter(hear, hear_aid_any == 1)$g_USE)
median(filter(hear, hear_aid_any == 0)$g_USE); IQR(filter(hear, hear_aid_any == 0)$g_USE); hist(filter(hear, hear_aid_any == 0)$g_USE)
median(filter(hear, dementia == 1)$g_USE); IQR(filter(hear, dementia == 1)$g_USE); hist(filter(hear, dementia == 1)$g_USE)
median(filter(hear, dementia == 0)$g_USE); IQR(filter(hear, dementia == 0)$g_USE); hist(filter(hear, dementia == 0)$g_USE)

median(hear$srt_min_USE); IQR(hear$srt_min_USE); hist(hear$srt_min_USE)
median(filter(hear, hear_aid_any == 1)$srt_min_USE); IQR(filter(hear, hear_aid_any == 1)$srt_min_USE); hist(filter(hear, hear_aid_any == 1)$srt_min_USE)
median(filter(hear, hear_aid_any == 0)$srt_min_USE); IQR(filter(hear, hear_aid_any == 0)$srt_min_USE); hist(filter(hear, hear_aid_any == 0)$srt_min_USE)
median(filter(hear, dementia == 1)$srt_min_USE); IQR(filter(hear, dementia == 1)$srt_min_USE); hist(filter(hear, dementia == 1)$srt_min_USE)
median(filter(hear, dementia == 0)$srt_min_USE); IQR(filter(hear, dementia == 0)$srt_min_USE); hist(filter(hear, dementia == 0)$srt_min_USE)

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

table(hear$mood_dis); prop.table(table(hear$mood_dis))*100
table(filter(hear, hear_aid_any == 1)$mood_dis); prop.table(table(filter(hear, hear_aid_any == 1)$mood_dis))*100
table(filter(hear, hear_aid_any == 0)$mood_dis); prop.table(table(filter(hear, hear_aid_any == 0)$mood_dis))*100
table(filter(hear, dementia == 1)$mood_dis); prop.table(table(filter(hear, dementia == 1)$mood_dis))*100
table(filter(hear, dementia == 0)$mood_dis); prop.table(table(filter(hear, dementia == 0)$mood_dis))*100

table(hear$inpatient_contact_cat); prop.table(table(hear$inpatient_contact_cat))*100
table(filter(hear, hear_aid_any == 1)$inpatient_contact_cat); prop.table(table(filter(hear, hear_aid_any == 1)$inpatient_contact_cat))*100
table(filter(hear, hear_aid_any == 0)$inpatient_contact_cat); prop.table(table(filter(hear, hear_aid_any == 0)$inpatient_contact_cat))*100
table(filter(hear, dementia == 1)$inpatient_contact_cat); prop.table(table(filter(hear, dementia == 1)$inpatient_contact_cat))*100
table(filter(hear, dementia == 0)$inpatient_contact_cat); prop.table(table(filter(hear, dementia == 0)$inpatient_contact_cat))*100

median(hear$presc_contact, na.rm = TRUE); IQR(hear$presc_contact, na.rm = TRUE); hist(hear$presc_contact)
median(filter(hear, hear_aid_any == 1)$presc_contact, na.rm = TRUE); IQR(filter(hear, hear_aid_any == 1)$presc_contact, na.rm = TRUE); hist(filter(hear, hear_aid_any == 1)$presc_contact)
median(filter(hear, hear_aid_any == 0)$presc_contact, na.rm = TRUE); IQR(filter(hear, hear_aid_any == 0)$presc_contact, na.rm = TRUE); hist(filter(hear, hear_aid_any == 0)$presc_contact)
median(filter(hear, dementia == 1)$presc_contact, na.rm = TRUE); IQR(filter(hear, dementia == 1)$presc_contact, na.rm = TRUE); hist(filter(hear, dementia == 1)$presc_contact)
median(filter(hear, dementia == 0)$presc_contact, na.rm = TRUE); IQR(filter(hear, dementia == 0)$presc_contact, na.rm = TRUE); hist(filter(hear, dementia == 0)$presc_contact)

median(hear$diag_contact, na.rm = TRUE); IQR(hear$diag_contact, na.rm = TRUE); hist(hear$diag_contact)
median(filter(hear, hear_aid_any == 1)$diag_contact, na.rm = TRUE); IQR(filter(hear, hear_aid_any == 1)$diag_contact, na.rm = TRUE); hist(filter(hear, hear_aid_any == 1)$diag_contact)
median(filter(hear, hear_aid_any == 0)$diag_contact, na.rm = TRUE); IQR(filter(hear, hear_aid_any == 0)$diag_contact, na.rm = TRUE); hist(filter(hear, hear_aid_any == 0)$diag_contact)
median(filter(hear, dementia == 1)$diag_contact, na.rm = TRUE); IQR(filter(hear, dementia == 1)$diag_contact, na.rm = TRUE); hist(filter(hear, dementia == 1)$diag_contact)
median(filter(hear, dementia == 0)$diag_contact, na.rm = TRUE); IQR(filter(hear, dementia == 0)$diag_contact, na.rm = TRUE); hist(filter(hear, dementia == 0)$diag_contact)
