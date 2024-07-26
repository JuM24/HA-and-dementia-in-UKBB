### Extract the required variables from the main UKBB phenotype file. ###

## This code assumes that the UKBB file is a .csv file and
# that its column names have the format:
# XfieldID.assessment0... etc; so for field ID 53 (date of attending assesment centre),
# we would have X53.0.0, X53.1.0, X53.2.0, X53.3.0 for assessments
# 0, 1, 2, and 3, respectively

file_path <- '' # path to the folder where the UKKB phenotype file is located 
file_name <- '' # name of the UKBB phenotype file

setwd(fle_path)

library(tidyverse)

# FO skin and subcutaneous tissues disorders 131696-131838; just even ones
FO_skin_fields <- sapply(seq(131696, 131838, 2), as.character)
FO_skin_fields <- sapply(FO_skin_fields, function(x) paste0('X', x, '.'))
# FO infectious and parasitic disorders; 130000-130344 just even ones
FO_infect_fields <- sapply(seq(130000, 130344, 2), as.character)
FO_infect_fields <- sapply(FO_infect_fields, function(x) paste0('X', x, '.'))

ukb <- read.csv(file_name)

# main raw data
main_vars <- ukb %>% 
  select(c(eid, starts_with(
    # demographic
    c('X52.', 'X34.', 'X53.','X31.', 'X6138.', 'X22189.', 'X21000.', 'X191.',
      'X42018.', # dementia
      # cognitive tests
      'X20016.', 'X20018.', 'X20023.', 'X399.', 'X6351.', 'X6373.', 'X23324.', 
      'X4282.', 'X21004.',
      # date of death
      'X40000.',
      # mood disorders
      'X130890.', 'X130892.', 'X130894.', 'X130896.', 'X130898.', 'X130900.', 
      'X130902.', 
      # respiratory disease
      'X131484.', 'X131486.', 'X131488.', 'X131490.', 'X131492.', 'X131494.', 
      'X131496.',
      # liver disease
      'X131498.', 'X131658.', 'X131660.', 'X131662.', 'X131664.', 'X131666.', 
      'X131668.', 'X131670.',
      # influenza/pneumonia
      'X131438.', 'X131440.', 'X131442.', 'X131444.', 'X131446.', 'X131448.', 
      'X131450.', 'X131452.', 'X131454.', 'X131456.',
      # heart disease
      'X131296.', 'X131298.', 'X131300.', 'X131302.', 'X131304.', 'X131306.',
      # appendicitis
      'X131604.', 'X131606.', 'X131608.',
      'X4803.', # tinnitus
      'X42014.',     # asthma
      FO_skin_fields, # skin disorders
      FO_infect_fields, # infection disorders
      'X40022.', 'X41270.', 'X41280.', 'X41271.', 'X41281.', # inpatient data
      # hearing-related codes
      'X2247.', 'X2257.', 'X4849.', 'X3393.', 'X20019.', 'X20021.', 'X131258.', 
      'X131260.', 'X131259.', 'X131261.', 'X4792.', 'X132460.'))))
saveRDS(main_vars, file = 'main_vars.Rds')

rm(list = ls()); gc()