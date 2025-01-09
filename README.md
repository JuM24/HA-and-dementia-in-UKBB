# HA-and-dementia-in-UKBB
Code to reproduce emulation of target trial for the effect of the use of hearing aids on dementia in people with hearing loss in UKBB:  
[![DOI](https://img.shields.io/badge/DOI-10.1093/aje/kwae452-blue)](https://doi.org/10.1093/aje/kwae452).

## Required files
The below table lists the UK Biobank field IDs that are required for the analyses.

Field ID | Description
----------- | -----
  `31` |	Sex
`34`, `52` |	Year and month of birth
`53` |	Date of attending assessment centre
`189` |	Townsend deprivation index at recruitment
`6138` |	Qualifications
`399` , `4282`, `6351`, `6373`, `20016`, `20018`, `20023`, `21004`, `23324` | Results from cognitive testing
`2247`, `2257`, `3393`, `4792` |	Hearing questionnaire
`4849`, `20019`, `20021` |	Speech-in-noise test
`40000` |	Date of death
`40022`, `41270`, `41271`, `41280`, `41281` |	Hospital inpatient data
`42014` |	Asthma date
`42018` |	Dementia date
`709`, `1031`, `6160` | Social isolation
`4803` | Tinnitus
`130890 - 130902` * |	Mood disorder date
`131258 - 131261` |	Hearing loss date
`131438 - 131456` * |	Influenza/pneumonia date
`131484 - 131498` * |	Lower respiratory system disorder date
`131658 - 131670` * |	Liver disease date
`132460` |	Congenital hearing problem date
`130000 - 130344` * |	Infection or parasitic disease date
`131696 - 131838` * |	Skin disorder date
`131604 - 131608` * | Appendicitis
`41259` |	Hospital inpatient records ("hesin.txt")
`42038` | GP prescription records ("gp_registrations.txt")
`42039` | GP prescription records ("gp_scripts.csv")
`42040` | GP clinical event records ("gp_clinical.txt")

*_only even-numbered field IDs_


Additionally, the following files are required:
- "participant_opt_out.csv": a table with one column `id` that contains as observations the UK Biobank participant IDs for participants that have opted out of the study. This list will change over time and researchers with access to UK Biobank data will be regularly informed of additions to the list.
- "censoring_dates.xlsx": a long-format table with three columns: `disorder`, `data_provider`, and `date`, indicating the name of the disorder as used in the code ('dementia', 'flu', 'heart', 'hepatic', 'respiratory', 'asthma', 'skin_dis', and 'infect'), the provider of the data to UK Biobank, and the date of censoring. This repository contains "censoring_dates.xlsx" for the release of UK Biobank used in our analyses. The release that you use may not be the same, so you might need to check these dates and correct/replace them if necessary.
- "data_period.Rds": a table with the periods of continuous EHR ascertainment per participant with primary care records based on the paper by Darke et al. ([![DOI](https://img.shields.io/badge/DOI-10.1093/jamia/ocab260-blue)](https://doi.org/10.1093/jamia/ocab260). The code and prerequisites are detailed at https://github.com/philipdarke/ukbb-ehr-data/tree/main. Run the R files with prefixes 01-04 in the "01_subset_visit_data.R" folder to create and export the file.

## Running the code
1. Download the contents of this repository and extract them to the folder that will contain all the code, datasets, and variables derived in the process of data preparation.
2. Install R version 4.3.3 and run the following (choose "activate the project and use the project library" when prompted):
```R
  install.packages('renv')
  renv::restore()
```
`renv::restore()` might need to be run again to install the correct versions of the required packages.

1. Run the scripts with the prefixes "`0_`" to "`6_`" sequentially to reproduce the results. Short descriptions are available within each script.
