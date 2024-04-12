# HA-and-dementia-in-UKBB
Code to reproduce emulation of target trial for the effect of the use of hearing aids on dementia in people with hearing loss in UKBB.

## Required files
The below table lists the UK Biobank field IDs that are required for the analyses.

Field ID | Description
----------- | -----
`31` |	Sex
`34`, `52` |	Year and month of birth
`53` |	Date of attending assessment centre
`48` |	Waist circumference
`189` |	Townsend deprivation index at recruitment
`6138` |	Qualifications
`1558` |	Alcohol use frequency
`20116` |	Smoking status
`6164` |	Physical activity
`24003`, `24004`, `24006`, `24016`, `24017`, `24018` | Air pollution
`2020` | Loneliness
`2050` | Depressive mood
`709`, `1031`, `6160` | Social isolation
`399` , `4282`, `20016`, `20018`, `20023` | Results from cognitive testing
`2247`, `2257`, `20019`, `20021`, `2257` |	Hearing impairment variables
`131258 - 131261` |	Hearing loss date
`40000` |	Date of death
`40022`, `41270`, `41271`, `41280`, `41281`, `41234` |	Hospital inpatient data
`130846` | Delirium date
`42018` |	Dementia date
`130890 - 130902` * |	Mood disorder date
`131296 - 131306` * |	Heart disease date
`131438 - 131456` * |	Influenza/pneumonia date
`131484 - 131498` * |	Lower respiratory system disorder date
`131658 - 131670` * |	Liver disease date
`131360 - 131378` * |	Cerebrovascular disease date
`130714` |	Hypertension date
`130814` |	Hyperlipidaemia date
`130874 - 130888` * |	Psychotic disorder date
`131212` | Visual impairment
`131060, 130920` | Sleep disorder
`42006`, `42028`, `130992-131032`, `131042-131050`, `131056`, `131058`, `130992-131010`, `131038-13120`, `131442` * | CNS disorder date
`130690-130748` * | Endocrine disorder date
`130750-130788` * | Nutritional deficiency date
`130798-130834` * | Metabolic disorder date
`40005`, `40006`, `40013` | Cancer
`41259` |	Hospital inpatient records ("hesin.txt")
`42038` | GP prescription records ("gp_registrations.txt")
`42039` | GP prescription records ("gp_scripts.csv")
`42040` | GP clinical event records ("gp_clinical.txt")

*_only even-numbered field IDs_

Additionally, the following files are required:
- "participant_opt_out.csv": a table with one column `id` that contains as observations the UK Biobank participant IDs for participants that have opted out of the study. This list will change over time and researchers with access to UK Biobank data will be regularly informed of additions to the list.


## The coding environment
1. Download the contents of this repository and extract them to the working directory.
2. Install R version 4.3.2 (https://cran.rstudio.com/bin/windows/base/old/4.3.2/) and Rstudio ([https://www.rstudio.com/categories/rstudio-ide/](https://posit.co/products/open-source/rstudio/)), and run the following (choose "activate the project and use the project library" when prompted):
```R
  install.packages('renv')
  renv::restore()
```
- `renv::restore()` might need to be run again to install the correct versions of the required packages.
3. Install Python via the Anaconda distribution (https://www.anaconda.com/download/success) and perform the following steps:
- (a) Launch the Anaconda prompt/terminal and navigate the the working directory.
- (b) Run `conda env create -f environment.yml` to setup the correct Python environment.
- (c) Run `conda activate pseudoscales.env` to activate the Python environment.
- The project can be discontinued and resumed later, but upon newly launching the Anaconda terminal, always repeat steps (a) and (c).

## Running the code



3. Run the scripts with the prefixes "`1_`" to "`6_`" sequentially to reproduce the results. Short descriptions are available within each script.
