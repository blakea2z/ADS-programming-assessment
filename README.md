# ADS-programming-assessment
Coding assessment repository used to generate SDTM datasets, ADaM datasets, and TLFs designed with clinical trial data standards in mind.

The question_1_sdtm folder contains an R script used to generate an SDTM DS Domain utilizing the sdtm.oak package. The R script generates an SDTM dataset in a .csv format contained within the folder and the folder also contains a log file as evidence of the code running error-free. The SDTM variables were derived per an aCRF provided to the programmer.

The question_2_adam folder contains an R script used to generate an ADaM ADSL Dataset with unique derived variables. Variables were derived with the help of the admiral family of packages and tidyverse tools. This R script generates an ADaM dataset in a .csv format contained within the folder and the folder also contains a log file as evidence of the code running error-free.

The question_3_tlg folder contains two R scripts used to help generate a summary table in .html format and two figures output as .pngs to conduct adverse events reporting. 

The summary table was made with the question_3_tlg_01_create_ae_summary_table.R script.
The summary table  was generated with the help of the gtsummary package and includes treatment-emergent AE records in descending frequency. A row has also been included at the top for total TEAEs. Columns included are various treatment groups and an overall column.

Both figures were generated via question_3_tlg_02_create_visualizations.R.

The first figure is a barchart examining AE severity distribution by treatment and was generated using ggplot2. The y-axis is a count of AEs and the x-axis is treatment arms.

The second figure captures the top 10 most frequent AEs and their Clopper-Pearson CIs. The y-axis includes the names of the top 10 most frequent AEs and the x-axis is percentage of patients.

The folder also contains log files as evidence of the code running error-free.
