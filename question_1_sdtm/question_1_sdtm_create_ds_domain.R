#### Setup paths ####
base_dir <- "."        # question_1_sdtm
log_dir  <- file.path(base_dir, "logs")
out_dir  <- file.path(base_dir, "output")

dir.create(log_dir, showWarnings = FALSE)
dir.create(out_dir, showWarnings = FALSE)

#### Load Packages ####
library(admiral)
library(sdtm.oak)
library(gt)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gtsummary)
library(knitr)
library(quarto)
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)
library(haven)
#### Start log ####
log_file <- file.path(log_dir, "ds.log")
log_con  <- file(log_file, open = "wt")

sink(log_con)
sink(log_con, type = "message")

#### Load Data ####
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm
#### Create OAK ID Variables ####
ds_raw <- ds_raw %>% 
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )
#### Read in CT Data ####
study_ct <- read.csv(
  file.path(base_dir, "metadata", "sdtm_ct.csv")
)
#### Recode variables ####
#The variables below are recoded per the aCRF provided.
#If OTHERSP is not null, map the value in OTHERSP to DSTERM.
#If OTHERSP is not null, map the value in OTHERSP to DSDECOD.
#If OTHERSP is not null then DSCAT = OTHER EVENT,
#If DSDECOD = Randomized then DSCAT = PROTOCOL MILESTONE, else DSCAT = DISPOSITON EVENT
ds_raw <- ds_raw %>%
  mutate(
    DSTERM_RAW = if_else(
      !is.na(OTHERSP) & OTHERSP != "",     
      OTHERSP,
      IT.DSTERM 
    ),
    DSDECOD_RAW = if_else(
      !is.na(OTHERSP) & OTHERSP != "",
      OTHERSP,
      IT.DSDECOD
    ),
    DSCAT_RAW = case_when(
      !is.na(OTHERSP) & OTHERSP != "" ~ "OTHER EVENT",
      IT.DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",
      TRUE ~ "DISPOSITION EVENT")
  )
#### Map topic variable ####
#Topic variable is the first variable mapped as it is the primary variable in the SDTM domain.
#Other vars add further definition
ds <-
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSTERM_RAW",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) 
#### Map other vars ####
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "DSDECOD_RAW",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",   
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "DSCAT_RAW",
    tgt_var = "DSCAT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  )%>%
  # Map VISIT from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"), #ISO8601 format for dates per aCRF
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"), #ISO8601 format for dates per aCRF
    id_vars = oak_id_vars()
  )
#### Create SDTM derived vars + dataset ####
ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM)
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "DSSEQ",
    "DSTERM",
    "DSDECOD",
    "DSCAT",
    "VISITNUM",
    "VISIT",
    "DSDTC",
    "DSSTDTC",
    "DSSTDY"
  )

#### Write SDTM DS dataset ####
write.csv(
  ds,
  file = file.path(out_dir, "ds.csv"),
  row.names = FALSE
)
#### End log ####
sink(type = "message")
sink()
close(log_con)