#### Setup paths ####
base_dir <- "."        # question_2_adsl
log_dir  <- file.path(base_dir, "logs")
out_dir  <- file.path(base_dir, "output")
meta_dir <- file.path(base_dir, "metadata")

dir.create(log_dir, showWarnings = FALSE)
dir.create(out_dir, showWarnings = FALSE)
#### Load packages ####
library(metacore)
library(metatools)
library(pharmaversesdtm)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

#### Start log ####
log_file <- file.path(log_dir, "adsl.log")
log_con  <- file(log_file, open = "wt")

sink(log_con)
sink(log_con, type = "message")
#### Load SDTM data ####
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs
suppdm <- pharmaversesdtm::suppdm

#### Convert blanks to NA ####
#Converting blank data to NA for better manipulation in R
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)
suppdm <- convert_blanks_to_na(suppdm)
#### Combine dm and suppdm for easier use####
dm_suppdm <- combine_supp(dm, suppdm)
#### Read in metacore object ####
metacore <- spec_to_metacore(
  path = file.path(meta_dir, "safety_specs.xlsx"),
  where_sep_sheet = FALSE
) %>%
  select_dataset("ADSL")
#### Start derivations ####
adsl <- build_from_derived(metacore,
                                 ds_list = list("dm" = dm_suppdm, "suppdm" = dm_suppdm), #call dm/suppdm separately to retrieve all vars
                                 predecessor_only = FALSE, keep = FALSE
)
#### Derive AGEGR9 and AGEGR9N ####
#Derivation of new age categories, catch-all conditions must come later
adsl <- adsl %>%
  derive_vars_cat(
    definition = exprs(
      ~condition,              ~AGEGR9,     ~AGEGR9N,
      AGE < 18,                "<18",        1,
      AGE >= 18 & AGE <= 50,   "18 - 50",    2,
      AGE > 50,                ">50",        3,
      TRUE,                    NA_character_, NA
    )
  )
#### Prepare exposure datetime with controlled imputation ####
#Convert dates from DTC to DTM
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )


#### Derive TRTSDTM and TRTSTMF ####
#Derivation only occurs when when patient received a valid dose (expected dose was greater than 0 or equal to 0 AND treatment was placebo)
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    new_vars = exprs(
      TRTSDTM = EXSTDTM,
      TRTSTMF = EXSTTMF
    )
  )%>%
# Treatment End Datetime
derive_vars_merged(
  dataset_add = ex_ext,
  filter_add = (EXDOSE > 0 |
                  (EXDOSE == 0 &
                     str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
  new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
  order = exprs(EXENDTM, EXSEQ),
  mode = "last",
  by_vars = exprs(STUDYID, USUBJID)
)
#### Derive ITTFL ####
#Set to Y if DM.ARM is not missing
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

#### Last known alive date ####
vs_lstdt <- vs %>%
  filter(!(is.na(VSSTRESN) & is.na(VSSTRESC))) %>%
  derive_vars_dt(
    dtc = VSDTC,
    new_vars_prefix = "VS"
  ) %>%
  filter(!is.na(VSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(VS_LSTDT = max(VSDT), .groups = "drop")

ae_lstdt <- ae %>%
  derive_vars_dt(
    dtc = AESTDTC,
    new_vars_prefix = "AE"
  ) %>%
  filter(!is.na(AEDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(AE_LSTDT = max(AEDT), .groups = "drop")

ds_lstdt <- ds %>%
  derive_vars_dt(
    dtc = DSSTDTC,
    new_vars_prefix = "DS"
  ) %>%
  filter(!is.na(DSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(DS_LSTDT = max(DSDT), .groups = "drop")


#### Derive LSTAVLDT ####
#Last known alive date is derived as the max of: Vitals complete, AE onset complete, (cont.)
#disposition complete, treatment complete.
adsl <- adsl %>%
  left_join(vs_lstdt, by = c("STUDYID", "USUBJID")) %>%
  left_join(ae_lstdt, by = c("STUDYID", "USUBJID")) %>%
  left_join(ds_lstdt, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    LSTAVLDT = pmax(
      VS_LSTDT,
      AE_LSTDT,
      DS_LSTDT,
      TRTEDTM,
      na.rm = TRUE
    )
  ) %>%
  select(-VS_LSTDT, -AE_LSTDT, -DS_LSTDT, -TRTEDTM, -TRTETMF)

#### Write ADSL dataset ####
write.csv(
  adsl,
  file = file.path(out_dir, "adsl.csv"),
  row.names = FALSE
)

#### End log ####
sink(type = "message")
sink()
close(log_con)