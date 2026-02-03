#### Setup paths ####
base_dir <- "."        # question_3_tlg
log_dir  <- file.path(base_dir, "logs")
out_dir  <- file.path(base_dir, "output")

dir.create(log_dir, showWarnings = FALSE)
dir.create(out_dir, showWarnings = FALSE)

#### Load packages ####
library(dplyr)
library(gtsummary)
library(pharmaverseadam)

#### Start log ####
log_file <- file.path(log_dir, "tlg_table.log")
log_con  <- file(log_file, open = "wt")

sink(log_con)
sink(log_con, type = "message")
#### Load and filter data ####
# ADSL
adsl <- pharmaverseadam::adsl %>%
  filter(SAFFL == "Y") %>%
  select(USUBJID, ACTARM)

# Adverse events
adae <- pharmaverseadam::adae %>%
  filter(
    SAFFL == "Y",
    TRTEMFL == "Y" #Treatment emergent AE records
  ) %>%
  select(USUBJID, ACTARM, AETERM)

#### Create Any TEAE row ####
any_teae <- adae %>%
  distinct(USUBJID, ACTARM) %>%
  mutate(AETERM = "Any Treatment-Emergent Adverse Event")

#### Combine Any TEAE row with main data ####
adae_for_table <- bind_rows(
  any_teae,
  adae
)

#### Create AE summary table ####
ae_table <- adae_for_table %>%
  tbl_summary(
    by = ACTARM, #columns by treatment group
    include = AETERM, #AE term rows
    statistic = all_categorical() ~ "{n} ({p}%)", #counts + percentages
    percent = "column",
    sort = list(all_categorical() ~ "frequency")
  ) %>%
  add_overall(last = TRUE) %>% #add overall column
  modify_header(label = "Preferred Term") %>%
  bold_labels()

#### Force Any TEAE row to appear first and order rest of rows####
ae_table <- ae_table %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        order = ifelse(
          label == "Any Treatment-Emergent Adverse Event", 0, 1
        )
      ) %>%
      arrange(order, desc(as.numeric(gsub(" .*", "", stat_0)))) %>%
      select(-order) #descending frequency
  )

#### Display table ####
ae_table

#### Save table output ####
ae_gt <- as_gt(ae_table)

gtsave(
  ae_gt,
  filename = file.path(out_dir, "ae_summary.html")
)

message("AE summary table created successfully.")

#### End log ####
sink(type = "message")
sink()
close(log_con)