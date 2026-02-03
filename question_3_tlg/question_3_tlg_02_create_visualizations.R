#### Setup paths ####
base_dir <- "."  # Root folder of the question
fig_dir  <- file.path(base_dir, "output", "figures")
log_dir  <- file.path(base_dir, "logs")

dir.create(fig_dir, showWarnings = FALSE)
dir.create(log_dir, showWarnings = FALSE)
#### Load packages ####
library(dplyr)
library(ggplot2)
library(pharmaverseadam)
library(scales)

#### Start log ####
log_file <- file.path(log_dir, "tlg_figures.log")
log_con  <- file(log_file, open = "wt")

sink(log_con)
sink(log_con, type = "message")
#####------------- PLOT 1: AE Severity Distribution by Treatment ------------####
#### Load and filter data ####
adae <- pharmaverseadam::adae %>%
  filter(
    SAFFL == "Y",
  )

#### Generate bar chart ####
p_sev <- adae %>%
  count(ACTARM, AESEV) %>%
  ggplot(aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c(
      "MILD"     = "#DAECFB",
      "MODERATE" = "#278EF5",
      "SEVERE"   = "#1440A9"
    )
  )+
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  theme_minimal()

#### Save figure ####
ggsave(
  filename = file.path(fig_dir, "ae_severity_by_treatment.png"),
  plot = p_sev,
  width = 8,
  height = 6,
  dpi = 300
)

#####------------- PLOT 2: Top 10 Most Frequent AEs + CIs------- ------------####
#### Load and filter data ####
adsl <- pharmaverseadam::adsl %>%
  filter(SAFFL == "Y") %>%
  select(USUBJID)

adae <- pharmaverseadam::adae %>%
  filter(
    SAFFL == "Y",
  ) %>%
  select(USUBJID, AETERM)

#### Total number of subjects ####
N_total <- n_distinct(adsl$USUBJID)

#### Subject-level incidence ####
ae_subj <- adae %>%
  distinct(USUBJID, AETERM)

#### Count subjects with each AE ####
ae_counts <- ae_subj %>%
  count(AETERM, name = "n")

#### Select top 10 AEs ####
top10_ae <- ae_counts %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

#### Calculate Clopper–Pearson 95% CIs ####
ae_plot_dat <- top10_ae %>%
  rowwise() %>%
  mutate(
    prop = n / N_total,
    ci = list(binom.test(n, N_total)$conf.int),
    lcl = ci[1],
    ucl = ci[2]
  ) %>%
  ungroup()

#### Generate figure ####
p_top10 <- ae_plot_dat %>%
  ggplot(aes(x = prop, y = reorder(AETERM, prop))) +
  geom_errorbarh(
    aes(xmin = lcl, xmax = ucl),
    width = 0.25,
    linewidth = 0.6
  ) +
  geom_point(size = 2) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(ae_plot_dat$ucl) * 1.15),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0(
      "n = ", N_total, " subjects; 95% Clopper–Pearson CIs"
    ),
    x = "Percentage of Patients",
    y = NULL
  )

#### Save figure ####
ggsave(
  filename = file.path(fig_dir, "top10_teae_overall_ci.png"),
  plot = p_top10,
  width = 9,
  height = 6,
  dpi = 300
)
#### End log ####
sink(type = "message")
sink()
close(log_con)


