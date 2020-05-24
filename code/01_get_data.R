## Imports ----
library(tidyverse)
library(readxl)

## Constants ----
## Get link to correct file: https://report.nih.gov/success_rates/
T204_URL <- "https://report.nih.gov/DisplayRePORT.aspx?rid=551"
SAVE_FILE <- "./data/T204 2019 CAREER DEV_Appls_Awds_Succ Rate_Fund by Act and IC_1.xlsx"

## Get data ----
if (!file.exists(SAVE_FILE)) {
    dir.create("./data", showWarnings = FALSE)
    download.file(T204_URL, SAVE_FILE)
}

## Read data ----
k_awards <- readxl::read_xlsx(
    SAVE_FILE,
    range = "A6:G1569",
    col_names = c(
        "year",
        "k_type",
        "institute",
        "apps_received",
        "apps_awarded",
        "success_rate",
        "total_funding"
    )
)

## Recalculate success rate and fix some institute names
k_awards <- k_awards %>%
    mutate(
        success_rate = apps_awarded / apps_received,
        institute = gsub("*", "", institute, fixed = TRUE),
        institute = gsub("NCMHD", "NIMHD", institute, fixed = TRUE),
        institute = gsub("NCCAM", "NCCIH", institute, fixed = TRUE)
    )

## Filter out one-off or non-standard award granters
k_awards <- k_awards %>%
    filter(!(k_type %in% c("FY TOTAL", "FY Total")),
           !(
               institute %in% c(
                   "NIDDK T1D",
                   "Common Fund",
                   "Activity Total",
                   "ACTIVITY TOTAL",
                   "Roadmap",
                   "OD ORIP-SEPA",
                   "†OD ORIP-SEPA",
                   "‡OD Other",
                   "OD Other‡",
                   "OD ORIP",
                   "FIC",
                   "OD ORIP-SEPA†",
                   "OD Common Fund",
                   "NCATS",
                   "NCCIH"
               )
           ),
           !is.na(year)
           )

saveRDS(k_awards, "./data/working_data.RDS")
