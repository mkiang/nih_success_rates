## Imports ----
library(tidyverse)
library(readxl)

## Constants ----
## Get link to correct file: https://report.nih.gov/success_rates/
T204_URL <- "https://report.nih.gov/reportweb/web/displayreport?rId=551"
T206_URL <- "https://report.nih.gov/reportweb/web/displayreport?rId=565"
SAVE_FILE_K <- "./data/T204 2022 CAREER DEV_Appls_Awds_Succ Rate_Fund by Act and IC.xlsx"
SAVE_FILE_R <- "./data/T206 2022 RES PROJ GR_Appl_Awds_Succ Rate_Fund by Type_IC and Act v2.xlsx"

## Get data ----
if (!file.exists(SAVE_FILE_K) | 
    !file.exists(SAVE_FILE_R)) {
    dir.create("./data", showWarnings = FALSE)
    download.file(T204_URL, SAVE_FILE_K)
    download.file(T206_URL, SAVE_FILE_R)
}

## Read data for K awards ----
k_awards <- readxl::read_xlsx(
    SAVE_FILE_K,
    range = "A5:G1507",
    col_names = c(
        "year",
        "activity_code",
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
        success_rate = apps_awarded / apps_received * 100,
        institute = gsub("*", "", institute, fixed = TRUE),
        institute = gsub("NCRR", "NCATS", institute, fixed = TRUE),
        institute = gsub("NCMHD", "NIMHD", institute, fixed = TRUE),
        institute = gsub("NCCAM", "NCCIH", institute, fixed = TRUE)
    )

## Filter out one-off or non-standard award granters
k_awards <- k_awards %>%
    filter(!(activity_code %in% c("FY TOTAL", "FY Total")),
           !(institute %in% c(
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
                   "NCCIH")),
           !is.na(year)
           ) %>% 
    mutate(big_group = "k")

## Read data for R awards ----
r_awards <- readxl::read_xlsx(
    SAVE_FILE_R,
    range = "A5:H4329",
    col_names = c(
        "year",
        "competing_status", 
        "institute",
        "activity_code",
        "apps_received",
        "apps_awarded",
        "success_rate",
        "total_funding"
    )
) %>% 
    mutate(big_group = "r") %>% 
    filter(competing_status == "New")

## Recalculate success rate and fix some institute names
r_awards <- r_awards %>%
    mutate(
        success_rate = apps_awarded / apps_received * 100,
        institute = gsub("*", "", institute, fixed = TRUE),
        institute = gsub("NCRR", "NCATS", institute, fixed = TRUE),
        institute = gsub("NCMHD", "NIMHD", institute, fixed = TRUE),
        institute = gsub("NCCAM", "NCCIH", institute, fixed = TRUE), 
        institute = gsub("NIDDK Type 1 Diabetes", "NIDDK", institute, fixed = TRUE),
        institute = gsub("OD COMMON FUND", "OD Common Fund", institute, fixed = TRUE),
        institute = gsub("OD COMMON\r\nFUND", "OD Common Fund", institute, fixed = TRUE)
    )

## Filter out one-off or non-standard award granters
r_awards <- r_awards %>%
    filter(!(
        activity_code %in% c(
            "TOTAL",
            "Total",
            "U01",
            "U19",
            "U34",
            "UA5",
            "UC2",
            "UC4",
            "UC7",
            "UF1",
            "UG3",
            "UH2",
            "UH3",
            "UM1",
            "UM2",
            "R55",
            "R50",
            "R61",
            "RC2",
            "RL1",
            "RL5",
            "SI7"
        )
    ), 
           !(institute %in% c(
               "All Institutes", 
               "OD ORIP-SEPA",
               "†OD ORIP-SEPA",
               "‡OD Other",
               "OD Other‡",
               "OD", 
               "OD ORIP",
               "OD ORIP†", 
               "OD ORIP-SEPA†")),
           !is.na(year)
    )

## Save ----
saveRDS(bind_rows(k_awards,
                  r_awards %>%
                      select(-competing_status)), 
        "./data/working_data.RDS")
