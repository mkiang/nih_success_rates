## Imports ----
library(tidyverse)
library(gganimate)
library(readxl)
source("./code/mk_nytimes.R")

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
                   "Roadmap",
                   "OD ORIP-SEPA",
                   "†OD ORIP-SEPA",
                   "‡OD Other",
                   "FIC",
                   "OD ORIP-SEPA†",
                   "OD Common Fund",
                   "NCATS",
                   "NCCIH"
               )
           ))

## How has total K-award funding changed over time? ----
x1 <- k_awards %>%
    group_by(year) %>%
    summarize(total_funding = sum(total_funding) / 1000000)
p1 <- ggplot(x1, 
             aes(x = year, y = total_funding), 
             clip = "off") +
    geom_line() +
    geom_point(size = 3.5, color = "white") +
    geom_point() +
    scale_x_continuous(NULL,
                       breaks = seq(2008, 2017, 3),
                       expand = c(0, .1)) +
    scale_y_continuous("Total NIH Funding for K-awards (millions of dollars)",
                       expand = c(0, .25)) +
    mk_nytimes()
ggsave(
    "./plots/overall_funding.png",
    p1, 
    width = 6,
    height = 3,
    scale = 1.5
)

## How much has each institute awarded over time?
x2 <- k_awards %>%
    group_by(year, institute) %>%
    summarize(i_funding = sum(total_funding) / 1000000) %>%
    filter(i_funding > 0)
p2 <- ggplot(x2, 
             aes(x = year, y = institute, fill = i_funding)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(
        "Funding awarded (millions of dollars)",
        trans = "log1p",
        direction = -1,
        breaks = c(
            min(x2$i_funding, na.rm = TRUE),
            1, 5, 10,
            max(x2$i_funding, na.rm = TRUE)
        ),
        labels = function(x)
            sprintf("%0.1f", round(x, 1)),
        na.value = "white",
        guide = guide_colorbar(
            title.position = "top",
            barheight = unit(.25, 'cm'),
            barwidth = unit(5.75, "cm")
        )
    ) +
    scale_x_continuous(NULL,
                       breaks = seq(2008, 2017, 3),
                       expand = c(0, 0)) +
    scale_y_discrete("NIH Institute/Center") +
    coord_equal() +
    mk_nytimes(legend.position = "bottom")
ggsave(
    "./plots/overall_funding_by_institute.png",
    p2, 
    width = 3,
    height = 6,
    scale = 1.1
)

## How much has each institute awarded over time by mechanism?
x3 <- k_awards %>%
    filter(!(k_type %in% c("KL2", "K30", "K26", "K43", "K76")))
p3 <- ggplot(x3, aes(
    x = year,
    y = institute,
    fill = total_funding / 1000000
    )) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(
        "Funding awarded (millions of dollars)",
        trans = "log1p",
        direction = -1,
        breaks = c(
            min(x3$total_funding / 1000000, na.rm = TRUE),
            .5,
            1,
            2,
            5,
            max(x3$total_funding / 1000000, na.rm = TRUE)
        ),
        labels = function(x)
            sprintf("%0.1f", round(x, 1)),
        na.value = "white",
        guide = guide_colorbar(
            title.position = "top",
            barheight = unit(.25, 'cm'),
            barwidth = unit(15, "cm")
        )
    ) +
    scale_x_continuous(NULL,
                       breaks = seq(2008, 2017, 3),
                       expand = c(0, 0)) +
    scale_y_discrete("NIH Institute/Center") +
    coord_equal() +
    mk_nytimes(legend.position = "bottom") +
    facet_wrap( ~ k_type, ncol = 6)
ggsave(
    "./plots/awards_by_type_institute.png",
    p3, 
    width = 7.5,
    height = 6,
    scale = 1.5
)

p4 <- ggplot(x3,
       aes(
           x = year,
           y = institute,
           color = success_rate,
           size = apps_received
       )) +
    geom_point(alpha = .8) +
    scale_color_viridis_c(
        "Success Rate",
        direction = -1,
        na.value = "white",
        guide = guide_colorbar(
            title.position = "top",
            barheight = unit(.25, 'cm'),
            barwidth = unit(10, "cm")
        )
    ) +
    scale_x_continuous(NULL,
                       breaks = seq(2008, 2017, 3),
                       expand = c(0, .75)) +
    scale_y_discrete("NIH Institute/Center") +
    scale_size_area("Number of applications",
                    guide = guide_legend(title.position = "top")) +
    coord_equal() +
    mk_nytimes(legend.position = "bottom") +
    facet_wrap( ~ k_type, ncol = 6)
ggsave(
    "./plots/successrates_by_type_institute.png",
    p4, 
    width = 7.5,
    height = 6,
    scale = 1.5
)


p5 <- ggplot(
    x3 %>%
        filter((k_type %in% c(
            "K01", "K08", "K23", "K99"
        ))) %>%
        mutate(year = as.integer(year)),
    aes(
        x = success_rate,
        y = total_funding / 1000000,
        group = k_type,
        size = apps_received,
        color = institute
    )
) +
    geom_point(alpha = .7) +
    facet_wrap( ~ k_type, nrow = 1) +
    scale_color_discrete("NIH Institute / Center",
                         guide = guide_legend(title.position = "top")) +
    scale_size_area(
        "Number of Applications",
        max_size = 8,
        guide = guide_legend(title.position = "top")
    ) +
    scale_y_continuous("Total amount funded (millions of dollars)") +
    scale_x_continuous("Proportion of applications funded",
                       breaks = c(0, .5, 1)) +
    mk_nytimes(legend.position = "bottom") +
    labs(title = 'Year: {frame_time}') +
    transition_time(year) +
    ease_aes('linear')
a_p5 <-
    animate(p5,
            width = 600,
            height = 450,
            detail = 10,)
anim_save("./plots/animated_awards.gif", a_p5)

