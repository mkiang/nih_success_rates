## utils.R ----

## Imports ----
library(shiny)
library(patchwork)
library(ggrepel)

initial_sub_only <-
    function(name_x = "first_sub",
             label_x = "Initial submissions only",
             value_x = FALSE) {
        checkboxInput(name_x, label_x, value_x)
    }

institute_selector <- function(name_x, label_x, selected_x = "All") {
    selectInput(
        inputId = name_x,
        label = label_x,
        selected = selected_x,
        choices = list(
            "All NIH Institutes and Centers" = "All",
            "Fogarty Iternational Center (FIC)" = "FIC", 
            "National Center for Advancing Translational Sciences (NCATS)" = "NCATS", 
            "National Center for Complementary and Integrative Health (NCCIH)", "NCCIH", 
            "National Cancer Institute (NCI)" = "NCI",
            #  "National Center for Research Resources" = "NCRR",
            "National Eye Institute (NEI)" = "NEI",
            "National Human Genome\n   Research Institute (NHGRI)" = "NHGRI",
            "National Heart, Lung,\n   and Blood Institute (NHLBI)" = "NHLBI",
            "National Institute on\n   Aging (NIA)" = "NIA",
            "National Institute on\n   Alcohol Abuse and Alcoholism (NIAAA)" = "NIAAA",
            "National Institute of\n   Allergy and Infectious Diseases (NIAID)" = "NIAID",
            "National Institute of\n   Arthritis and Musculoskeletal and Skin Diseases (NIAMS)" = "NIAMS",
            "National Institute of\n   Biomedical Imaging and Bioengineering (NIBIB)" = "NIBIB",
            "National Institute of\n   Child Health and Human Development (NICHD)" = "NICHD",
            "National Institute on\n   Deafness and Other Communication Disorders (NIDCD)" = "NIDCD",
            "National Institute of\n   Dental and Craniofacial Research (NIDCR)" = "NIDCR",
            "National Institute of\n   Diabetes and Digestive and Kidney Diseases (NIDDK)" = "NIDDK",
            "National Institute on\n   Drug Abuse (NIDA)" = "NIDA",
            "National Institute of\n   Environmental Health Sciences (NIEHS)" = "NIEHS",
            "National Institute of\n   General Medical Sciences (NIGMS)" = "NIGMS",
            "National Institute of\n   Mental Health (NIMH)" = "NIMH",
            "National Institute on\n   Minority Health and Health Disparities (NIMHD)" = "NIMHD",
            "National Institute of\n   Neurological Disorders and Stroke (NINDS)" = "NINDS",
            "National Institute of\n   Nursing Research (NINR)" = "NINR",
            "National Library of Medicine (NLM)" = "NLM", 
            "Office of the Director Common Fund (OD Common Fund)" = "OD Common Fund"
        )
    )
}


award_selectize <- function(name_x = "k_awards",
                            label_x = NULL,
                            selected_x = c("K01", "R01", "DP2", "K99")) {
    selectizeInput(
        multiple = TRUE, 
        inputId = name_x,
        label = label_x,
        choices = list(
            "K01" = "K01",
            "K02" = "K02",
            "K05" = "K05",
            "K07" = "K07",
            "K08" = "K08",
            "K12" = "K12",
            "K18" = "K18",
            "K22" = "K22",
            "K23" = "K23",
            "K24" = "K24",
            "K25" = "K25",
            "K26" = "K26",
            "K30" = "K30",
            "K43" = "K43",
            "K76" = "K76",
            "K99" = "K99",
            "KL2" = "KL2",
            "DP1" = "DP1", 
            "DP2" = "DP2",
            "DP3" = "DP3", 
            "DP5" = "DP5",
            "P01" = "P01", 
            "R00" = "R00", 
            "R01" = "R01", 
            "R03" = "R03", 
            "R15" = "R15", 
            "R21" = "R21", 
            "R33" = "R33", 
            "R34" = "R34", 
            "R35" = "R35", 
            "R36" = "R36", 
            "R37" = "R37", 
            "R56" = "R56", 
            "RF1" = "RF1",
            "RM1" = "RM1", 
            "SI2" = "SI2"
        ),
        selected = selected_x,
        options = list(maxItems = 4)
    )
}

award_selector <- function(name_x = "k_award",
                            label_x = NULL,
                            selected_x = c("R01")) {
    selectInput(
        inputId = name_x,
        label = label_x,
        choices = list(
            "K01" = "K01",
            "K02" = "K02",
            "K05" = "K05",
            "K07" = "K07",
            "K08" = "K08",
            "K12" = "K12",
            "K18" = "K18",
            "K22" = "K22",
            "K23" = "K23",
            "K24" = "K24",
            "K25" = "K25",
            "K26" = "K26",
            "K30" = "K30",
            "K43" = "K43",
            "K76" = "K76",
            "K99" = "K99",
            "KL2" = "KL2",
            "DP1" = "DP1", 
            "DP2" = "DP2",
            "DP3" = "DP3", 
            "DP5" = "DP5",
            "P01" = "P01", 
            "R00" = "R00", 
            "R01" = "R01", 
            "R03" = "R03", 
            "R15" = "R15", 
            "R21" = "R21", 
            "R33" = "R33", 
            "R34" = "R34", 
            "R35" = "R35", 
            "R36" = "R36", 
            "R37" = "R37", 
            "R56" = "R56", 
            "RF1" = "RF1",
            "RM1" = "RM1", 
            "SI2" = "SI2"
        ),
        selected = selected_x
    )
}

plot_apps_and_success <- function(
    k_awards_df,
    activity_codes = c("K01", "K08", "K23", "K99"),
    ics = "All",
    first_sub = FALSE
    ) {
    sub_df <- k_awards_df %>%
        ungroup()
    
    if (ics != "All") {
        sub_df <- sub_df %>%
            filter(institute %in% ics) %>%
            group_by(institute)
    }
    
    x <- sub_df %>%
        group_by(year, activity_code, add = TRUE) %>%
        summarize(
            apps_received = sum(apps_received),
            apps_awarded = sum(apps_awarded),
            total_funding = sum(total_funding),
            app_first_sub = sum(app_first_sub, na.rm = TRUE),
        ) %>%
        mutate(success_rate = apps_awarded / apps_received * 100,
               success_rate_first = app_first_sub / apps_received * 100) %>%
        filter(activity_code %in% activity_codes)
    
    p1 <- ggplot(x,
                 aes(x = year, y = apps_received, color = activity_code),
                 clip = "off") +
        geom_line(size = .8, alpha = .9) +
        geom_point(size = 3.5, color = "white") +
        geom_point() +
        scale_colour_brewer("K-type", palette = "Dark2") +
        scale_x_continuous(
            NULL,
            breaks = seq(2010, 2019, 3),
            expand = c(0, .1),
            limits = c(2010, 2021)
        ) +
        scale_y_continuous("Applications received (N)",
                           expand = c(0, 20)) +
        mk_nytimes(legend.position = "none") +
        geom_text_repel(
            data = x %>%
                group_by(activity_code) %>% 
                filter(year == max(year)),
            aes(
                x = year,
                y = apps_received,
                label = activity_code,
                color = activity_code
            ),
            nudge_x = .2,
            show.legend = FALSE
        )
    
    p2 <- ggplot(x,
                 aes(x = year, y = success_rate_first, color = activity_code),
                 clip = "off") +
        geom_line(size = .8, alpha = .9) +
        geom_point(size = 3.5, color = "white") +
        geom_point() +
        scale_colour_brewer("K-type", palette = "Dark2") +
        scale_x_continuous(
            NULL,
            breaks = seq(2010, 2019, 3),
            expand = c(0, .1),
            limits = c(2010, 2021)
        ) +
        scale_y_continuous("Success rate among initial submissions (%)",
                           expand = c(0, .1)) +
        mk_nytimes(legend.position = "none") +
        geom_text_repel(
            data = x %>%
                group_by(activity_code) %>% 
                filter(year == max(year)),
            aes(
                x = year,
                y = success_rate_first,
                label = activity_code,
                color = activity_code
            ),
            nudge_x = .2,
            show.legend = FALSE
        )
    
    p3 <- ggplot(x,
                 aes(x = year, y = success_rate, color = activity_code),
                 clip = "off") +
        geom_line(size = .8, alpha = .9) +
        geom_point(size = 3.5, color = "white") +
        geom_point() +
        scale_colour_brewer("K-type", palette = "Dark2") +
        scale_x_continuous(
            NULL,
            breaks = seq(2010, 2019, 3),
            expand = c(0, .1),
            limits = c(2010, 2021)
        ) +
        scale_y_continuous("Success rate (%)") +
        mk_nytimes(legend.position = "none") +
        geom_text_repel(
            data = x %>%
                group_by(activity_code) %>% 
                filter(year == max(year)),
            aes(
                x = year,
                y = success_rate,
                label = activity_code,
                color = activity_code
            ),
            nudge_x = .2,
            show.legend = FALSE
        )
    
    if (first_sub) {
        p1 + p3 + p2 + plot_layout(ncol = 3)
    } else {
        p1 + p3 + plot_layout(ncol = 2)
    }
}


plot_circles <- function(k_awards_df,
                         activity_code_x = "K99",
                         highlight_institute = NA,
                         add_loess = TRUE,
                         first_sub = FALSE) {
    circle_alpha <- 1
    circle_stroke <- .9
    
    if (!is.na(highlight_institute)) {
        circle_alpha <- .5
        circle_stroke <- .75
    } 
    
    if (first_sub) {
        y_label <- "Success rates among initial submissions (%)"
        k_awards_df <- k_awards_df %>% 
            rename(y_target = success_rate_first)
        
    } else {
        y_label <- "Success rates (%)"
        k_awards_df <- k_awards_df %>% 
            rename(y_target = success_rate)
    }
    
    p3 <- ggplot(
        k_awards_df %>%
            filter(activity_code == activity_code_x),
        aes(
            x = year,
            y = y_target,
            size = apps_received,
            weight = apps_received
        ),
        clip = "off"
    ) 
    
    p3 <- p3 +
        geom_point(alpha = circle_alpha, 
                   shape = 1,
                   stroke = circle_stroke) +
        scale_color_viridis_d("Institute/Center") +
        scale_x_continuous(
            NULL,
            breaks = seq(2010, 2019, 3),
            expand = c(0, .1),
            limits = c(2010, 2021)
        ) +
        scale_size_area(
            "Applications received",
            # trans = "log1p",
            labels = function(x)
                round(x),
            breaks = c(0, 10, 50, 100, 150),
            max_size = 9
        ) +
        scale_y_continuous(y_label, expand = c(0, .01)) +
        mk_nytimes(legend.position = "bottom")
    
    if (add_loess) {
        p3 <- p3 +
            geom_smooth(show.legend = FALSE)
    }
    
    if (!is.na(highlight_institute)) {
        p3 <- p3 +
            geom_point(
                data =   k_awards_df %>%
                    filter(activity_code == activity_code_x,
                           institute == highlight_institute),
                aes(
                    x = year,
                    y = y_target,
                    size = apps_received
                ),
                alpha = .9,
                shape = 1,
                stroke = 1.2, 
                color = "red",
                show.legend = FALSE
            )
    }
    
    p3
}
