## utils.R ----

## Imports ----
library(shiny)
library(patchwork)
library(ggrepel)

institute_selector <- function(name_x, label_x, selected_x = "All") {
    selectInput(
        inputId = name_x,
        label = label_x,
        selected = selected_x,
        choices = list(
            "All NIH Institutes and Centers" = "All",
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
            "National Library of Medicine (NLM)" = "NLM"
        )
    )
}


award_selectize <- function(name_x = "k_awards",
                            label_x = NULL,
                            selected_x = c("K01", "K08", "K23", "K99")) {
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
            "KL2" = "KL2"
        ),
        selected = selected_x,
        options = list(maxItems = 4)
    )
}

award_selector <- function(name_x = "k_award",
                            label_x = NULL,
                            selected_x = c("K99")) {
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
            "KL2" = "KL2"
        ),
        selected = selected_x
    )
}

plot_apps_and_success <- function(
    k_awards_df,
    k_types = c("K01", "K08", "K23", "K99"),
    ics = "All"
    ) {
    sub_df <- k_awards_df %>%
        ungroup()
    
    if (ics != "All") {
        sub_df <- sub_df %>%
            filter(institute %in% ics) %>%
            group_by(institute)
    }
    
    x <- sub_df %>%
        group_by(year, k_type, add = TRUE) %>%
        summarize(
            apps_received = sum(apps_received),
            apps_awarded = sum(apps_awarded),
            total_funding = sum(total_funding)
        ) %>%
        mutate(success_rate = apps_awarded / apps_received * 100) %>%
        filter(k_type %in% k_types)
    
    p1 <- ggplot(x,
                 aes(x = year, y = apps_received, color = k_type),
                 clip = "off") +
        geom_line(size = .8, alpha = .9) +
        geom_point(size = 3.5, color = "white") +
        geom_point() +
        scale_colour_brewer("K-type", palette = "Dark2") +
        scale_x_continuous(
            NULL,
            breaks = seq(2010, 2019, 3),
            expand = c(0, .1),
            limits = c(2010, 2019.6)
        ) +
        scale_y_continuous("Applications received (N)",
                           expand = c(0, 20)) +
        mk_nytimes(legend.position = "none") +
        geom_text_repel(
            data = x %>%
                group_by(k_type) %>% 
                filter(year == max(year)),
            aes(
                x = year,
                y = apps_received,
                label = k_type,
                color = k_type
            ),
            nudge_x = .2,
            show.legend = FALSE
        )
    
    # p2 <- ggplot(x,
    #              aes(x = year, y = apps_awarded, color = k_type),
    #              clip = "off") +
    #     geom_line(size = .8, alpha = .9) +
    #     geom_point(size = 3.5, color = "white") +
    #     geom_point() +
    #     scale_colour_brewer("K-type", palette = "Dark2") +
    #     scale_x_continuous(
    #         NULL,
    #         breaks = seq(2010, 2019, 3),
    #         expand = c(0, .1),
    #         limits = c(2010, 2019.6)
    #     ) +
    #     scale_y_continuous("Applications awarded (N)",
    #                        expand = c(0, 20)) +
    #     mk_nytimes(legend.position = "none") +
    #     geom_text_repel(
    #         data = x %>%
    #             group_by(k_type) %>% 
    #             filter(year == max(year)),
    #         aes(
    #             x = year,
    #             y = apps_received,
    #             label = k_type,
    #             color = k_type
    #         ),
    #         nudge_x = .2,
    #         show.legend = FALSE
    #     )
    
    p3 <- ggplot(x,
                 aes(x = year, y = success_rate, color = k_type),
                 clip = "off") +
        geom_line(size = .8, alpha = .9) +
        geom_point(size = 3.5, color = "white") +
        geom_point() +
        scale_colour_brewer("K-type", palette = "Dark2") +
        scale_x_continuous(
            NULL,
            breaks = seq(2010, 2019, 3),
            expand = c(0, .1),
            limits = c(2010, 2019.6)
        ) +
        scale_y_continuous("Success rate (%)") +
        mk_nytimes(legend.position = "none") +
        geom_text_repel(
            data = x %>%
                group_by(k_type) %>% 
                filter(year == max(year)),
            aes(
                x = year,
                y = success_rate,
                label = k_type,
                color = k_type
            ),
            nudge_x = .2,
            show.legend = FALSE
        )
    
    # p1 + p2 + p3 + plot_layout(ncol = 3)
    p1 + p3 + plot_layout(ncol = 2)
}

plot_circles <- function(k_awards_df,
                         k_type_x = "K99",
                         highlight_institute = NA,
                         add_loess = TRUE) {
    circle_alpha <- 1
    circle_stroke <- .9
    
    if (!is.na(highlight_institute)) {
        circle_alpha <- .5
        circle_stroke <- .75
    } 
    
    p3 <- ggplot(
        k_awards %>%
            filter(k_type == k_type_x),
        aes(
            x = year,
            y = success_rate,
            size = apps_received,
            weight = apps_received
        ),
        clip = "off"
    ) +
        geom_point(alpha = circle_alpha, 
                   shape = 1,
                   stroke = circle_stroke) +
        scale_color_viridis_d("Institute/Center") +
        scale_x_continuous(
            NULL,
            breaks = seq(2010, 2019, 3),
            expand = c(0, .1),
            limits = c(2010, 2019.6)
        ) +
        scale_size_area(
            "Applications received",
            # trans = "log1p",
            labels = function(x)
                round(x),
            breaks = c(0, 10, 50, 100, 150),
            max_size = 9
        ) +
        scale_y_continuous("Success rates (%)", expand = c(0, .01)) +
        mk_nytimes(legend.position = "bottom")
    
    if (add_loess) {
        p3 <- p3 +
            geom_smooth(show.legend = FALSE)
    }
    
    if (!is.na(highlight_institute)) {
        p3 <- p3 +
            geom_point(
                data =   k_awards %>%
                    filter(k_type == k_type_x,
                           institute == highlight_institute),
                aes(
                    x = year,
                    y = success_rate,
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
