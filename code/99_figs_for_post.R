library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

k_awards <- readRDS(here("data", "working_data.RDS"))
first_sub <- read_csv(here("data", "first_submissions_all.csv")) %>% 
    select(year = fy, 
           k_type = activity,
           app_first_sub = n_first_sub,
           institute)

first_sub <- bind_rows(
    first_sub,
    first_sub %>% 
        group_by(year, k_type) %>% 
        summarize(app_first_sub = sum(app_first_sub)) %>%
        mutate(institute = "All")
)

k_awards <- k_awards %>% 
    left_join(first_sub) %>% 
    mutate(success_rate_first = app_first_sub / apps_received * 100)

p1 <- plot_apps_and_success(k_awards,
                            activity_codes = c("K01", "K08", "R01", "K99"),
                            ics = "All")
p1b <- plot_apps_and_success(k_awards,
                            activity_codes = c("K01", "K08", "R01", "K99"),
                            ics = "All", first_sub = TRUE)
p2 <- plot_circles(k_awards, "K99", "NIDA")

ggsave(
    here("plots", "blogfig1_success_over_time.pdf"),
    p1,
    width = 6,
    height = 3,
    scale = 1.2,
    device = cairo_pdf
)

ggsave(
    here("plots", "blogfig1b_success_over_time.pdf"),
    p1b,
    width = 9,
    height = 3,
    scale = 1.2,
    device = cairo_pdf
)

ggsave(
    here("plots", "blogfig2_k99_variation.pdf"),
    p2,
    width = 6,
    height = 3.5,
    scale = 1.2,
    device = cairo_pdf
)

ggsave(
    here("plots", "blogfig1_success_over_time.png"),
    p1,
    width = 6,
    height = 3,
    scale = 1.2,
    dpi = 300
)

ggsave(
    here("plots", "blogfig1b_success_over_time.png"),
    p1b,
    width = 9,
    height = 3,
    scale = 1.2,
    dpi = 300
)

ggsave(
    here("plots", "blogfig2_k99_variation.png"),
    p2,
    width = 6,
    height = 3.5,
    scale = 1.2,
    dpi = 300
)
