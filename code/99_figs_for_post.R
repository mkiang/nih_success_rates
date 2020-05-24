library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

k_awards <- readRDS(here("data", "working_data.RDS"))

p1 <- plot_apps_and_success(k_awards,
                            k_types = c("K01", "K08", "K23", "K99"),
                            ics = "All")
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
    here("plots", "blogfig2_k99_variation.pdf"),
    p2,
    width = 6,
    height = 3.5,
    scale = 1.2,
    device = cairo_pdf
)

ggsave(
    here("plots", "blogfig1_success_over_time.jpg"),
    p1,
    width = 6,
    height = 3,
    scale = 1.2,
    dpi = 300
)

ggsave(
    here("plots", "blogfig2_k99_variation.jpg"),
    p2,
    width = 6,
    height = 3.5,
    scale = 1.2,
    dpi = 300
)
