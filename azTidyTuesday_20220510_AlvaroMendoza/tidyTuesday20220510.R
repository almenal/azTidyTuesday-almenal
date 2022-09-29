library(tidyverse)
library(patchwork)
library(magrittr)
library(tvthemes)
theme_set(theme_bw())

phds <- read_csv("azTidyTuesday_20220510_AlvaroMendoza/phds.csv") %>%
  mutate(field_index = as.numeric(factor(broad_field)))

phds %>% count(broad_field, year)
phds %>% count(field)
phds %>% group_by(year) %>% summarise(total = sum(n_phds, na.rm = T))
phds %>% filter(year == 2008) %>% group_by(broad_field) %>% summarise(total = sum(n_phds, na.rm = T)) %>% pull(total) %>% sum()


# Line graph + Stacked bar --------------------------------------------------

phds <- mutate(phds, broad_field = case_when(
  broad_field == "Mathematics and computer sciences" ~ "Maths/Comp. Sci",
  broad_field == "Psychology and social sciences" ~ "Psychology &\nSocial sciences",
  broad_field == "Humanities and arts" ~ "Humanities/arts",
  TRUE ~ broad_field
))

# Choosing colors...
# names(tvthemes:::westeros_palette)
# scales::show_col(tvthemes:::westeros_palette[[4]])
# names(tvthemes:::brooklyn99_palette)
# scales::show_col(tvthemes:::brooklyn99_palette[[1]])
# scales::show_col(tvthemes:::brooklyn99_palette[[2]])

phds_total <- (
  phds
  %>% group_by(year)
  %>% summarise(total_phds = sum(n_phds, na.rm = TRUE), .groups = "drop")
  %>% ggplot(aes(year, total_phds))
  + geom_line(size = 1.5, color = "#175E78")
  + scale_y_continuous(
      labels = scales::number_format(accuracy = 1, scale = 1/1e3, suffix = " K")
    )
  + labs(x = "", y = "Total PhDs\nawarded")
)
phds_proportions <- (
  phds
  %>% group_by(year, broad_field)
  %>% summarise(total_phds = sum(n_phds, na.rm = TRUE), .groups = "drop")
  %>% ggplot(aes(year, total_phds, fill = broad_field))
  + geom_col(color = "white", position = position_fill())
  + scale_y_continuous(labels = scales::percent_format())
  + tvthemes::scale_fill_brooklyn99(palette = "Dark")
  + labs(x = "", y = "Proportion of awarded PhDs")
)

(
  (phds_total / phds_proportions)
  + plot_layout(ncol = 1, heights = c(1, 3))
  + plot_annotation(title = "Airplanes nowadays are full of doctors...",
                    subtitle = "...but not many of them can save you!",
                    theme = theme(plot.title = element_text(hjust = .5),
                                  plot.subtitle = element_text(hjust = .5)))
  & scale_x_continuous(breaks = min(phds$year):max(phds$year))
  & theme(legend.position = "bottom", legend.title = element_blank())
) %T>%
  ggsave("azTidyTuesday_20220510_AlvaroMendoza/phds_by_field.png", plot = .,
         width = 3, height = 4, scale = 2)



