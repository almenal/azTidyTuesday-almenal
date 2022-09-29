library(tidyverse)
library(here)
theme_set(theme_bw())

employed <- readr::read_csv(here("azTidyTuesday_20220315_AlvaroMendoza", "employed.csv"))
earn <- readr::read_csv(here("azTidyTuesday_20220315_AlvaroMendoza", "earn.csv"))

summary(employed %>% mutate(across(where(is.character), as.factor)))
length(unique(employed$industry))

# Industries' men:women ratio ------------

industry_gender_ratio <-
  employed %>%
  filter(race_gender == "Men" | race_gender == "Women",!(
    industry %in% c("Men", "Women", "White", "Asian", "Black or African American")
  )) %>%
  select(!any_of(c(
    "employ_n", "major_occupation", "minor_occupation"
  ))) %>%
  distinct() %>% #arrange(industry, race_gender) %>% View()
  pivot_wider(names_from = race_gender,
              values_from = "industry_total") %>%
  mutate(
    industry_total = Men + Women,
    gender_ratio = Men / Women,
    log_gender_ratio = log2(Men / Women),
  ) %>%
  arrange(desc(industry_total)) %>%
  mutate(industry = str_wrap(industry, 25)) %>%
  mutate(industry = factor(industry, levels = unique(.[["industry"]])))

p_gender_ratio <- (
  ggplot(
    industry_gender_ratio %>% drop_na(industry),
    aes(log_gender_ratio, industry)
    )

  # Main
  + geom_point(
      aes(fill = year, size = industry_total),
      alpha = 0.5, shape = 21, stroke = 1
    )
  + scale_fill_viridis_c(
      guide = guide_legend(
        title = "Year",
        override.aes = list(alpha = 1, size = 2.5, stroke = 0.5),
        direction = "horizontal",
        label.position = "bottom",
        nrow = 1,
        # label.theme = element_text(angle = 45)
      )
    )

  # Visual aid
  + geom_vline(xintercept = 0, linetype = 3)

  # Labels, themes
  + labs(
    x = "Men:Woman ratio (log2)", y = "",
    title = "Women are taking over male-dominated fields",
    subtitle = str_wrap(
      str_c("Progression of gender ratio across industries. Most industries have ",
      "not changed their gender representation, except some traditionally ",
      "male-dominated fields like construction."),
      85
      )
    )
  + guides(size = "none")
  + theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = 'bold', hjust = 0.2,
                              margin = margin(t = 0.2, b = 0.1, unit = 'in')),
    plot.subtitle = element_text(size = 10, hjust = 0,
                                 margin = margin(l=1,r=1, unit = 'in'))
    )

  # Annotations
  + annotate("label", x = 0, y = 19, label = "More women", fill = "#DB72FB", hjust=1)
  + annotate("label", x = 0, y = 19, label = "More men", fill = "#619CFF", hjust=0)
  + annotate(
      "segment",
      x = 0, xend = -2, y = 18.25, yend = 18.25,
      color = "#DB72FB", size = 1.5,
      arrow = arrow(angle = 10, length = unit(0.05, 'npc'), type = "closed")
    )
  + annotate(
      "segment",
      x = 0, xend = +2, y = 18.25, yend = 18.25,
      color = "#619CFF", size = 1.5,
      arrow = arrow(angle = 10, length = unit(0.05, 'npc'), type = "closed")
    )
)

ggsave(here::here("azTidyTuesday_20220315_AlvaroMendoza/",
                  "gender_ratio_per_industry.png"),
       plot = p_gender_ratio,
       width = 3,
       height = 4,
       scale = 2)

