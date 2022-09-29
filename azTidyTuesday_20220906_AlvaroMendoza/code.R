library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(stringr)
library(NatParksPalettes)

library(ggtext)
library(showtext)
showtext_auto()

theme_set(theme_bw())

data <- tidytuesdayR::tt_load('2021-09-14')

# Is there anything that can explain that a song is going to make it to the
# top of the charts? Could it be explained by its genre, its danceability,
# its energy, its loudness, its speechiness, its acousticness, its
# instrumentalness,  its liveness, its valence, its tempo, its duration?


# EDA ---------------------------------------------------------------------

bill <- data$billboard %>% mutate(week = lubridate::as_date(week_id, format = "%m/%d/%Y"))
range(bill$week)

songs <- unique(bill$song_id)
n_songs <- 20
song_sample <- sample(songs, n_songs)

top_position <- (
  bill
  %>% filter(song_id %in% song_sample) # reduce the amount of data for EDA
  %>% group_by(song_id)
  %>% mutate(song_top_pos = min(week_position))
  %>% filter(week_position == song_top_pos)
  %>% ungroup()
  )

(
  ggplot(
    bill %>% filter(song_id %in% song_sample),
    aes(week, -week_position)
    )
  + geom_point(shape = 21)
  + geom_line(aes(group = song, color = song), show.legend = FALSE)
  + geom_label_repel(data = top_position, aes(label = song, fill = song), show.legend = FALSE)
  + scale_color_manual(values = natparks.pals("DeathValley", n = n_songs))
  + scale_fill_manual(values = natparks.pals("DeathValley", n = n_songs))
)

# bill %>% filter(song_id %in% song_sample[[1]]) %>% View()

bill %>% filter(instance > 1) %>% View()
tmp <- bill %>% filter(instance == max(bill$instance)) %>% pull(song_id) %>% unique()

(
  ggplot(
    bill %>% filter(song_id %in% tmp),
    aes(week, -week_position)
  )
  + geom_point(shape = 21)
  + geom_line(aes(group = song, color = song), show.legend = FALSE, size = 1.5)
  # + geom_label_repel(data = top_position, aes(label = song, fill = song), show.legend = FALSE)
  + scale_color_manual(values = natparks.pals("DeathValley", n = length(tmp)))
  + scale_fill_manual(values = natparks.pals("DeathValley", n = length(tmp)))
  + scale_x_date(breaks = "5 years", minor_breaks = "1 year")
)


# Model -------------------------------------------------------------------

# Let's get the maximum weeks on chart per song
weeks_on_chart <- (
  bill
  %>% group_by(song_id)
  %>% summarise(
    max_week_on_chart = max(weeks_on_chart)
  )
)

df <- left_join(weeks_on_chart, data$audio_features, by = "song_id")
df_sub <- df %>%
  select(max_week_on_chart, where(is.numeric),
         -spotify_track_popularity, spotify_track_explicit)
m <- lm(max_week_on_chart ~ ., data = df_sub)
coefs_ordered <- coefficients(m)[ order(abs(coefficients(m)), decreasing = TRUE) ]

# Quick viz to check coefficients of linear model
xtmp <- barplot(coefs_ordered)
text(
  xtmp,
  0, #abs(coefs_ordered),
  names(coefs_ordered),
  adj=c(0, 0),
  srt = 45
  )


# Plot --------------------------------------------------------------------
# Plot which songs stayed the longest on charts based
# on top important variables from linear model

df_long <- df_sub %>%
  mutate(song_id = df$song_id) %>%
  relocate(song_id, max_week_on_chart) %>%
  pivot_longer(
    cols = 3:ncol(.),
    names_to = "variable",
    values_to = "value"
    ) %>%
  drop_na()

variable_to_title_case <- setNames(
  snakecase::to_title_case(unique(df_long$variable)),
  nm = unique(df_long$variable)
)

df_long_top_var <- (
  df_long
  %>% filter(variable %in% names(coefs_ordered)[1:5])
  %>% mutate(
    variable_title = str_c(
      variable_to_title_case[variable],
      "~ ( beta == ",
      round(coefs_ordered[variable], 3),
      " )"
      )
    )
)


# Iteration 1: scatter ----------------------------------------------------

p_lm <- (
  ggplot(df_long_top_var,# %>% sample_frac(0.05),
         aes(value, max_week_on_chart))
  + geom_point(alpha = 0.15)
  + geom_smooth(formula = y ~ x, method = "lm", se = TRUE, size = 1.25)
  + facet_wrap( ~ variable_title, ncol = 2, labeller = label_parsed)
)
ggsave(
  here::here("azTidyTuesday_20220920_AlvaroMendoza",
             "weeks_on_chart_vs_top_var_lm.png"),
  width = 12,
  height = 12,
  plot = p_lm
  )

p_loess <- (
  ggplot(df_long_top_var,# %>% sample_frac(0.05),
         aes(value, max_week_on_chart))
  + geom_point(alpha = 0.15)
  + geom_smooth(formula = y ~ x, method = "loess", se = TRUE, size = 1.25)
  + facet_wrap( ~ variable_title, ncol = 2, labeller = label_parsed)
)
ggsave(
  here::here("azTidyTuesday_20220920_AlvaroMendoza",
             "weeks_on_chart_vs_top_var_loess.png"),
  width = 12,
  height = 12,
  plot = p_loess
  )


# Iteration 2: Hexbin -----------------------------------------------------

song_meta <- (
  data$billboard
  %>% select(song_id, song, performer)
  %>% distinct()
  %>% mutate(song_id_clean = str_c(song, " - ", performer))
)

K = 5
top_K_song_ever <- (
  df_long_top_var
  %>% select(song_id, max_week_on_chart)
  %>% distinct()
  %>% mutate(week_on_chart_rank = rank(-max_week_on_chart, ties.method = "min"))
  %>% filter(week_on_chart_rank <= K)
  %>% left_join(song_meta, by = "song_id")
  %>% left_join(df_long_top_var, by = c("song_id", "max_week_on_chart"))
)

p_lm_hex <- (
  ggplot(df_long_top_var, aes(value, max_week_on_chart))

  # Get overall distribution and linear trends
  + geom_hex(aes(fill = log10( after_stat(count) )))
  + geom_smooth(formula = y ~ x, method = "lm", se = TRUE, size = 1.25)

  # Highlight top 5 songs' characteristics
  + ggrepel::geom_label_repel(
      data = top_K_song_ever,
      aes(label = song_id_clean),
      nudge_x = 1.05,
      # box.padding = 0.5,
      direction = "y",
      max.overlaps = Inf,
      max.iter = 1e6,
      size = 5.5
    )
  + geom_point(data = top_K_song_ever, shape = 23, stroke = 1.5,
               fill = "yellow", color = "black", size = 2.5)

  # Facets, scales, etc
  + facet_wrap( ~ variable_title, ncol = 2, labeller = label_parsed)
  + scale_fill_viridis_c(option = "magma", labels = scales::label_math())
  + scale_x_continuous(limits = c(0, 1.25), breaks = seq(0,1,0.25))
  + labs(
    x = "", y = "Max. weeks on chart", fill = "Counts",
    title = "More **danceable** songs stay in the charts for  longer",
    subtitle = (
      str_c("**Speechiness** seems to have the ",
            "opposite effect, with speechier songs staying less. Highlighted ",
            "are the 5 songs that have stayed the longest in the charts.\n")
      )
    )
  # Fixing text sizes after including shotext::showtext_auto()
  + theme(
    plot.title = element_markdown(hjust = 0.5, family = "lobster", size = 25),
    plot.subtitle = element_textbox_simple(
      size = 15,
      hjust =  0.5,
      lineheight = 0.5,
      margin = ggplot2::margin(b = 10)
    ),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 10)
    )
)
ggsave(
  here::here("azTidyTuesday_20220906_AlvaroMendoza",
             "weeks_on_chart_vs_top_var_lm_hex.png"),
  width = 12,
  height = 12,
  plot = p_lm_hex
)
