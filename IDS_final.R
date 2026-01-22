# ============================================================
# Music project: Billboard (2000–2023) + MusicOSet
# ============================================================

# 0) Packages (basic data + plots + model)
library(tidyverse)
library(janitor)
library(stringr)
library(scales)
library(corrplot)
library(caret)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(pROC)   # for ROC curve and AUC

theme_set(theme_minimal(base_size = 12))

# colours (red / blue / black)
col_line1 <- "#E41A1C"  # red
col_line2 <- "#377EB8"  # blue
col_line3 <- "#000000"  # black

col_success_fill <- c("NotTop10" = col_line2, "Top10" = col_line1)
col_success_line <- c("NotTop10" = col_line2, "Top10" = col_line1)

# ------------------------------------------------------------
# 1) Helper functions
# ------------------------------------------------------------

tidy_text <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace_all("\\s+", " ")
}

read_guess_delim <- function(path) {
  header_line <- readLines(path, n = 1, warn = FALSE)
  n_tabs  <- str_count(header_line, fixed("\t"))
  n_comma <- str_count(header_line, fixed(","))
  if (n_tabs > n_comma) {
    readr::read_tsv(path, show_col_types = FALSE) %>%
      clean_names()
  } else {
    readr::read_csv(path, show_col_types = FALSE) %>%
      clean_names()
  }
}

save_figure <- function(p, file_name, w = 8, h = 5) {
  dir.create("plots", showWarnings = FALSE)
  ggsave(
    filename = file.path("plots", file_name),
    plot     = p,
    width    = w,
    height   = h,
    dpi      = 220
  )
}

# ------------------------------------------------------------
# 2) Load all data
# ------------------------------------------------------------

bb_raw  <- read_guess_delim("billboard_24years_lyrics_spotify.csv")

art_tbl   <- read_guess_delim("artists.csv")
alb_tbl   <- read_guess_delim("albums.csv")
song_tbl  <- read_guess_delim("songs.csv")
track_tbl <- read_guess_delim("tracks.csv")
rel_tbl   <- read_guess_delim("releases.csv")

song_pop_tbl   <- read_guess_delim("song_pop.csv")
song_chart_tbl <- read_guess_delim("song_chart.csv")
audio_tbl      <- read_guess_delim("acoustic_features.csv")

artist_pop_tbl   <- read_guess_delim("artist_pop.csv")
album_pop_tbl    <- read_guess_delim("album_pop.csv")
artist_chart_tbl <- read_guess_delim("artist_chart.csv")
album_chart_tbl  <- read_guess_delim("album_chart.csv")
lyrics_tbl       <- read_guess_delim("lyrics.csv")

# ------------------------------------------------------------
# 3) Basic checks
# ------------------------------------------------------------

stopifnot("id"      %in% names(bb_raw))
stopifnot("song_id" %in% names(song_pop_tbl))
stopifnot("song_id" %in% names(song_chart_tbl))
stopifnot("song_id" %in% names(audio_tbl))

# ------------------------------------------------------------
# 4) Standardise IDs + key text fields
# ------------------------------------------------------------

bb_raw <- bb_raw %>%
  mutate(
    id          = as.character(id),
    year        = as.integer(year),
    song        = tidy_text(song),
    band_singer = tidy_text(band_singer)
  )

song_tbl       <- song_tbl       %>% mutate(song_id = as.character(song_id))
track_tbl      <- track_tbl      %>% mutate(song_id = as.character(song_id))
song_pop_tbl   <- song_pop_tbl   %>% mutate(song_id = as.character(song_id))
song_chart_tbl <- song_chart_tbl %>% mutate(song_id = as.character(song_id))
audio_tbl      <- audio_tbl      %>% mutate(song_id = as.character(song_id))

# ------------------------------------------------------------
# 5) Clean Billboard table (one row per song-year)
# ------------------------------------------------------------

bb_clean <- bb_raw %>%
  drop_na(
    id, year, ranking,
    danceability, energy, tempo, loudness, valence
  ) %>%
  group_by(id, year) %>%
  summarise(
    ranking          = min(ranking, na.rm = TRUE),
    song             = first(song),
    band_singer      = first(band_singer),
    lyrics           = first(lyrics),
    danceability     = mean(danceability,     na.rm = TRUE),
    energy           = mean(energy,           na.rm = TRUE),
    loudness         = mean(loudness,         na.rm = TRUE),
    speechiness      = mean(speechiness,      na.rm = TRUE),
    acousticness     = mean(acousticness,     na.rm = TRUE),
    instrumentalness = mean(instrumentalness, na.rm = TRUE),
    liveness         = mean(liveness,         na.rm = TRUE),
    valence          = mean(valence,          na.rm = TRUE),
    tempo            = mean(tempo,            na.rm = TRUE),
    duration_ms      = mean(duration_ms,      na.rm = TRUE),
    .groups          = "drop"
  ) %>%
  mutate(
    hit_group = if_else(ranking <= 10, "Top10", "NotTop10"),
    hit_group = factor(hit_group, levels = c("NotTop10", "Top10"))
  )

write_csv(bb_clean, "output_billboard_clean.csv")

# ------------------------------------------------------------
# 6) MusicOSet song‑level summaries
# ------------------------------------------------------------

song_chart_song <- song_chart_tbl %>%
  mutate(week = suppressWarnings(as.Date(week))) %>%
  group_by(song_id) %>%
  summarise(
    best_chart_pos  = min(peak_position,  na.rm = TRUE),
    max_weeks_chart = max(weeks_on_chart, na.rm = TRUE),
    mean_rank_score = mean(rank_score,    na.rm = TRUE),
    .groups         = "drop"
  )

song_pop_latest <- song_pop_tbl %>%
  arrange(song_id, desc(year)) %>%
  distinct(song_id, .keep_all = TRUE)

audio_song <- audio_tbl %>%
  mutate(
    duration_ms = if_else(duration_ms < 0, NA_real_, duration_ms)
  ) %>%
  distinct(song_id, .keep_all = TRUE)

moset_song_data <- song_tbl %>%
  left_join(song_pop_latest, by = "song_id") %>%
  left_join(song_chart_song, by = "song_id") %>%
  left_join(audio_song,      by = "song_id")

write_csv(moset_song_data, "output_musicoset_song_master.csv")

# ------------------------------------------------------------
# 7) Join Billboard with MusicOSet
# ------------------------------------------------------------

full_data <- bb_clean %>%
  left_join(moset_song_data, by = c("id" = "song_id"))

# drop duplicated joined columns
full_data <- full_data %>%
  select(-ends_with(".y"))

write_csv(full_data, "output_master_integrated.csv")

# ------------------------------------------------------------
# 8) Summary tables 
# ------------------------------------------------------------

# ---- Table 1: Clean Descriptive Statistics (one row per variable) ----
numeric_cols <- full_data %>% select(where(is.numeric))

table_summary <- numeric_cols %>%
  summarise(across(
    everything(),
    list(
      Minimum = ~min(.x, na.rm = TRUE),
      Maximum = ~max(.x, na.rm = TRUE),
      Mean    = ~mean(.x, na.rm = TRUE),
      Median  = ~median(.x, na.rm = TRUE),
      StdDev  = ~sd(.x, na.rm = TRUE)
    )
  )) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  ) %>%
  filter(!if_all(Minimum:StdDev, is.na)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(Variable)

readr::write_csv(table_summary, "output_table_descriptive_stats_clean.csv")
message("Table 1 (Descriptive Statistics) saved as output_table_descriptive_stats_clean.csv")
grid.table(table_summary)   # this will appear in the Plots pane

# ---- Table 2 – dataset sizes (pipeline overview) ----
tbl_sizes <- tibble(
  dataset = c(
    "Billboard raw",
    "Billboard cleaned",
    "MusicOSet song table",
    "Joined master"
  ),
  n_rows = c(
    nrow(bb_raw),
    nrow(bb_clean),
    nrow(moset_song_data),
    nrow(full_data)
  )
)

# Ensure final variable names after potential joins
if ("danceability.x" %in% names(full_data)) {
  full_data <- full_data %>%
    rename(
      danceability     = danceability.x,
      energy           = energy.x,
      speechiness      = speechiness.x,
      acousticness     = acousticness.x,
      instrumentalness = instrumentalness.x,
      liveness         = liveness.x
    )
}
if ("loudness.x" %in% names(full_data)) {
  full_data <- full_data %>%
    rename(
      loudness = loudness.x,
      tempo    = tempo.x,
      valence  = valence.x
    )
}
if ("year.x" %in% names(full_data)) {
  full_data <- full_data %>%
    rename(year = year.x)
}
grid.table(tbl_sizes)

# Print in console
print(table_summary, n = nrow(table_summary))
tbl_sizes


png("plots/table_summary.png", width = 900, height = 600)
grid.table(table_summary)
dev.off()

png("plots/tbl_sizes.png", width = 900, height = 600)
grid.table(tbl_sizes)
dev.off()

# ============================================================
# 9) Plots – (Figures 1–10)
# ============================================================

# ---------- Figure 1: two scatter plots ----------
fig1_data <- full_data %>%
  select(hit_group, danceability, energy, popularity) %>%
  pivot_longer(
    cols      = c(danceability, energy),
    names_to  = "feature",
    values_to = "x_value"
  ) %>%
  drop_na(x_value, popularity)

fig1 <- ggplot(fig1_data,
               aes(x = x_value, y = popularity, colour = hit_group)) +
  geom_point(alpha = 0.35, size = 1.2) +
  scale_colour_manual(
    values = col_success_line,
    name   = "Chart success",
    labels = c("NotTop10 (blue)", "Top10 (red)")
  ) +
  facet_wrap(
    ~ feature,
    scales = "free_x",
    labeller = as_labeller(
      c(danceability = "Danceability vs popularity",
        energy       = "Energy vs popularity")
    )
  ) +
  labs(
    title = "Figure 1. Feature–popularity scatterplots",
    x     = "Feature value",
    y     = "Popularity"
  ) +
  theme(legend.position = "top")

save_figure(fig1, "fig1_two_feature_scatter.png", w = 9, h = 5)
print(fig1)

# ---------- Figure 2: feature trends by success ----------
audio_time_success <- full_data %>%
  group_by(year, hit_group) %>%
  summarise(
    danceability = mean(danceability, na.rm = TRUE),
    energy       = mean(energy,       na.rm = TRUE),
    valence      = mean(valence,      na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  pivot_longer(
    cols      = c(danceability, energy, valence),
    names_to  = "feature",
    values_to = "mean_value"
  )

fig2 <- ggplot(audio_time_success,
               aes(x = year, y = mean_value,
                   colour = hit_group, linetype = hit_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_manual(
    values = col_success_line,
    name   = "Chart success",
    labels = c("NotTop10 (blue)", "Top10 (red)")
  ) +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Chart success") +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Figure 2. Feature trends by success",
    x     = "Year",
    y     = "Mean feature value"
  ) +
  theme(legend.position = "top")

save_figure(fig2, "fig2_feature_trends_by_success.png", w = 9, h = 6)
print(fig2)

# ---------- Figure 3: boxplots by success group ----------
audio_long_form <- full_data %>%
  select(hit_group, danceability, energy, tempo, loudness, valence) %>%
  pivot_longer(
    cols      = -hit_group,
    names_to  = "feature",
    values_to = "value"
  ) %>%
  drop_na(value)

fig3 <- ggplot(audio_long_form, aes(hit_group, value, fill = hit_group)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ feature, scales = "free", ncol = 3) +
  scale_fill_manual(
    values = col_success_fill,
    name   = "Chart success",
    labels = c("NotTop10 (blue)", "Top10 (red)")
  ) +
  labs(
    title = "Figure 3. Feature boxplots by success",
    x     = "Chart success",
    y     = "Feature value"
  ) +
  theme(legend.position = "top")

save_figure(fig3, "fig3_feature_boxplots_by_success.png", w = 10, h = 7)
print(fig3)

# ---------- Figure 4: yearly counts by success group ----------
fig4_data <- full_data %>%
  count(year, hit_group)

fig4 <- ggplot(fig4_data,
               aes(x = year, y = n, colour = hit_group, group = hit_group)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_line(position = position_dodge(width = 0.4)) +
  scale_colour_manual(
    values = col_success_line,
    name   = "Chart success",
    labels = c("NotTop10 (blue)", "Top10 (red)")
  ) +
  labs(
    title = "Figure 4. Song counts by year and success",
    x     = "Year",
    y     = "Number of songs"
  ) +
  theme(legend.position = "top")

save_figure(fig4, "fig4_yearly_counts_by_success.png", w = 9, h = 5)
print(fig4)

# ---------- Figure 5: loudness by success ----------
loud_time <- full_data %>%
  group_by(year, hit_group) %>%
  summarise(
    mean_loudness = mean(loudness, na.rm = TRUE),
    .groups       = "drop"
  )

fig5 <- ggplot(loud_time,
               aes(x = year, y = mean_loudness,
                   group = hit_group, colour = hit_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = col_success_line,
    name   = "Chart success",
    labels = c("NotTop10 (blue)", "Top10 (red)")
  ) +
  facet_wrap(~ hit_group, ncol = 1) +
  labs(
    title = "Figure 5. Mean loudness by success",
    x     = "Year",
    y     = "Mean loudness (dB)"
  ) +
  theme(legend.position = "top")

save_figure(fig5, "fig5_loudness_faceted_lines.png", w = 9, h = 6)
print(fig5)

# ---------- Figure 6: correlation matrix ----------
audio_vars <- c(
  "danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)

audio_mat <- full_data %>%
  select(any_of(audio_vars)) %>%
  drop_na()

corr_cols <- colorRampPalette(c(col_line2, "white", col_line1))(200)

op <- par(mar = c(5, 5, 2, 2), oma = c(3, 0, 0, 0))
corrplot(
  cor(audio_mat),
  method      = "color",
  type        = "lower",
  col         = corr_cols,
  tl.col      = "black",
  tl.srt      = 35,
  addCoef.col = NA   # no overlapping numbers
)
mtext("Figure 6. Correlation matrix of audio features",
      side = 1, line = 4, cex = 0.9, font = 1)
par(op)

png("plots/fig6_audio_correlation.png", width = 950, height = 720)
par(mar = c(5, 5, 2, 2), oma = c(3, 0, 0, 0))
corrplot(
  cor(audio_mat),
  method      = "color",
  type        = "lower",
  col         = corr_cols,
  tl.col      = "black",
  tl.srt      = 35,
  addCoef.col = NA
)
mtext("Figure 6. Correlation matrix of audio features",
      side = 1, line = 4, cex = 0.9, font = 1)
dev.off()
par(op)

# ---------- Figure 7: energy–valence dot-line path ----------
mood_time <- full_data %>%
  group_by(year) %>%
  summarise(
    mean_energy  = mean(energy,  na.rm = TRUE),
    mean_valence = mean(valence, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  arrange(year)

fig7 <- ggplot(mood_time,
               aes(x = mean_energy, y = mean_valence)) +
  geom_path(colour = col_line2, linewidth = 1) +
  geom_point(aes(colour = year), size = 2) +
  scale_colour_gradient(low = col_line1, high = col_line3) +
  labs(
    title  = "Figure 7. Mean energy–valence trajectory",
    x      = "Mean energy",
    y      = "Mean valence",
    colour = "Year"
  )

save_figure(fig7, "fig7_mood_trajectory.png", w = 8, h = 6)
print(fig7)

# ---------- Figure 8: lollipop chart of top artists ----------
top_artists <- full_data %>%
  count(band_singer, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  arrange(n) %>%
  mutate(band_singer = factor(band_singer, levels = band_singer))

fig8 <- ggplot(top_artists,
               aes(x = n, y = band_singer)) +
  geom_segment(aes(x = 0, xend = n, y = band_singer, yend = band_singer),
               colour = col_line2, linewidth = 0.8) +
  geom_point(size = 2.5, colour = col_line1) +
  labs(
    title = "Figure 8. Top 20 artists (lollipop)",
    x     = "Number of songs",
    y     = "Artist"
  )

save_figure(fig8, "fig8_top_artists_lollipop.png", w = 9, h = 6)
print(fig8)

# ---------- Figure 9: density plots of audio features by success ----------
hist_long <- full_data %>%
  select(
    hit_group,
    danceability, energy, loudness, speechiness,
    acousticness, instrumentalness, liveness,
    valence, tempo
  ) %>%
  pivot_longer(
    cols      = -hit_group,
    names_to  = "feature",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  mutate(
    feature = factor(
      feature,
      levels = c("danceability","energy","loudness","speechiness",
                 "acousticness","instrumentalness","liveness",
                 "valence","tempo")
    )
  )

fig9 <- ggplot(hist_long, aes(x = value, fill = hit_group, colour = hit_group)) +
  geom_density(alpha = 0.35, adjust = 1.1) +
  scale_fill_manual(
    values = col_success_fill,
    name   = "Chart success",
    labels = c("NotTop10 (blue)", "Top10 (red)")
  ) +
  scale_colour_manual(
    values = col_success_line,
    name   = "Chart success",
    labels = c("NotTop10 (blue)", "Top10 (red)")
  ) +
  facet_wrap(
    ~ feature,
    scales = "free",
    ncol   = 2
  ) +
  labs(
    title = "Figure 9. Feature distributions by success",
    x     = "Feature value",
    y     = "Density"
  ) +
  theme(
    legend.position = "top",
    strip.text      = element_text(size = 9),
    axis.text.x     = element_text(size = 7),
    axis.text.y     = element_text(size = 7),
    plot.title      = element_text(size = 11)
  )

save_figure(fig9, "fig9_feature_densities_by_success.png", w = 9, h = 7)
print(fig9)

# ============================================================
# 10) Logistic regression: model, performance table, ROC curve
# ============================================================

model_data <- full_data %>%
  select(
    hit_group,
    danceability, energy, loudness, tempo, valence
  ) %>%
  drop_na() %>%
  mutate(hit_group = factor(hit_group, levels = c("NotTop10", "Top10")))

set.seed(123)
idx_split <- caret::createDataPartition(
  model_data$hit_group,
  p    = 0.8,
  list = FALSE
)
train_set <- model_data[idx_split, ]
test_set  <- model_data[-idx_split, ]

log_mod <- glm(
  hit_group ~ danceability + energy + loudness + tempo + valence,
  data   = train_set,
  family = binomial()
)

prob_hat <- predict(log_mod, newdata = test_set, type = "response")
pred_label <- if_else(prob_hat > 0.5, "Top10", "NotTop10") %>%
  factor(levels = c("NotTop10", "Top10"))

cmat <- caret::confusionMatrix(pred_label, test_set$hit_group, positive = "Top10")

logit_perf_tbl <- tibble(
  Model       = "Logistic regression",
  Accuracy    = cmat$overall["Accuracy"],
  Kappa       = cmat$overall["Kappa"],
  Sensitivity = cmat$byClass["Sensitivity"],
  Specificity = cmat$byClass["Specificity"],
  Precision   = cmat$byClass["Pos Pred Value"],
  Recall      = cmat$byClass["Sensitivity"]
)

write_csv(logit_perf_tbl, "output_logit_performance_table.csv")
grid.table(
  logit_perf_tbl,
  rows = NULL,
  theme = ttheme_default(
    core    = list(fg_params = list(cex = 0.8)),
    colhead = list(fg_params = list(cex = 0.9))
  )
)

png("plots/logit_performance_table.png",
    width = 1600, height = 800, res = 220)
grid.table(
  logit_perf_tbl,
  rows = NULL,
  theme = ttheme_default(
    core    = list(fg_params = list(cex = 0.8)),
    colhead = list(cex = 0.9)
  )
)
dev.off()

roc_obj <- roc(
  response  = test_set$hit_group,
  predictor = prob_hat,
  levels    = c("NotTop10", "Top10"),
  direction = "<"
)
auc_val <- auc(roc_obj)

roc_data <- tibble(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

fig10 <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
  geom_line(colour = col_line2, linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey60") +
  coord_equal() +
  labs(
    title = "Figure 10. ROC curve for hit prediction",
    x     = "False positive rate",
    y     = "True positive rate"
  )

save_figure(fig10, "fig10_logit_roc.png", w = 7, h = 6)
print(fig10)

sink("output_model_summary.txt")
cat("=== Logistic regression summary ===\n")
print(summary(log_mod))
cat("\n=== Confusion matrix ===\n")
print(cmat)
cat("\n=== AUC ===\n")
print(auc_val)
sink()

message("Finished: CSVs in working folder, plots in /plots, model summary + performance table saved.")