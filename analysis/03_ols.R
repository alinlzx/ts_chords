source("header.R")

ts_extend_flags <- read_rds("build/mst/03.1_ts_extended_with_flags.rds")

ts_lm <- lm(popularity ~ as.factor(album) + as.factor(vid) + as.factor(basic_chords), data = ts_extend_flags %>% filter(release_date > as.Date("2020-1-1")))

summary(ts_lm)

# plotting

for_chart <- ts_extend_flags %>% 
  group_by(album) %>% 
  mutate(popularity_iqr = quantile(popularity, 0.75) - quantile(popularity, 0.25),
         popularity_mean = mean(popularity),
         label = ifelse(popularity > popularity_mean + 1.5*popularity_iqr | popularity < popularity_mean - 3*popularity_iqr, T, F),
         vid = ifelse(vid == 1, "Music Video", "No Music Video"),
         across(contains("basic_chords"), ~ifelse(minor_key == 1, "Minor Key", .))) %>% 
  ungroup()

palette <- c("#fcba05", "#fc05be", "#7f0b85", "#0b8585", "#db6f0f", "#fa5050", "#7250fa", 'lightgreen')
names(palette) <- c('Use Other Chords', 
                    'Only I, V, IV, and vi Chords',
                    'Only I, V, IV, vi, and ii Chords',
                    'Present',
                    'Not Present',
                    'Music Video',
                    'No Music Video',
                    'Minor Key')

ggplot() + 
  geom_point(data = for_chart, aes(x = release_date, y = popularity, color = as.factor(vid)), size = 3) + 
  theme_economist_white(gray_bg = F) + 
  labs(x = "Release Date", y = "Popularity (Spotify Index)") +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.title = element_blank()) +
  scale_color_manual(values = palette) +
  geom_label_repel(data = for_chart %>% filter(label == T), aes(x = release_date, y = popularity, label = song, color = as.factor(vid)), size = 3, box.padding = 1)
