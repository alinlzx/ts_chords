source("header.R")

# Reading in TS BUILD ---------------

ts_all <- readRDS("build/mst/02_ts_all.rds") %>% rowwise %>% 
  transform(fc = ifelse(Axis_type + B_type + `50s_type` + D_type + E_type > 0, 'Four Chords', "Other")) %>% 
  filter(album != "THE TORTURED POETS DEPARTMENT")

# Visualizations --------------------

palette <- c("#fcba05", "#fc05be")

col_to_compare <- 'fc'

# popularity
ts_grouped <- ts_all %>% group_by(album, release_date, chords = get(col_to_compare)) %>% 
  summarise(across(where(is.numeric), ~mean(.)),
            n = n()) %>% ungroup()

ggplot(ts_grouped, aes(x = reorder(album, acousticness), y = acousticness)) + 
  geom_bar(aes(fill = chords), stat = 'identity', position ='dodge') + 
  theme_economist_white(gray_bg = F) + 
  scale_fill_manual(values = rev(palette) )+ 
  # scale_fill_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  labs(x = "Album")

ggplot(ts_grouped, aes(x = release_date, y = n)) + 
  geom_line(aes(group = chords, color = chords), linewidth = 2) + 
  geom_label(aes(color = chords, label = album, size = popularity)) +
  theme_economist_white(gray_bg = F) + 
  scale_color_manual(values = rev(palette)) + 
  # scale_color_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  labs(x = "Date", y = "Number of Tracks")


ggplot(ts_all, aes(x = acousticness, y = popularity)) + 
  geom_point(aes(color = fc)) +
  theme_economist_white(gray_bg = F) + 
  scale_color_manual(values = rev(palette)) + 
  # geom_label(data = ts_all %>% filter(popularity > 85), aes(x = acousticness, y = popularity, label = song)) +
  # scale_color_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) 

ts_rdr <- ts_grouped %>% filter(album == 'evermore') %>% 
  select(chords, acousticness, danceability, instrumentalness, liveness, speechiness, valence) %>% 
  ggradar()

ts_rdr


ts_most_pop <- ts_all %>% arrange(-popularity) %>% 
  select(album, song, contains("type"), fc)

# ML --------------------------------------

# OLS ------------------------------------

ts_lm <- lm(popularity ~ acousticness + danceability + energy + instrumentalness + 
              liveness + valence + loudness + speechiness + duration_ms + tempo + 
              Axis_type + B_type + X50s_type + D_type + E_type + as.factor(album), data = ts_all)

summary(ts_lm)
  