source("header.R")
library(ggrepel)

# reading in data -----

ts_extend <- read_csv("build/mst/03_ts_extended.csv") %>% 
  mutate(
    number_of_other_chords = ifelse(song == "DON'T BLAME ME", NA, 
                                    ifelse(song == "I'M ONLY ME WHEN I'M WITH YOU", 1, number_of_other_chords)),
    other_chords = ifelse(song != 'CRUEL SUMMER', other_chords,
                          'iii'),
    minor_key = ifelse(is.na(number_of_other_chords), 1, 0),
    basic_chords = ifelse(n_chords < 5 & number_of_other_chords ==0, "Only I, V, IV, and vi Chords",
                          ifelse(minor_key == 1, NA, "Use Other Chords")),
    basic_chords_plus_ii = ifelse((is.na(other_chords) | other_chords == "ii") & minor_key !=1, "Only I, V, IV, vi, and ii Chords",
                                  ifelse(minor_key == 1, NA, "Use Other Chords"))
  )

ts_extend_no_vids <- ts_extend %>% filter(vid == 0)
View(ts_extend_no_vids)
# generating chordal flags ------

chord_flag <- function (chord, chord_col) {
  
  colname <- str_c(chord, "_flag") %>% str_replace_all("\\-", "_")
  
  if (chord == 'ii') {
    ts_extend %>% select(all_of(c("song", "chorus", chord_col))) %>% 
      # rename(chord_col = get(chord_col)) %>% 
      mutate({{colname}} := ifelse(get(chord_col) == chord | grepl("^ii,|, ii$", get(chord_col)), 
                                   1, 0)) %>% 
      select(song, contains("flag")) 
  } else {ts_extend %>% select(all_of(c("song", chord_col))) %>% 
    # rename(chord_col = get(chord_col)) %>% 
    mutate({{colname}} := grepl(chord, get(chord_col)) %>% as.integer) %>% 
    select(song, contains("flag"))}
  
}

chord_to_an <- data.frame(
  chord = c('ii', 'iii', 'I-V-vi', 'vi-IV', "IV-V", 'I-V-vi-IV'),
  chord_col = c(rep('other_chords', 2), rep('chorus', 4))
)

ts_extend_flags <- ts_extend %>% left_join(pmap(chord_to_an, chord_flag) %>% 
                                           reduce(left_join),
                                     by = c('song')) %>% 
  mutate(across(contains("flag"), ~replace_na(., 0)))

ts_extend_flags %>% write_rds("build/mst/03.1_ts_extended_with_flags.rds")

# grouping data  --------

col_to_compare <- 'basic_chords'
col_to_y <- 'popularity'

labs_xwalk <- data.frame(
  var = c('basic_chords', 'popularity', 'n', 'acousticness'),
  var_name = c('Type of Chords', 'Popularity (Spotify Index)', 'Number of Tracks', 'Acousticness (Spotify Index)')
)

# bar charts --------

palette <- c("#fcba05", "#fc05be", "#7f0b85", "#63ad02", "#d482fa")
names(palette) <- c('Use Other Chords', 
                    'Only I, V, IV, and vi Chords',
                    'Only I, V, IV, vi, and ii Chords',
                    'Present',
                    'Not Present')

make_bars <- function (col_to_compare, col_to_y, chart = 'bar') {

  
  # group
  ts_grouped <- ts_extend_flags %>% # group_by(album) %>% arrange(-popularity) %>% slice_head(n = 5) %>% ungroup %>% 
    group_by(album, release_date, chords = get(col_to_compare)) %>% 
    summarise(across(c(acousticness:duration_ms), ~mean(.)),
              n = n()) %>% ungroup() %>% filter(!is.na(chords)) %>% 
    transform(chords = as.factor(chords))
  
  if (grepl("flag", col_to_compare)) {
    
    if (chart == 'bar' | chart == 'line') {
      ts_grouped <- ts_grouped %>% 
        transform(chords = ifelse(chords == 1, "Present", "Not Present")) 
    } else if (chart == 'box') {
      ts_for_boxplot <- ts_extend_flags%>% 
        rename(chords = all_of(col_to_compare)) %>% 
        transform(chords = ifelse(chords == 1, "Present", "Not Present")) 
    }
    
    legend_title <- col_to_compare %>% str_replace("_flag", " chord(s)") %>% str_replace_all("_", "-")
    scale_fill_custom <- function () {
      scale_fill_manual(values = palette, name = legend_title)
    }
  } else {
    scale_fill_custom <- function () {
      scale_fill_manual(values = palette, name = "")
    }
    
    if (chart == 'box') {
      ts_for_boxplot <- ts_extend_flags%>% 
        rename(chords = all_of(col_to_compare))
    }
  }
  
  
  if (chart == 'bar') {
    
    # bar charts 
    ggplot(ts_grouped, aes(x = reorder(album, release_date), y = get(col_to_y))) + 
      geom_bar(aes(fill = as.factor(chords)), stat = 'identity', position ='dodge') + 
      theme_economist_white(gray_bg = F) + 
      scale_fill_custom() + 
      # scale_fill_manual(values = rev(palette) )+ 
      # scale_fill_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      labs(x = "Album",
           y = labs_xwalk %>% filter(var == col_to_y) %>% pull(var_name))
    
  } else if (chart == 'box') {
    
    # boxplots
    ggplot(ts_for_boxplot %>% filter(minor_key == 0), aes(x = reorder(album, release_date), y = get(col_to_y))) + 
      geom_boxplot(aes(fill = as.factor(chords))) + 
      theme_economist_white(gray_bg = F) + 
      scale_fill_custom() + 
      # scale_fill_manual(values = rev(palette) )+ 
      # scale_fill_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      labs(x = "Album",
           y = labs_xwalk %>% filter(var == col_to_y) %>% pull(var_name))
    
  } else if (chart == 'line') {
    
    # line_charts
    ggplot(ts_grouped, aes(x = release_date, y = get(col_to_y))) + 
      geom_line(aes(group = chords, color = as.factor(chords)), linewidth = 2) + 
      geom_label_repel(data = ts_grouped %>% filter(as.integer(chords) == 1), aes(color = chords, label = album), size = 5, box.padding=0.5) +
      theme_economist_white(gray_bg = F) + 
      scale_color_manual(values = rev(palette)) + 
      # scale_color_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
      theme(legend.title = element_blank(),
            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      labs(x = "Album Release Date", y = labs_xwalk %>% filter(var == col_to_y) %>% pull(var_name))
    
  }
  
}



# ---------
# make_lines <- function (col_to_compare) {
#   
#   palette <- c("#fcba05", "#fc05be")
#   
#   # group
#   ts_grouped <- ts_extend %>% group_by(album, release_date, chords = get(col_to_compare)) %>% 
#     summarise(across(c(acousticness:duration_ms), ~mean(.)),
#               n = n()) %>% ungroup() %>% filter(!is.na(chords)) %>% 
#     transform(chords = as.factor(chords))
#   
#   if (grepl("flag", col_to_compare)) {
#     ts_grouped <- ts_grouped %>% 
#       transform(chords = ifelse(chords == 1, "Present", "Not Present")) 
#     
#     legend_title <- col_to_compare %>% str_replace("_flag", " chord(s)") %>% str_replace_all("_", "-")
#     scale_fill_custom <- function () {
#       scale_fill_manual(values = palette, name = legend_title)
#     }
#   } else {
#     scale_fill_custom <- function () {
#       scale_fill_manual(values = palette, guide = "none")
#     }
#   }
#   
#   # draw
#   
#   ggplot(ts_grouped, aes(x = release_date, y = n)) + 
#     geom_line(aes(group = chords, color = chords), linewidth = 2) + 
#     geom_label_repel(data = ts_grouped %>% filter(as.integer(chords) == 1), aes(color = chords, label = album), size = 3, box.padding=0.5) +
#     theme_economist_white(gray_bg = F) + 
#     scale_color_manual(values = rev(palette)) + 
#     # scale_color_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
#     theme(legend.title = element_blank(),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
#     labs(x = "Album Release Date", y = "Number of Tracks")
# }


# n chords by collection ------------

ggplot(ts_extend_flags, aes(x = n_chords)) + 
  geom_histogram(fill = 'maroon', bins = 10) + facet_wrap(vars(reorder(album, release_date)), ncol = 5) +
  # theme_classic() +
  theme_economist_white(gray_bg = F) + 
  # scale_fill_custom() + 
  # scale_fill_manual(values = rev(palette) )+ 
  # scale_fill_manual(labels = c("other", "I V vi IV"), values = palette) + #, labels = c("other", "I V vi IV")) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  labs(x = "Album",
       y = "Number of Chords")

# rdr -----------

palette <- c("#fc05be", "#fcba05")

col_to_compare <- 'basic_chords'

ts_grouped <- ts_extend_flags %>% group_by(album, chords = get(col_to_compare)) %>% 
  summarise(across(c(acousticness:duration_ms), ~mean(.)),
            n = n()) %>% ungroup() %>% filter(!is.na(chords)) %>% 
  transform(chords = as.factor(chords))

ts_rdr <- ts_grouped %>% #filter(album == 'Red') %>% 
  select(chords, acousticness, danceability, instrumentalness, valence, energy) %>% 
  ggradar(group.colours = palette,
          legend.position = 'bottom',
          background.circle.colour = "white",
          fill = T,
          fill.alpha = 0.25)

ts_rdr
