source("header.R")

# read in data -------------

tswift_disco <- read.csv("build/raw/taylor_swift_spotify.csv")

tv <- tswift_disco %>% 
  filter(grepl("(Taylor's Version)", album)) %>% select(name, popularity_tv = popularity) %>% 
  transform(name = str_replace(name, "\\s\\([A-Za-z\\s']+\\)", "")) %>% 
  group_by(name) %>% slice_max(popularity_tv)

tswift_disco_cleaned <- tswift_disco %>% 
  transform(name = ifelse(name == 'I Knew You Were Trouble.', "I Knew You Were Trouble", name)) %>% 
  filter(!grepl('\\(|\\[|\\:|World|Tour|Live|Edition', album)) %>% 
  left_join(tv) %>% 
  transform(name  = toupper(name)) %>% rowwise %>% 
  mutate(popularity = max(popularity, popularity_tv, na.rm = T)) %>% 
  select(-popularity_tv) %>% distinct()

ts_chords <- read.csv("build/mst/01_ts_chords.csv")

# Joining data ---------------

filter_ts_ch <- function (t) {
  
  chord_col = str_c(t, "_type")
  for_j <- ts_chords %>% filter(Type == t) %>% 
    transmute(name = toupper(Song), {{chord_col}} := Chord)
  
}

ch_list <- map(c('Axis', 'B', '50s', 'D', 'E'), filter_ts_ch) %>% reduce(full_join) 

ts_all <- tswift_disco_cleaned %>% fuzzy_join(ch_list, match_fun = stringr::str_detect, mode = 'full') %>% 
  mutate(across(contains("type"), ~ifelse(!is.na(.), T, F))) %>% 
  select(-name.y) %>% rename(song = name.x) %>% na.omit

ts_all %>% write_rds("build/mst/02_ts_all.rds")
ts_all %>% write.csv("build/mst/02_ts_all.csv")
