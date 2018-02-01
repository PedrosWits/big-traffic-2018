library(tidyverse)

pattern = "^(([A-Za-z]{1,2}[ ]?[0-9]{1,4})|([A-Za-z]{3}[ ]?[0-9]{1,3})|([0-9]{1,3}[ ]?[A-Za-z]{3})|([0-9]{1,4}[ ]?[A-Za-z]{1,2})|([A-Za-z]{3}[ ]?[0-9]{1,3}[ ]?[A-Za-z])|([A-Za-z][ ]?[0-9]{1,3}[ ]?[A-Za-z]{3})|([A-Za-z]{2}[ ]?[0-9]{2}[ ]?[A-Za-z]{3})|([A-Za-z]{3}[ ]?[0-9]{4}))$"

valid_days = c(6,7,8,9,10,13,14,15,16,17,29,21,22,23,24)
weekends = c(4,5,11,12,18,19,25,26)

pre_npdata = 
  readr::read_csv(file = "data/npdata-Feb-2017.csv",
                  col_names = c("CameraId", "Timestamp", "Confidence", "Tag"),
                  col_types = "cTic",
                  na = c("NULL", "NA"),
                  progress = TRUE,
                  skip = 1)

data_per_day = 
  pre_npdata %>%
  mutate(Day = lubridate::day(Timestamp)) %>%
  group_by(Day) %>%
  summarise(Sightings = n()) %>%
  filter(!is.na(Day)) %>%
  mutate(Type = ifelse(Day %in% weekends, "Weekend", "Weekday")) %>%
  mutate(Type = as.factor(Type))

data_per_camera_per_day = 
  pre_npdata %>%
  mutate(Day = lubridate::day(Timestamp)) %>%
  group_by(CameraId, Day) %>%
  summarise(Sightings = n()) %>%
  filter(!is.na(Day)) %>%
  mutate(Type = ifelse(Day %in% weekends, "Weekend", "Weekday")) %>%
  mutate(Type = as.factor(Type))

obs_day_plot = 
  ggplot() +
  geom_line(data = data_per_day, 
            aes(x = Day, y = Sightings),
            color = "#5088C9",
            size = 1.10) +
  geom_point(data = data_per_day,
             aes(x = Day, y = Sightings, color = Type, shape = Type),
             size = 3,
             fill = "white",
             stroke = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = pretty(data_per_day$Day, n=28), minor_breaks = NULL) +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.08),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        axis.line.x = element_blank())

ggsave(file = "graphs/observations_per_day.pdf", 
       plot = obs_day_plot, 
       device = "pdf", 
       dpi = "retina",
       width = 8,
       height = 5)

camera_most_sightings = data_per_camera_per_day %>% group_by(CameraId) %>% summarise(Sightings = sum(Sightings)) %>% arrange(desc(Sightings)) %>% head(1) %>% pull(CameraId)

data_per_camera_per_day_cumsum = data_per_camera_per_day %>% group_by(CameraId) %>% mutate(CumulativeSightings = cumsum(Sightings))

obs_day_camera_plot = 
  ggplot() +
  geom_line(data = data_per_camera_per_day_cumsum, 
            aes(x = Day, y = Sightings, group = CameraId), alpha = 0.2) +
  geom_point(data = data_per_camera_per_day_cumsum %>% filter(CameraId == camera_most_sightings),
             aes(x = Day, y = Sightings, color = Type, shape = Type),
             fill = "white",
             size = 2,
             stroke = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = pretty(data_per_day$Day, n=28), minor_breaks = NULL) +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        axis.line.x = element_blank())

obs_day_camera_plot

ggsave(file = "graphs/observations_per_day_camera.pdf", 
       plot = obs_day_camera_plot, 
       device = "pdf",
       dpi = "retina",
       width = 8,
       height = 5)

# Pipeline
#   1. Filter bad number plates
#   2. Choose 3 first complete weeks (only week days) of February
#   3. Remove all sightings with confidence < 0.80
#   4. Arrange by timestamp

np_data =
  pre_npdata %>%
  filter(grepl(pattern, Tag)) %>%
  mutate(Day = lubridate::day(Timestamp)) %>%
  filter(Day %in% valid_days) %>%
  filter(Confidence >= 85) %>%
  arrange(Timestamp) %>%
  select(-c(Day, Confidence))

system("say Computed number plate dataset")

# Release raw data from memory
rm(pre_npdata)

save(np_data, file = "data/np_data.RData")