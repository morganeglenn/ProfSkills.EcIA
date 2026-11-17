library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

path <- "event_ipt_MamGroup.xlsx"
threshold_min <- 30
events <- read_excel(path, sheet = "Sampling Events")
occ <- read_excel(path, sheet = "Associated Occurences")

stopifnot(ncol(events) ==  15, ncol(occ) == 16)

names(events) <- c("eventID", "eventDate", "timing", "cameraID", "protocol", "effort_text", "sampleSizeValue", "sampleSizeUnit", "hill", "lat", "lon", "datum", "altitude", "locationNotes", "countryCode")
names(occ) <- c("eventID", "occurrenceID", "basisOfRecord", "eventDate", "clock_time",
                "airTemp", "dayOrNight", "commonName", "scientificName", "individuals",
                "habitat", "imageID", "groupID", "lat", "lon", "direction")

parse_effort <- function(x){
  frac <- str_extract(x, "^[0-9]+(/[0-9]+)?")
  sapply(strsplit(frac, "/"), function(p) {
    p <- as.numeric(p)
    if (length(p) == 2) p[1] / p[2] else p[1]
    })
}

events <- events %>%
  mutate(
    eventID = str_trim(eventID),
    date = as.Date(eventDate, format = "%d/%m/%y"),
    camera_days = parse_effort(effort_text), 
    side = if_else(startsWith(eventID, "NS"), "North", "South"),
    lon = -abs(lon)
  )

print(events %>% select(eventID, date, cameraID, side, camera_days, lat, lon))

occ <- occ %>%
  filter(!is.na(eventID)) %>%
  select(eventID, occurrenceID, eventDate, clock_time, commonName, scientificName,
         individuals, habitat, imageID) %>%
  mutate(
    eventID = str_trim(eventID),
    commonName = str_trim(commonName),
    scientificName = str_trim(scientificName), 
    clock_time = format(clock_time, "%H:%M:%S", tz = "UTC"),
    datetime = as.POSIXct(paste(eventDate, clock_time),
                          format = "%d/%m/%y %H:%M:%S", tz = "UTC")
  ) %>%
  left_join(events %>% select(eventID, cameraID, side), by = "eventID")

cat("Events in the occurrences sheet but not in Sampling Events :/n")
print(setdiff(occ$eventID, events$eventID))
cat("Events with no photos (still count towards effort):\n")
print(setdiff(events$eventID, occ$eventID))
cat("Occurrence IDs used for more than one photo :\n")
occ %>% count(occurrenceID) %>% filter (n>1) %>% print()

detections <- occ %>%
  arrange(cameraID, scientificName, datetime) %>%
  group_by(cameraID, scientificName) %>%
  mutate(gap_min       = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
         new_detection = is.na(gap_min) | gap_min > threshold_min,
         detection_id  = cumsum(new_detection)) %>%
  group_by(cameraID, side, commonName, scientificName, habitat, detection_id) %>%
  summarise(start       = min(datetime),
            photos      = n(),
            max_animals = max(individuals),
            .groups = "drop") %>%
  mutate(hour = as.numeric(format(start, "%H")) + as.numeric(format(start, "%M")) / 60)

cat("Photos:", nrow(occ), " Independent detections:", nrow(detections), "\n")

camera_effort <- events %>%
  group_by(cameraID, side, lat, lon) %>%
  summarise(camera_days = sum(camera_days), .groups = "drop")

camera_summary <- camera_effort %>%
  left_join(detections %>% group_by(cameraID) %>%
              summarise(detections = n(), species = n_distinct(scientificName)),
            by = "cameraID") %>%
  mutate(across(c(detections, species), ~ replace_na(.x, 0)),
         per_100_camera_days = round(100 * detections/camera_days, 1))
print(camera_summary)

side_effort <- events %>% group_by(side) %>% summarise(camera_days = sum(camera_days))

# detections per 100 camera days

rai <- detections %>%
  group_by(side, commonName) %>%
  summarise(detections = n(), .groups = "drop") %>%
  complete(side, commonName, fill = list(detections = 0)) %>%
  left_join(side_effort, by = "side") %>%
  mutate(per_100_camera_days = round(100 * detections/camera_days, 1))
print(rai)

p_rai <- ggplot(rai, aes(x = commonName, y = per_100_camera_days, fill = side)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Detections per 100 camera-days", fill = "Side") + 
  theme_minimal()
print(p_rai)
ggsave("mammal_detection_rate.png", p_rai, width = 7, height = 5, dpi = 300)

#time of day of each detection

p_time <- ggplot(detections, aes(x = hour, y = commonName, colour = side)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
  labs(x = "Hour of day", y = NULL, colour = "Side") +
  theme_minimal()
print(p_time)
ggsave("mammal_time_of_day.png", p_time, width = 7, height = 4, dpi = 300)

# camera locations

p_map <- ggplot(camera_summary, aes(x = lon, y = lat)) +
  geom_point(aes(size = detections, colour = side), alpha = 0.7) +
  geom_text(aes(label = cameraID), vjust = -1.5, size = 3) +
  coord_quickmap() +
  labs(x = "Longitutde", y = "Latitude", size = "Detections", colour = "Side") +
  theme_minimal()
print(p_map)
ggsave("mammal_camera_map.png", p_map, width = 6, height = 6, dpi = 300)
