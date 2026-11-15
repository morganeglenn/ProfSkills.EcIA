library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#importing and cleaning up data

path <- "COMBINED INSECT DATA COPY.xlsx"
events <- read_excel(path, sheet = "Sampling Events")
occ    <- read_excel(path, sheet = "Associated Occurrences")

events <- events %>%
  select(eventID, eventDate, samplingProtocol, decimalLatitude, decimalLongitude) %>%
  mutate(
    eventDate = as.Date(eventDate),
    decimalLongitude = case_when(
      eventID == "NS-INS-SN-149-AG3" ~ -5.278017,
      eventID == "NS-INS-SN-149-AG4" ~ -5.277717,
      TRUE                           ~ decimalLongitude
    )
  )

occ <- occ %>%
  select(eventID, occurrenceID, commonName, scientificName, organismQuantity, taxonRank) %>%
  fill(eventID) %>%   
  mutate(
    scientificName = str_trim(scientificName),
    scientificName = if_else(scientificName == "Aranae", "Araneae", scientificName),  
    is_tick        = scientificName == "Ixodida",
    protocol = if_else(grepl("-MT-", eventID), "Moth trap", "Sweep/beating"),
    side     = if_else(startsWith(eventID, "NS"), "North", "South"),
    habitat  = case_when(
      protocol == "Moth trap"      ~ NA_character_,
      grepl("-H[0-9]+$",  eventID) ~ "Heathland",
      grepl("-BG[0-9]+$", eventID) ~ "Bog",
      grepl("-W[0-9]+$",  eventID) ~ "Woodland",
      grepl("-B[0-9]+$",  eventID) ~ "Bracken",
      grepl("-AG[0-9]+$", eventID) ~ "Acid grassland",
      grepl("-S[0-9]+$",  eventID) ~ "Scrub"
    )
  ) %>%
  left_join(events %>% select(eventID, eventDate, decimalLatitude, decimalLongitude),
           by = "eventID")
stopifnot(!any(is.na(occ$habitat[occ$protocol == "Sweep/beating"])))
cat("Events in the occurrences sheet but missing from Sampling Events:\n")
print(setdiff(occ$eventID, events$eventID))
cat("Occurrence IDs that appear twice:\n")
occ %>% count(occurrenceID) %>% filter(n > 1) %>% print()

#Sweep net and beating data

sweep <- occ %>% filter(protocol == "Sweep/beating")
point_metrics <- sweep %>%
  distinct(eventID, side, habitat, eventDate, decimalLatitude, decimalLongitude) %>%
  left_join(
    sweep %>% filter(!is_tick) %>% group_by(eventID) %>%
      summarise(individuals = sum(organismQuantity),
                taxa        = n_distinct(scientificName)),
    by = "eventID"
  ) %>%
  left_join(
    sweep %>% filter(is_tick) %>% group_by(eventID) %>%
      summarise(ticks = sum(organismQuantity)),
    by = "eventID"
  ) %>%
  mutate(across(c(individuals, taxa, ticks), ~ replace_na(.x, 0)))

habitat_summary <- point_metrics %>%
  group_by(side, habitat) %>%
  summarise(points           = n(),
            mean_individuals = round(mean(individuals), 1),
            mean_ticks       = round(mean(ticks), 1),
            .groups = "drop")
print(habitat_summary)

top_taxa <- sweep %>%
  group_by(side, scientificName) %>%
  summarise(individuals = sum(organismQuantity), .groups = "drop") %>%
  arrange(side, desc(individuals))
print(top_taxa, n = 20)

# number of individuals per sampling plot (no ticks)

p_sweep <- ggplot(point_metrics, aes(x = habitat, y = individuals)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6) +
  facet_wrap(~ side, scales = "free_x") +
  labs(x = "Habitat", y = "Individuals per sampling point (ticks excluded)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_sweep)
ggsave("insect_sweep_by_habitat.png", p_sweep, width = 8, height = 5, dpi = 300)

#Sampling Map

p_map <- point_metrics %>%
  filter(!is.na(decimalLongitude)) %>%
  ggplot(aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point(aes(size = individuals, colour = habitat), alpha = 0.7) +
  coord_quickmap() +
  labs(x = "Longitude", y = "Latitude", size = "Individuals", colour = "Habitat") +
  theme_minimal()

print(p_map)
ggsave("insect_sampling_map.png", p_map, width = 7, height = 7, dpi = 300)

#Moth data

moths <- occ %>% filter(protocol == "Moth trap")

moth_summary <- moths %>%
  group_by(side) %>%
  summarise(records = n(), taxa = n_distinct(scientificName),
            individuals = sum(organismQuantity))
print(moth_summary)

moth_species <- moths %>%
  filter(taxonRank == "species") %>%
  select(side, scientificName, organismQuantity) %>%
  arrange(side, desc(organismQuantity))
print(moth_species, n = 50)

resolution <- occ %>%
  count(taxonRank) %>%
  mutate(percent = round(100 * n / sum(n), 1))
print(resolution)

filter(!is_tick) %>%
  count(scientificName, wt = organismQuantity, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(scientificName)

#Moth trap species bar graph

p_moth <- ggplot(moth_species, aes(x = reorder(scientificName, organismQuantity),
                                   y = organismQuantity)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ side, scales = "free_y") +
  labs(x = NULL, y = "Individuals") +
  theme_minimal()
print(p_moth)
ggsave("insect_moth_species.png", p_moth, width = 9, height = 6, dpi = 300)