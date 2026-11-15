library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

path <- "Excel Bog.xlsx"
events <- read_excel(path, sheet = "Sampling Events")
occ <- read_excel(path, sheet = "Associated Occurrences")

events <- events %>%
  mutate(
    eventID = trimws(eventID),
    side = if_else(startsWith(eventID, "NS"), "North", "South"), 
    habitat = case_when(grepl("Pond", samplingProtocol) ~ "Bog",
                        grepl("Kick", samplingProtocol) ~ "Stream")
  ) %>%
  filter(!is.na(habitat))

occ <- occ %>%
  filter(!is.na(occurrenceID)) %>%
  fill(eventID) %>%
  filter(occurrenceStatus == "present") %>%
  mutate(
    eventID = trimws(eventID),
    individualCount = as.numeric(individualCount), 
    scientificName = if_else(scientificName == "Ephermera", "Ephemeroptera", scientificName),
    order = case_when(
      scientificName %in% c("Anisoptera", "Zygoptera") ~"Odonata", 
      scientificName %in% c("Coleoptera", "Dytiscidae", "Dytiscus marginalis")~ "Coleoptera",
      TRUE ~ scientificName
    )
  )

site_metrics <- events %>%
  left_join(
    occ %>% group_by(eventID) %>%
      summarise(abundance = sum(individualCount), richness = n_distinct(order)),
    by = "eventID"
  ) %>%
  mutate(abundance = replace_na(abundance, 0),
         richness = replace_na(richness, 0)) %>%
  select(eventID, eventDate, habitat, side, abundance, richness)

print(site_metrics)

site_metrics %>%
  group_by(habitat, side) %>%
  summarise(sites = n(), total = sum(abundance), mean_per_site = mean(abundance),
            .groups = "drop") %>%
  print()

composition <- occ %>%
  left_join(events %>% select(eventID, habitat, side), by = "eventID") %>%
  group_by(habitat, side, scientificName) %>%
  summarise(individuals = sum(individualCount), sites = n_distinct(eventID),
            .groups = "drop")
print(composition)

p <- ggplot(occ %>% left_join(events %>% select(eventID, habitat), by = "eventID"),
            aes(x = eventID, y = individualCount, fill = scientificName)) +
  geom_col()+
  facet_grid(. ~habitat, scales = "free_x", space = "free_x")+
  labs(x = "Sampling event", y = "Individuals", fill = "Taxon") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

ggsave("bog_composition.png", p, width = 9, height = 5, dpi = 300
)
