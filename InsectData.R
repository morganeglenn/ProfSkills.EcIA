library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

path <- "COMBINED INSECT DATA COPY.xlsx"
events <- read_excel(path, sheet = "Sampling Events")
occ <- read_excel(path, sheet = "Associated Occurrences")

events <- events %>%
  select(eventID, eventDate, samplingProtocol, decimalLatitude, decimalLongitutde) %>%
  mutate(
    eventDate = as.Date(eventDate),
    decimalLongitutde = case_when(
      eventID == "NS-INS-SN-149-AG3" ~ 5.278017,
      eventID == "NS-INS-SN-149-AG4" ~ 5.277717,
      TRUE ~ decimalLongitutde
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

sweeto