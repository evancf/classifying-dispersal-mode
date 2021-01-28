library("tidyverse")
library("BIEN")
library("rnaturalearth")
library("rnaturalearthdata")

# First get a list of country names (which we'll eventually map)

country_names <- ne_countries(scale = "medium", returnclass = "sf")$admin

# Note that many of these are wrong but we will treat these as equal
map_to_bien_changes <- c(
  "French Southern and Antarctic Lands" = "French Southern Territories",
  "The Bahamas" = "Bahamas",
  "Northern Cyprus" = "Cyprus", 
  "Republic of Congo" = "Republic of the Congo",
  "Curaçao" = "Curacao",
  "Federated States of Micronesia" = "Micronesia",
  "Guinea Bissau" = "Guinea-Bissau",
  "Hong Kong S.A.R." = "Hong Kong",
  "Macao S.A.R" = "Macao",
  "Pitcairn Islands" = "Pitcairn",
  "South Georgia and South Sandwich Islands" = "South Georgia and the South Sandwich Islands",
  "Somaliland" = "Somalia",
  "Palestine" = "Jordan",
  "Republic of Serbia" = "Serbia",
  "United Republic of Tanzania" = "Tanzania",
  "United States of America" = "United States",
  "United States Virgin Islands" = "U.S. Virgin Islands")

country_names <- plyr::revalue(country_names, map_to_bien_changes)

# Get a list of all species by country

country_lists <- BIEN_list_country(country = "Nepal")[0,]

for(i in country_names){

  dat <- BIEN_list_country(country = i)

  country_lists <- rbind(country_lists, dat)

  print(paste(i, dim(dat)[1]))

}

country_lists <- country_lists %>% mutate(genus = word(country_lists$scrubbed_species_binomial, 1))

country_names[which(!country_names %in% country_lists$country)]

write.csv(country_lists, file = "./data/country_lists.csv")
