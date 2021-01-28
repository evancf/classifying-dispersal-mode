# Packages ---------------------------------------------------------------------

devtools::source_gist("71d758f65261a72ab7dc") # gist for ipak

pkgs <- c("tidyverse", "BIEN", "repmis", "rdryad")

ipak(pkgs)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# Decide on dispersal modes

dispersal_modes <- c("endo", "ant", "attach", "cache",
                     "ballistic", "wind", "water", "unassisted")


# Public data sources ----------------------------------------------------------
# This takes a few minutes to run

# BIEN
BIEN_trait_list()

bien_dat <- BIEN_trait_trait("whole plant dispersal syndrome")
bien_dat <- tibble(sp = bien_dat$scrubbed_species_binomial,
                   mode = bien_dat$trait_value)

# Fricke and Svenning 2020
fs_objects <- repmis::source_data("https://github.com/evancf/network-homogenization/blob/main/analysis/homogenization.RData?raw=T")
rm(list = fs_objects[-which(fs_objects == "net.long")])
fs_dat <- tibble(sp = net.long$plant.accepted.species %>% unique(),
                 mode = "endo",
                 datasource = "fricke_svenning_2020")
rm("net.long")

# I've made excel files with mode and speces from a number of studies
# where data isn't easily accessible online. The dryad r package wasn't
# working for me - something to check back into...
studies <- list.files("./data/mode_data", pattern = "csv", full.names = T)
study_dat <- fs_dat[0,]

for(i in studies){
  dat <- read.csv(i)[,c("sp","mode")]

  study_dat <- rbind(study_dat, dat)

}

# TRY
# Takes a couple minutes. Change to your path for your TRY data release
try_dat <- sqldf::read.csv.sql(file = "data/13238.txt", # Can add sql query to this
                       header = T, sep = "\t")

# First keep only trait data while removing the supporting data
try_dat <- subset(try_dat, TraitName != "")

# Treat TraitName as a factor
try_dat$TraitName <- as.factor(try_dat$TraitName)

# Some initial data manipulations
try_dat <- subset(try_dat, TraitName != "")

# Clean up species names
try_dat$AccSpeciesName <- gsub("species:", "", try_dat$AccSpeciesName)
try_dat$AccSpeciesName <- gsub("species", "", try_dat$AccSpeciesName)
try_dat$AccSpeciesName <- gsub(" x ", " ", try_dat$AccSpeciesName)
try_dat$AccSpeciesName <- gsub(" cf ", " ", try_dat$AccSpeciesName)
try_dat$AccSpeciesName <- gsub(" cf. ", " ", try_dat$AccSpeciesName)

try_dat$AccSpeciesName <- str_replace_all(try_dat$AccSpeciesName, "[^a-zA-Z0-9 -]+", " ") %>% str_squish() %>% tolower() %>% firstup()
try_dat$AccSpeciesName <- ifelse(is.na(word(try_dat$AccSpeciesName, 2)),
                                word(try_dat$AccSpeciesName, 1),
                                word(try_dat$AccSpeciesName, 1, 2))

try_dat <- subset(try_dat, !grepl("nidentified", AccSpeciesName))
try_dat <- subset(try_dat, !grepl("ndetermined", AccSpeciesName))
try_dat <- subset(try_dat, !grepl("nknown", AccSpeciesName))

try_dat$AccSpeciesName[grep("-", try_dat$AccSpeciesName)][1:6] # This keeps the dashes

# Want to only keep dispersal syndrome
try_dat <- filter(try_dat, TraitName == "Dispersal syndrome")

# # There are many dispersal mode values listed
# filter(try_dat, TraitName == "Dispersal syndrome")$OrigValueStr %>% table() %>% as.data.frame() %>% write.csv(., file = "./data/try_syndromes.csv")
#
# syndro <- "wild cat, jaguar"
# filter(try_dat, TraitName == "Dispersal syndrome" & OrigValueStr == syndro)$AccSpeciesName %>% head(1) %>% clipr::write_clip() #

# Will pull in the results of this manual work
try_syndromes_recategorized <-
  read.csv("data/try_syndromes_recategorized.csv",
           row.names = 1)[,c("OrigValueStr", "mode")]

try_dat <- left_join(try_dat, try_syndromes_recategorized)
try_dat[try_dat == ""] <- NA

try_dat <- tibble(sp = try_dat$AccSpeciesName,
                  mode = try_dat$mode) %>% .[complete.cases(.),]

# Lastly put all these together into a mode_dat df
mode_dat <- rbind(fs_dat[,c("sp", "mode")],
                  bien_dat[,c("sp", "mode")],
                  study_dat[,c("sp", "mode")],
                  try_dat[,c("sp", "mode")])

# Clean up species
mode_dat$sp <- ifelse(sapply(strsplit(mode_dat$sp, " "), length) > 2,
                      word(mode_dat$sp, 1, 2),
                      mode_dat$sp)

mode_dat <- mode_dat[!sapply(strsplit(mode_dat$sp, " "), length) == 0,]

mode_dat$sp <- mode_dat$sp %>% tolower() %>% firstup()

# Clean up mode, add a biotic column
biotic_modes <- c("animalnotspecified", "ant", "attach", "biotic",
                  "cache", "endo", "invert", "vertebrate", "vertebratenotspecified")

mode_dat <- mode_dat %>%
  filter(mode != "") %>%
  mutate(biotic = ifelse(mode %in% biotic_modes,1, 0))

mode_dat <- mode_dat[complete.cases(mode_dat[,1:3]),]

mode_dat[sample(1:dim(mode_dat)[1]), ] %>%
  filter(!duplicated(sp)) %>% janitor::tabyl(biotic)


# Will add a genus column
mode_dat <- mode_dat %>% mutate(genus = word(mode_dat$sp, 1))

# Clean up a couple strange values
mode_dat <- mode_dat %>% filter(genus != "")
mode_dat <- mode_dat %>% filter(!grepl(".", genus, fixed = T))
mode_dat <- mode_dat %>% filter(!grepl("??", genus, fixed = T))
mode_dat <- mode_dat %>% filter(!grepl("(", genus, fixed = T))


# Write this to CSV
write.csv(mode_dat, file = "./data/mode_dat.csv")

