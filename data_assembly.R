# Packages ---------------------------------------------------------------------

devtools::source_gist("71d758f65261a72ab7dc") # gist for ipak

pkgs <- c("tidyverse", "BIEN", "repmis", "rdryad")

ipak(pkgs)


# Decide on some things

dispersal_modes <- c("endo", "ant", "attach", "cache",
                     "ballistic", "wind", "water", "passive")


# Public data sources ----------------------------------------------------------

# BIEN
BIEN_trait_list()

bien_dat <- BIEN_trait_trait("whole plant dispersal syndrome")
dim(bien_dat)

# Sinnott-Armstrong et al. 2018

# Having a hard time pulling this from Dryad with the rdryad package...
sa_dat <- read.csv("./data/Sinnott-Armstrong et al. 2019.csv")

# A couple other data sets




# Fricke and Svenning 2020

fs_objects <- repmis::source_data("https://github.com/evancf/network-homogenization/blob/main/analysis/homogenization.RData?raw=T")
rm(list = fs_objects[-which(fs_objects == "net.long")])

fs_dat <- tibble(sp = net.long$plant.accepted.species %>% unique(),
                 mode = "endo",
                 datasource = "fricke_svenning_2020") 
rm("net.long")


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

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

try_dat$AccSpeciesName <- str_replace_all(try_dat$AccSpeciesName, "[^a-zA-Z0-9 -]+", " ") %>% str_squish() %>% tolower() %>% firstup()
try_dat$AccSpeciesName <- ifelse(is.na(word(try_dat$AccSpeciesName, 2)),
                                word(try_dat$AccSpeciesName, 1),
                                word(try_dat$AccSpeciesName, 1, 2))

try_dat <- subset(try_dat, !grepl("nidentified", AccSpeciesName))
try_dat <- subset(try_dat, !grepl("ndetermined", AccSpeciesName))
try_dat <- subset(try_dat, !grepl("nknown", AccSpeciesName))

try_dat$AccSpeciesName[grep("-", try_dat$AccSpeciesName)][1:6] # This keeps the dashes

# There are many dispersal mode values listed
filter(try_dat, TraitName == "Dispersal syndrome")$OrigValueStr %>% table() %>% as.data.frame() %>% write.csv(., file = "./data/try_syndromes.csv")

syndro <- "Melanerpes carolinus; Odocoileus virginianus clavium"
filter(try_dat, TraitName == "Dispersal syndrome" & OrigValueStr == syndro)$AccSpeciesName #%>% head()


# Get vector of trait names
trait.names <- levels(factor(try_dat$TraitName))


filter(try_dat, TraitName == "Dispersal syndrome" & OrigValueStr == "nautochor") %>% head()
filter(try_dat, TraitName == "Dispersal syndrome")$OrigValueStr %>% table()
filter(try_dat, TraitName == "Dispersal unit appendages")$OrigValueStr %>% table()


# Next, make this a wide database with ObservationID as the

colnames(try_dat)
try.long <- try_dat[,c("AccSpeciesName", "ObservationID", "TraitName", "StdValue")]

try.long$StdValue <- as.numeric(try.long$StdValue)

try.wide <- try.long %>%
  group_by(AccSpeciesName, ObservationID, TraitName, ObservationID) %>%
  summarize(mean(StdValue))

colnames(try.wide)[which(colnames(try.wide) == "mean(StdValue)")] <- "StdValue"


try.wide <- spread(try.wide,
                   key = TraitName,
                   value = StdValue)




