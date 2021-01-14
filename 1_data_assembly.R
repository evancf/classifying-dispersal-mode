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


# # Public data sources --------------------------------------------------------
# # NOTE that I'll read the result of this chunk in, as it takes maybe an hour or so to run
# 
# # BIEN
# BIEN_trait_list()
# 
# bien_dat <- BIEN_trait_trait("whole plant dispersal syndrome")
# bien_dat <- tibble(sp = bien_dat$scrubbed_species_binomial,
#                    mode = bien_dat$trait_value)
# 
# # Fricke and Svenning 2020
# fs_objects <- repmis::source_data("https://github.com/evancf/network-homogenization/blob/main/analysis/homogenization.RData?raw=T")
# rm(list = fs_objects[-which(fs_objects == "net.long")])
# fs_dat <- tibble(sp = net.long$plant.accepted.species %>% unique(),
#                  mode = "endo",
#                  datasource = "fricke_svenning_2020") 
# rm("net.long")
# 
# # I've made excel files with mode and speces from a number of studies
# # where data isn't easily accessible online. The dryad r package wasn't
# # working for me - something to check out.
# studies <- list.files("./data/mode_data", pattern = "csv", full.names = T)
# study_dat <- fs_dat[0,]
# 
# for(i in studies){
#   dat <- read.csv(i)[,c("sp","mode")]
#   
#   study_dat <- rbind(study_dat, dat)
#   
# }
# 
# 
# 
# # TRY
# 
# # Takes a couple minutes. Change to your path for your TRY data release
# try_dat <- sqldf::read.csv.sql(file = "data/13238.txt", # Can add sql query to this
#                        header = T, sep = "\t")
# 
# 
# # First keep only trait data while removing the supporting data
# try_dat <- subset(try_dat, TraitName != "")
# 
# # Treat TraitName as a factor
# try_dat$TraitName <- as.factor(try_dat$TraitName)
# 
# # Some initial data manipulations
# try_dat <- subset(try_dat, TraitName != "")
# 
# # Clean up species names
# try_dat$AccSpeciesName <- gsub("species:", "", try_dat$AccSpeciesName)
# try_dat$AccSpeciesName <- gsub("species", "", try_dat$AccSpeciesName)
# try_dat$AccSpeciesName <- gsub(" x ", " ", try_dat$AccSpeciesName)
# try_dat$AccSpeciesName <- gsub(" cf ", " ", try_dat$AccSpeciesName)
# try_dat$AccSpeciesName <- gsub(" cf. ", " ", try_dat$AccSpeciesName)
# 
# 
# try_dat$AccSpeciesName <- str_replace_all(try_dat$AccSpeciesName, "[^a-zA-Z0-9 -]+", " ") %>% str_squish() %>% tolower() %>% firstup()
# try_dat$AccSpeciesName <- ifelse(is.na(word(try_dat$AccSpeciesName, 2)),
#                                 word(try_dat$AccSpeciesName, 1),
#                                 word(try_dat$AccSpeciesName, 1, 2))
# 
# try_dat <- subset(try_dat, !grepl("nidentified", AccSpeciesName))
# try_dat <- subset(try_dat, !grepl("ndetermined", AccSpeciesName))
# try_dat <- subset(try_dat, !grepl("nknown", AccSpeciesName))
# 
# try_dat$AccSpeciesName[grep("-", try_dat$AccSpeciesName)][1:6] # This keeps the dashes
# 
# # Want to only keep dispersal syndrome
# try_dat <- filter(try_dat, TraitName == "Dispersal syndrome")
# 
# # # There are many dispersal mode values listed
# # filter(try_dat, TraitName == "Dispersal syndrome")$OrigValueStr %>% table() %>% as.data.frame() %>% write.csv(., file = "./data/try_syndromes.csv")
# # 
# # syndro <- "wild cat, jaguar"
# # filter(try_dat, TraitName == "Dispersal syndrome" & OrigValueStr == syndro)$AccSpeciesName %>% head(1) %>% clipr::write_clip() #
# 
# # Will pull in the results of this manual work
# try_syndromes_recategorized <-
#   read.csv("data/try_syndromes_recategorized.csv", 
#            row.names = 1)[,c("OrigValueStr", "mode")]
# 
# try_dat <- left_join(try_dat, try_syndromes_recategorized)
# try_dat[try_dat == ""] <- NA
# 
# try_dat <- tibble(sp = try_dat$AccSpeciesName,
#                   mode = try_dat$mode) %>% .[complete.cases(.),]
# 
# 
# # Lastly put all these together
# 
# mode_dat <- rbind(fs_dat[,c("sp", "mode")], 
#                   bien_dat[,c("sp", "mode")], 
#                   study_dat[,c("sp", "mode")],
#                   try_dat[,c("sp", "mode")])
# 
# 
# # Clean up species
# mode_dat$sp <- ifelse(sapply(strsplit(mode_dat$sp, " "), length) > 2,
#                       word(mode_dat$sp, 1, 2),
#                       mode_dat$sp)
# 
# mode_dat <- mode_dat[!sapply(strsplit(mode_dat$sp, " "), length) == 0,]
# 
# mode_dat$sp <- mode_dat$sp %>% tolower() %>% firstup()
# 
# # Clean up mode, add a biotic
# biotic_modes <- c("animalnotspecified", "ant", "attach", "biotic", 
#                   "cache", "endo", "invert", "vertebrate", "vertebratenotspecified")
# 
# mode_dat <- mode_dat %>% 
#   filter(mode != "") %>% 
#   mutate(biotic = ifelse(mode %in% biotic_modes,1, 0))
# 
# mode_dat <- mode_dat[complete.cases(mode_dat[,1:3]),]
# 
# mode_dat[sample(1:dim(mode_dat)[1]), ] %>% 
#   filter(!duplicated(sp)) %>% janitor::tabyl(biotic)
# 
# 
# # Want to get families for each genus.
# 
# mode_dat <- mode_dat %>% mutate(family = NA)
# sp_vec <- unique(mode_dat$sp)
# for(i in sp_vec[14526:length(sp_vec)]){
# 
#   inds <- which(mode_dat$sp == i)
# 
#   dat <- taxize::pow_search(i)
# 
#   if(!is.null(dim(dat$data))){
#     mode_dat$family[inds] <- dat$data$family %>% table() %>% sort(decreasing = T) %>% names() %>% .[1]
#   }
# 
#   counter <- which(sp_vec == i)
#   if(counter %in% (seq(1,30000, length.out = 100) %>% round())){
#     print(paste(counter, "of ~", 27000))
#   }
# 
# }
# 
# 
# 
# # Clean up families # NOTE THAT WE SHOULD USE taxonlookup::lookuptable()
# mode_dat <- mode_dat %>% mutate(genus = word(sp, 1))
# 
# gen_vec <- unique(mode_dat$genus)
# for(i in gen_vec){
# 
#   inds <- which(mode_dat$genus == i)
# 
#   families <- filter(mode_dat, genus == i)$family %>% na.omit() %>% as.vector()
#   
#   if(length(families) == 0){
#     
#     families1 <- taxize::pow_search(i)$data$family
#     
#     if(length(families1) == 0){
#       print(paste("need to assign family for -----", i))
#       next()
#     } else{
#       mode_dat$family[inds] <- families1 %>% table() %>% sort(decreasing = T) %>% names() %>% .[1]
#     }
# 
#   }
#   
#   families_equal <- length(unique(families)) == 1
#   
#   if(families_equal){
#     mode_dat$family[inds] <- families[1]
#   }
# 
#   if(!families_equal) {
#     
#     families2 <- taxize::pow_search(i)$data$family
#     
#     mode_dat$family[inds] <- families2 %>% table() %>% sort(decreasing = T) %>% names() %>% .[1]
#     
#     #print(paste(i, "==", families %>% table() %>% sort(decreasing = T) %>% names() %>% .[1]))
#     
#   }
# 
# }
# mode_dat$family %>% is.na() %>% table() 
# # This is a really tiny percentage, but ideally these would be corrected.
# # For this first pass, will simply omit them
# 
# mode_dat <- mode_dat %>% filter(complete.cases(mode_dat))
# 
# write.csv(mode_dat, file = "./data/mode_dat.csv")

mode_dat <- read.csv("./data/mode_dat.csv", row.names = 1) %>% tibble





mode_dat <- mode_dat %>% filter(!genus == "")

group_dat <- tibble(genus = unique(mode_dat$genus),
                    group = NA)

intervals <- seq(1, length(group_dat$genus), length.out = 100) %>% round()
for(i in 1:(length(intervals)-1)){
  
  int <- intervals[i]:(intervals[i+1])
  
  group_dat$group[int] <- taxonlookup::lookup_table(group_dat$genus[int], 
                                                    by_species = T)$group
  
  print(paste(i, "of", length(intervals)))
  
}

mode_dat <- left_join(mode_dat, group_dat)
mode_dat <- mode_dat %>% 
  filter(group %in% c("Angiosperms", "Gymnosperms"))

mode_dat$biotic %>% mean()

mode_av <- mode_dat %>% group_by(sp) %>% summarise_at(vars(biotic), list(mean = mean))

mode_av$mean %>% hist()

mean(mode_av$mean > 0.5)







