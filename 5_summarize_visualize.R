library("tidyverse")
library("BIEN")
library("rnaturalearth")
library("rnaturalearthdata")
library("phytools")

load(file = "./data/phylopars_dat.RData")
country_lists <- read.csv("./data/country_lists.csv", row.names = 1) %>% tibble()


# Pull out the trait data, now with imputed values

ind <- which(!is.na(as.numeric(rownames(phylopars_dat$anc_recon)))) %>% head(1) - 1 # clunky way to get last species

phylo_gen_dat <- phylopars_dat$anc_recon[1:ind, ] %>% 
  as.data.frame() %>% 
  mutate(genus = word(rownames(.), 1, sep = fixed("_")))# %>% 
  #select(genus, genbiotic, genfleshy)

head(phylo_gen_dat)


# Join this to country data

meannarm <- function(x) mean(x, na.rm=T)

country_means <- country_lists %>% 
  left_join(phylo_gen_dat) %>% 
  group_by(country) %>% 
  summarise(biotic = meannarm(genbiotic), 
            fleshy = meannarm(genfleshy))

country_means



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

# bien_to_map_changes <- split(rep(names(map_to_bien_changes), 
#                                  lengths(map_to_bien_changes)), 
#                              unlist(map_to_bien_changes)) %>% unlist()

# Pull in spatial data and join with the country level averages
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  mutate(country = plyr::revalue(admin, map_to_bien_changes)) %>% 
  left_join(country_means)

pdf("map biotic.pdf", width=6, height=2.2)

ggplot(data = world) +
  geom_sf(aes(fill = biotic*100), lwd = 0) +
  scale_fill_viridis_c(option = "plasma", name = "% of plant\nspecies\ndispersed\nby animals") +
  coord_sf(crs = "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") +
  theme_classic()#theme(panel.grid.major = element_line(colour = "transparent"))

dev.off()



pdf("map fleshy.pdf", width=6, height=2.2)

ggplot(data = world) +
  geom_sf(aes(fill = fleshy*100), lwd = 0) +
  scale_fill_viridis_c(option = "plasma", name = "% of plant\nspecies\nfleshy\nfruited") +
  coord_sf(crs = "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") +
  theme_classic()#theme(panel.grid.major = element_line(colour = "transparent"))

dev.off()



# Get global averages

all_sp <- country_lists %>% 
  left_join(phylo_gen_dat) %>% 
  filter(!is.na(genbiotic)) %>% 
  mutate(species = gsub(" ", "_", scrubbed_species_binomial, fixed = T))
all_sp <- all_sp[!duplicated(all_sp$scrubbed_species_binomial),]

phylo_sp <- filter(all_sp, all_sp$species %in% sp_tree[[1]]$tip.label) %>% as.data.frame()

all_sp_tree <- sp_tree[[1]]
all_sp_tree <- ape::keep.tip(all_sp_tree, phylo_sp$species)

rownames(phylo_sp) <- phylo_sp$species
phylo_sp <- phylo_sp[all_sp_tree$tip.label,]

plot(all_sp_tree, show.tip.label = F, tip.color = ifelse(phylo_sp$genbiotic > 0.5,
                                                         "red", "brown"))

dotTree(all_sp_tree, ifelse(phylo_sp$genbiotic > 0.5,
                            1, 0), length=10, ftype="i")




global_means <- all_sp %>% 
  summarise(biotic = meannarm(genbiotic > 0.5), 
            fleshy = meannarm(genfleshy > 0.5))
global_means


