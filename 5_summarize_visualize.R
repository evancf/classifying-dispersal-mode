library("tidyverse")
library("BIEN")
library("rnaturalearth")
library("rnaturalearthdata")

load(file = "./data/phylopars_dat.RData")
country_lists <- read.csv("./data/country_lists.csv", row.names = 1) %>% tibble()


# Pull out the trait data, now with imputed values

ind <- which(!is.na(as.numeric(rownames(phylopars_dat$anc_recon)))) %>% head(1) - 1 # clunky way to get last species

phylo_gen_dat <- phylopars_dat$anc_recon[1:ind, ] %>% 
  as.data.frame() %>% 
  mutate(genus = word(rownames(.), 1, sep = fixed("_"))) %>% 
  select(genus, genbiotic, genfleshy)

head(phylo_gen_dat)


# Join this to country data

meannarm <- function(x) mean(x, na.rm=T)

country_means <- country_lists %>% 
  left_join(phylo_gen_dat) %>% 
  group_by(country) %>% 
  summarise(biotic = meannarm(genbiotic), 
            fleshy = meannarm(genfleshy))

country_means


# We need to use codes
BIEN_names <- BIEN_metadata_list_political_names()
country_means <- country_means %>% left_join(unique(BIEN_names[,c("country",
                                                                  "country_iso")]))
country_means$iso_a2 <- country_means$country_iso



# Pull in spatial data and join with the country level averages
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
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



# Get global average

global_means <- country_lists %>% 
  left_join(phylo_gen_dat)
global_means <- global_means[!duplicated(global_means$scrubbed_species_binomial),]

global_means <- global_means %>% 
  summarise(biotic = meannarm(genbiotic > 0.5), 
            fleshy = meannarm(genfleshy > 0.5))
global_means



# 

mode_dat <- read.csv("./data/mode_dat.csv", row.names = 1) %>% tibble()

mode_dat$sp %>% unique() %>% length()
mode_dat$genus %>% unique() %>% length()
mode_dat$family %>% unique() %>% length()




