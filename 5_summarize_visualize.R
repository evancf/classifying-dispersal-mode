library("tidyverse")
library("BIEN")
library("rnaturalearth")
library("rnaturalearthdata")


# Pull out the trait data, now with imputed values

phylo_gen_dat <- phylopars_dat$anc_recon[1:(dim(impute_dat_gen)[1]), ] %>% 
  as.data.frame() %>% 
  mutate(genus = word(rownames(.), 1, sep = fixed("_"))) %>% 
  select(genus, genbiotic, genfleshy)

head(phylo_gen_dat)


# Join this to country data

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

tiff("map biotic.tiff", units="in", width=6, height=3, res=300)

ggplot(data = world) +
  geom_sf(aes(fill = biotic*100), lwd = 0) +
  scale_fill_viridis_c(option = "plasma", name = "% of plant\nspecies\ndispersed\nby animals") +
  coord_sf(crs = "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") +
  theme_classic()#theme(panel.grid.major = element_line(colour = "transparent"))

dev.off()



tiff("map fleshy.tiff", units="in", width=6, height=3, res=300)

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