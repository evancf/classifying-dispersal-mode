# install.packages("Rphylopars")
library("Rphylopars")
library("tidyverse")


load(file = "./data/sp_tree.RData")
mode_dat <- read.csv("./data/mode_dat.csv", row.names = 1) %>% tibble()
country_lists <- read.csv("./data/country_lists.csv", row.names = 1) %>% tibble()

#plot(sp_tree[[1]], show.tip.label = F)

mode_dat$mode %>% table()

fleshy_1 <- "endo"
fleshy_0 <- c("abiotic", "attach", "autochory", "ballistic", 
              "nonzoochorous",  "passive",
              "unassisted", "water", "wind")
fleshy_NA <- c("animalnotspecified", "ant", "biotic", "cache", "invert",
               "vertebrate", "vertebratenotspecified")

mode_dat <- mode_dat %>% mutate(fleshy = ifelse(mode %in% fleshy_1, 1,
                                                ifelse(mode %in% fleshy_0, 0,
                                                       NA)))

meannarm <- function(x) mean(x, na.rm=T)

mode_dat_mean1 <- mode_dat %>% group_by(sp) %>% summarise_at(vars(biotic), list(biotic = meannarm))
mode_dat_mean2 <- mode_dat %>% group_by(sp) %>% summarise_at(vars(fleshy), list(fleshy = meannarm))
mode_dat_mean <- tibble(species = mode_dat_mean1$sp,
                        biotic = mode_dat_mean1$biotic,
                        fleshy = mode_dat_mean2$fleshy)

# Now make a database that will be used for the imputation

impute_dat <- tibble(species = sp_tree[[1]]$tip.label)

# Need underscores instead of spaces to match tips
mode_dat_mean$species <- gsub(" ", "_", mode_dat_mean$species)

# mode_dat_mean$species %in% 

impute_dat <- impute_dat %>% left_join(., mode_dat_mean)

impute_dat <- as.data.frame(impute_dat)


# Need to have fewer species sent to phylopars

# # subsample highly represented genera??
# 
# set.seed(4)
# impute_dat <- impute_dat[1:length(impute_dat$species),] # randomize order
# 
# impute_dat <- impute_dat %>% mutate(genus = word(species, 1, sep = fixed("_")))
# 
# impute_dat$has_mode <- (!is.na(impute_dat$biotic)) + (!is.na(impute_dat$fleshy))
# table(impute_dat$has_mode)
# 
# impute_dat <- impute_dat[order(impute_dat$has_mode, decreasing = T), ]
# 
# impute_dat <- impute_dat %>% group_by(genus) %>% mutate(count = sequence(n()))
# 
# impute_dat2 <- impute_dat %>% filter(count < 3) #  | has_mode > 0
# impute_dat2 <- as.data.frame(impute_dat2) %>% dplyr::select(c(species, biotic, fleshy))
# dim(impute_dat2)
# head(impute_dat2)
# 
# sp_tree2 <- ape::keep.tip(sp_tree[[1]], impute_dat2$species)

#plot(sp_tree2, show.tip.label = F)

# # This is still too slow

# Get mean by genus

impute_dat_gen <- impute_dat %>%
  mutate(genus = word(species, 1, sep = fixed("_"))) %>% 
  #filter(genus != "") %>% 
  group_by(genus) %>%
  summarise(species = dplyr::first(species),
            genbiotic = meannarm(biotic), 
            genfleshy = meannarm(fleshy)) %>% 
  as.data.frame()

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

impute_dat_gen[is.nan(impute_dat_gen)] <- NA

table(is.na(impute_dat_gen$genfleshy))
head(impute_dat_gen)
dim(impute_dat_gen)

sp_tree_gen <- ape::keep.tip(sp_tree[[1]], impute_dat_gen$species)


# Also want to add in other traits - here I'm pulling from another TRY data allocation cleaned elsewhere

plant_trait_dat <- read.table("./data/plant.trait.mean.txt", header = T)
head(plant_trait_dat)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

plant_trait_gen <- plant_trait_dat %>%
  mutate(genus = firstup(word(species, 1, sep = fixed("_")))) %>% 
  filter(genus != "") %>% 
  group_by(genus) %>%
  summarise(dispersal.unit.thickness = meannarm(dispersal.unit.thickness), 
            dispersal.unit.width = meannarm(dispersal.unit.width),
            plant.height.vegetative = meannarm(plant.height.vegetative),
            seed.dry.mass = meannarm(seed.dry.mass),
            seed.length = meannarm(seed.length),
            seed.thickness = meannarm(seed.thickness),
            seed.width = meannarm(seed.width),
            sla = meannarm(sla),
            ssd = meannarm(ssd),
            fruit.length = meannarm(fruit.length)) %>% 
  as.data.frame()

plant_trait_gen[is.nan(plant_trait_gen)] <- NA

head(plant_trait_gen)

# Join these other traits onto the impute_dat_gen dataframe

impute_dat_gen <- left_join(impute_dat_gen, plant_trait_gen)
impute_dat_gen <- impute_dat_gen %>% dplyr::select(-genus)

#

time0 <- Sys.time()
phylopars_dat <- phylopars(trait_data = impute_dat_gen, 
                           tree = sp_tree_gen,
                           pheno_correlated = F)
time1 <- Sys.time()
time1-time0

hist(phylopars_dat$anc_recon[,"genbiotic"])

mean(phylopars_dat$anc_recon[,"genbiotic"] > 0.5)
mean(phylopars_dat$anc_recon[,"genbiotic"])

mean(phylopars_dat$anc_recon[,"genfleshy"])



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

library("BIEN")

BIEN_names <- BIEN_metadata_list_political_names()

country_means <- country_means %>% left_join(unique(BIEN_names[,c("country",
                                                                  "country_iso")]))

country_means$iso_a2 <- country_means$country_iso



# Plot it
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world <- world %>% left_join(country_means)


tiff("dispersal map.tiff", units="in", width=6, height=3, res=300)

ggplot(data = world) +
  geom_sf(aes(fill = biotic*100), lwd = 0) +
  scale_fill_viridis_c(option = "plasma", name = "% of plant\nspecies\ndispersed\nby animals") +
  coord_sf(crs = "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") +
  theme_classic()#theme(panel.grid.major = element_line(colour = "transparent"))

dev.off()




global_means <- country_lists %>% 
  left_join(phylo_gen_dat)
global_means <- global_means[!duplicated(global_means$scrubbed_species_binomial),]

global_means <- global_means %>% 
  summarise(biotic = meannarm(genbiotic > 0.5), 
            fleshy = meannarm(genfleshy > 0.5))
global_means
