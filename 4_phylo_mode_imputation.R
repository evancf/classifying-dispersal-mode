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

# Output this for RevBayes

fleshy_dat <- mode_dat %>% 
  filter(sp %in% sp_tree[[2]]$species &
           datasource != "TRY" &
           !is.na(fleshy) &
           sapply(strsplit(mode_dat$sp, " "), length) == 2) %>% 
  mutate(species = sp)%>% 
  select(species, fleshy) %>%
  group_by(species) %>%
  summarise(fleshy = mean(fleshy)) %>% 
  mutate(tip = gsub(" ", "_", species, fixed = T)) 

fleshy_dat$species %>% unique() %>% length()

fleshy_dat <- fleshy_dat %>%
  left_join(taxonlookup::lookup_table(fleshy_dat$species,
                                      by_species = T) %>%
              mutate(species = rownames(.))) %>%
  group_by(genus) %>%
  mutate(fleshy_genus = mean(fleshy),
         count_genus = n()) %>%
  group_by(family) %>%
  mutate(fleshy_family = mean(fleshy),
         count_family = n()) %>%
  mutate(prob = (fleshy != fleshy_genus) &
           (abs(fleshy - fleshy_genus) > 0.5)) %>%
  mutate(sole_prob = prob &
           (fleshy_genus == 1/count_genus |
              fleshy_genus == (count_genus-1) / count_genus))

# Fix some where it's clear based on genus and family that it should be the opposite
fleshy_dat$fleshy <- with(fleshy_dat,
                          ifelse(sole_prob & fleshy < 0.5 & fleshy_genus > 0.5,
                            1, fleshy))
fleshy_dat$fleshy <- with(fleshy_dat,
                          ifelse(sole_prob & fleshy > 0.5 & fleshy_genus < 0.5,
                                 0, fleshy))

# Some Poales need changing
fleshy_dat$fleshy[fleshy_dat$prob & fleshy_dat$family %in% c("Poaceae", "Cyperaceae", "Juncaceae")] <- 0


# Check where the problems continue

fleshy_dat <- fleshy_dat %>%
  group_by(genus) %>%
  mutate(fleshy_genus = mean(fleshy),
         count_genus = n()) %>%
  group_by(family) %>%
  mutate(fleshy_family = mean(fleshy),
         count_family = n()) %>%
  mutate(prob = (fleshy != fleshy_genus) &
           (abs(fleshy - fleshy_genus) > 0.5)) %>%
  mutate(sole_prob = prob &
           (fleshy_genus == 1/count_genus |
              fleshy_genus == (count_genus-1) / count_genus))


# Cases where fleshy is not 0 or 1, will go with genus level info
fleshy_dat$fleshy <- ifelse(fleshy_dat$fleshy > 0 & fleshy_dat$fleshy < 1,
                            ifelse(fleshy_dat$fleshy_genus >= 0.5, 1, 0),
                            fleshy_dat$fleshy)

fleshy_dat <- fleshy_dat %>% 
  select(species, tip, fleshy, genus, family, order, group)

# Write the phylogeny out for RevBayes analysis
write.csv(fleshy_dat,
          file = "./for_RevBayes/fleshy_dat.csv")


# Now make sure this info is added to the mode data

mode_dat <- mode_dat %>% 
  select(-fleshy) %>% 
  left_join(select(fleshy_dat, species, fleshy, group), by = c("sp" = "species"))

# Also make sure we only have the relevant groups

mode_dat <- mode_dat %>% 
  left_join(taxonlookup::lookup_table(unique(mode_dat$sp),
                                      by_species = T) %>%
              mutate(sp = rownames(.))) %>% 
  filter(group %in% c("Angiosperms", "Gymnosperms"))

mode_dat %>% glimpse()

mode_dat$group %>% table()



#

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

# # This code makes it a binary
# impute_dat_gen$genbiotic <- ifelse(impute_dat_gen$genbiotic > 0.5, 1, 0)
# impute_dat_gen$genfleshy <- ifelse(impute_dat_gen$genfleshy > 0.5, 1, 0)

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
set.seed(4)
phylopars_dat <- phylopars(trait_data = impute_dat_gen, 
                           tree = sp_tree_gen,
                           pheno_correlated = F)
time1 <- Sys.time()
time1-time0

save(phylopars_dat, file = "./data/phylopars_dat.RData")




# A quick visualization

sp_tree_gen$tip.label <- sp_tree_gen$tip.label %>% word(1, sep = fixed("_"))

phylo_sp <- phylo_gen_dat
phylo_sp$genbiotic <- ifelse(phylo_sp$genbiotic > 0.5, 1, NA)
phylo_sp$genfleshy <- ifelse(phylo_sp$genfleshy > 0.5, 1, NA)
rownames(phylo_sp) <- phylo_sp$genus
phylo_sp <- phylo_sp[sp_tree_gen$tip.label,]

labs <- c(phylo_sp$genfleshy,
          rep(NA, length(sp_tree_gen$node.label)))

png(filename = "phylo_fleshy_genera_red.png", width = 1000, height = 4000)
ggtree(sp_tree_gen) + 
  geom_tippoint(aes(color = factor(labs)), size=1, alpha=1) +
  scale_color_brewer("fleshy", palette="RdBu")
dev.off()






