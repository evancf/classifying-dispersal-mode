# Note that data_assembly.R and getting_species_lists.R need to be
# run and finalized before running this

library("V.PhyloMaker")
library("tidyverse")

mode_dat <- read.csv("./data/mode_dat.csv", row.names = 1) %>% tibble()
country_lists <- read.csv("./data/country_lists.csv", row.names = 1) %>% tibble()
vpm_families <- read.csv("./data/Appendix_2-Family_list_in_GBOTB.extended.tre.csv")


# Make dataframe in format that works for V.PhyloMaker
sp_dat <- mode_dat[,c("sp", "genus")] %>% unique()
sp_country <- tibble(sp = country_lists$scrubbed_species_binomial,
                     genus = country_lists$genus) %>% unique()
sp_dat <- rbind(sp_dat,
                sp_country)

# Add in higher taxonomy

# First remove a couple genera (that aren't genera) that cause problems
sp_dat <- sp_dat %>% filter(!genus == "")
sp_dat <- sp_dat %>% filter(!genus == "X")


# Second, make a tibble for each genus, join to higher taxonomy from lookup_table()
genera_dat <- tibble(genus = unique(sp_dat$genus))
genera_dat <- genera_dat %>% left_join(taxonlookup::lookup_table(genera_dat$genus, 
                                      by_species = T))


# Third, add this to sp_dat
sp_dat <- left_join(sp_dat, genera_dat) %>% 
  filter(group %in% c("Angiosperms", "Gymnosperms")) %>% 
  dplyr::select(-group, -order)

# Don't want duplicates for the purpose of this
sp_dat <- sp_dat %>% unique()



# Figure out which families aren't represented
filter(sp_dat, !family %in% vpm_families$FAMILY)$family %>% unique()

vpm_fam_changes <- c("Rhipogonaceae" = "Ripogonaceae",
                     "Greyiaceae" = "Francoaceae",
                     "Batidaceae" = "Bataceae",
                     "Vivianiaceae" = "Geraniaceae")

sp_dat$family <- plyr::revalue(sp_dat$family,
                               vpm_fam_changes)


# Lastly, format this for V.PhyloMaker
sp_dat <- tibble(species = sp_dat$sp,
                 genus	= sp_dat$genus,
                 family = sp_dat$family,
                 species.relative = NA,
                 genus.relative = NA) %>% as.data.frame()


# Correct families that weren't correct from the perspective of vpm
sp_dat$vpm_family <- left_join(sp_dat[,c("genus"), drop = F], 
                               unique(filter(nodes.info.1, 
                                             level == "G")[,c("genus", "family")]))$family

filter(sp_dat, !family %in% nodes.info.1$family)$family %>% unique()

sp_dat$family <- ifelse(is.na(sp_dat$vpm_family),
                        sp_dat$family,
                        sp_dat$vpm_family)
sp_dat <- sp_dat %>% dplyr::select(-vpm_family)


bad_families <- filter(sp_dat, !family %in% vpm_families$FAMILY)$family %>% unique()

sp_dat <- sp_dat %>% filter(!family %in% bad_families)





# # subsample highly represented genera??
# 
# set.seed(4)
# sp_dat <- sp_dat[1:length(sp_dat$species),] # randomize order
# 
# sp_dat <- sp_dat %>% group_by(genus) %>% mutate(count = sequence(n()))
# 
# sp_dat$has_mode <- sp_dat$species %in% mode_dat$sp
# 
# sp_dat <- sp_dat %>% filter(count < 5 | has_mode)


dim(sp_dat)
time.phylo.start <- Sys.time()
sp_tree <- phylo.maker(sp_dat) # Started around 5:00 PM Wednesday
time.phylo.end <- Sys.time()
time.phylo.end - time.phylo.start # Took about 16 hours

save(sp_tree, file = "./data/sp_tree.RData")



# Write the phylogeny out for RevBayes analysis
write.tree(sp_tree[[1]],
           file = "./for_RevBayes/seed_plant_phylo.txt")

