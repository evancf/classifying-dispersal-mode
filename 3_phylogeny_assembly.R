# Note that data_assembly.R and getting_species_lists.R need to be
# run and finalized before running this

library("V.PhyloMaker")
library("tidyverse")

mode_dat <- read.csv("./data/mode_dat.csv", row.names = 1) %>% tibble()
country_lists <- read.csv("./data/country_lists.csv", row.names = 1) %>% tibble()
vpm_families <- read.csv("./data/Appendix_2-Family_list_in_GBOTB.extended.tre.csv")



# Make dataframe in format that works for V.PhyloMaker
sp_dat <- mode_dat[,c("sp", "genus", "family")] %>% unique()
sp_country <- tibble(sp = country_lists$scrubbed_species_binomial,
                     genus = country_lists$genus,
                     family = country_lists$family) %>% unique()
sp_dat <- rbind(sp_dat,
                sp_country)


# Figure out which families aren't represented

filter(sp_dat, !family %in% vpm_families$FAMILY)$family %>% unique()

vpm_fam_changes <- c("Viburnaceae" = "Adoxaceae")

sp_dat$family <- plyr::revalue(sp_dat$family,
                               vpm_fam_changes)





# Lastly, format this for V.PhyloMaker

sp_dat <- tibble(species = sp_dat$sp,
                 genus	= sp_dat$genus,
                 family = sp_dat$family,
                 species.relative = NA,
                 genus.relative = NA) %>% as.data.frame()


# Correct families that weren't correct from taxize::pow_search

sp_dat$vpm_family <- left_join(sp_dat[,c("genus"), drop = F], 
                               unique(filter(nodes.info.1, 
                                             level == "G")[,c("genus", "family")]))$family

filter(sp_dat, !family %in% vpm_family)$family %>% unique()

sp_dat$family[which(sp_dat$family == "Lonchitidaceae")] <- "Lindsaeaceae"

sp_dat$family <- ifelse(is.na(sp_dat$vpm_family),
                        sp_dat$family,
                        sp_dat$vpm_family)
sp_dat <- sp_dat %>% dplyr::select(-vpm_family)


bad_families <- filter(sp_dat, !family %in% vpm_families$FAMILY)$family %>% unique()

sp_dat <- sp_dat %>% filter(!family %in% bad_families)

sp_dat <- sp_dat %>% filter(!genus == "")
sp_dat <- sp_dat %>% filter(!genus == "X")


# Want to keep only the spermatophytes

group_dat <- tibble(genus = unique(sp_dat$genus),
                    group = NA)

intervals <- seq(1, length(group_dat$genus), length.out = 100) %>% round()
for(i in 1:(length(intervals)-1)){
  
  int <- intervals[i]:(intervals[i+1])
  
  group_dat$group[int] <- taxonlookup::lookup_table(group_dat$genus[int], 
                                                    by_species = T)$group
  
  print(paste(i, "of", length(intervals)))
  
}

sp_dat <- left_join(sp_dat, group_dat) %>% 
  filter(group %in% c("Angiosperms", "Gymnosperms")) %>% 
  dplyr::select(-group)

sp_dat <- sp_dat %>% unique()


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
sp_tree <- phylo.maker(sp_dat) # Started around 4:26
time.phylo.end <- Sys.time()
time.phylo.start - time.phylo.end