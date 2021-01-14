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

# Now make a database that will

impute_dat <- tibble(species = sp_tree[[1]]$tip.label)

impute_dat <- impute_dat %>% left_join(., mode_dat_mean)

phylopars_dat <- phylopars(trait_data = impute_dat,
                 tree = sp_tree[[1]],
                 pheno_correlated = F)

# phylopars_dat
# 
# phylopars_dat$anc_recon # Ancestral state reconstruction and species mean prediction
# phylopars_dat$anc_var # Prediction variance
