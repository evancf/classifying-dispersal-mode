library("tidyverse")
library("BIEN")

# Get a list of all species by country

# country_lists <- BIEN_list_country(country = "Nepal")[0,]
# 
# for(i in sort(unique(BIEN_names$country))){
#   
#   dat <- BIEN_list_country(country = i)
#   
#   country_lists <- rbind(country_lists, dat)
#   
#   print(i)
#   
# }
# 
# country_lists <- country_lists %>% mutate(genus = word(country_lists$scrubbed_species_binomial, 1))
# 
# 
# # Pulling this in to fill in families
# mode_dat <- read.csv("./data/mode_dat.csv", row.names = 1) %>% tibble()
# 
# 
# # Want to get families for each genus.
# country_lists <- left_join(country_lists, unique(mode_dat[,c("genus", "family")]))
# 
# # Fill in families for those with missing genus info
# gen_vec <- unique(filter(country_lists, is.na(family))$genus)
# for(i in gen_vec){
#   
#   inds <- which(country_lists$genus == i)
#   
#   dat <- taxize::pow_search(i) # NOTE THAT WE SHOULD USE taxonlookup::lookuptable()
#   
#   if(!is.null(dim(dat$data))){
#     country_lists$family[inds] <- dat$data$family %>% table() %>% sort(decreasing = T) %>% names() %>% .[1]
#   }
#   
#   counter <- which(gen_vec == i)
#   if(counter %in% (seq(1,20000, length.out = 100) %>% round())){
#     print(paste(counter, "of ~", 17000))
#   }
#   
# }
# 
# write.csv(country_lists, file = "./data/country_lists.csv")



country_lists <- read.csv("./data/country_lists.csv", row.names = 1) %>% tibble()















# Want to get families for each genus.
# all_gen <- tibble(ex.sp = all_test$species[!duplicated(all_test$genus)],
#                   genus = all_test$genus[!duplicated(all_test$genus)],
#                   family = NA)
# 
# 
# for(i in 1:length(all_gen$genus)){
#   
#   dat <- taxize::pow_search(all_gen$genus[i])
#   
#   if(!is.null(dim(dat$data))){
#     all_gen$family[i] <- dat$data$family %>% table() %>% sort() %>% names() %>% .[1]
#   }
#   
#   
#   if(i %in% (seq(1,20000, length.out = 100) %>% round())){
#     print(paste(i, "of", length(all_gen$ex.sp)))
#   }
#   
# }
# 
# table(all_gen$family %>% is.na())
# 
# write.csv(all_gen, file = "./data/all_gen.csv")




# # Misc no longer useful? 
# 
# 
# 
# 
# 
# ex <- Taxonstand::TPL(splist = all_gen$ex.sp[c(500,18000)])
# 
# 
# all_tpl <- Taxonstand::TPL(splist=all_test$species[!duplicated(all_test$genus)]) # started 4:27
# 
# 
# ?BIEN_list_spatialpolygons
# 
# ipak(c("rnaturalearth", "sp", "sf"))
# 
# world <- ne_countries(scale = "medium", returnclass = "sp")
# # Will use a behrman equal area projection to create the grid
# world <- spTransform(world, CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# world <- st_as_sf(world)
# world_grid <- st_make_grid(world[1], n = 20)# 20 for 400 grid cells
# world_grid <- st_transform(world_grid, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# sp_world_grid <- as_Spatial(world_grid)
# 
# ggplot(data = world) + geom_sf()
# 
# test1 <- sp_world_grid[364]
# 
# plot(0,0, xlim = c(-180, 180), ylim = c(-90, 90))
# plot(test1, add = T)
# 
# test1_list <- BIEN_list_spatialpolygons(test1) # Took about an hour (50 min)
# time.test <- Sys.time() # Started at 2:46
# 
# 
# plot(sp_world_grid[1])
# 
# 
# time0 <- Sys.time()
# country.test <- BIEN_list_country(country = "United States") # Really quick
# 
# all_test <- BIEN_list_all()
# 
# all_test$genus <- word(all_test$species, 1)
# 
# all_tpl <- TPL(splist=all_test$species[!duplicated(all_test$genus)]) # started 4:27
# 
# all_phylo <- BIEN_phylogeny_complete(n_phylogenies = 1,seed = 1)
# length(all_test)
