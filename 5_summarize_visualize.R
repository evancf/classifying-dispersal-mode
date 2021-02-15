library("tidyverse")
library("BIEN")
library("rnaturalearth")
library("rnaturalearthdata")
library("phytools")

load(file = "./data/phylopars_dat.RData")
load(file = "./data/sp_tree.RData")
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



# Get global averages ----------------------------------------------------------

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

global_means <- all_sp %>% 
  summarise(biotic = meannarm(genbiotic > 0.5), 
            fleshy = meannarm(genfleshy > 0.5))
global_means





# Examine trait relationships --------------------------------------------------

fleshy_col <- rgb(241,163,64, maxColorValue = 255)
not_fleshy_col <- rgb(153,142,195, maxColorValue = 255)
pt_cex <- 0.2

mod <- smatr::sma(sla ~ seed.dry.mass * ifelse(genbiotic > 0.5, "y", "n"), 
                  data = phylo_gen_dat)
mod %>% summary()


par(mfrow=c(1,1))
plot(sla ~ seed.dry.mass, 
     col = ifelse(genbiotic > 0.5, fleshy_col, not_fleshy_col), 
     cex = pt_cex,
     data = phylo_gen_dat,
     xlab = "Seed dry mass (mg)",
     ylab = "Specific leaf area (m^2/kg)",
     xlim = c(-10, 10),
     ylim = c(0.4,4.5),
     las = 1,
     xaxt = "n",
     yaxt = "n")
axis(1, at = log(10^(-10:10)),
     labels = c(10^(-10:10)))
axis(2, at = log(2^(1:6)),
     labels = c(2^(1:6)), las = 1)

legend("topright",
       c("Fleshy", "Not fleshy"),
       pch = 16,
       col = c(fleshy_col, not_fleshy_col))
text_cex <- 0.85
text(10, 0.4, "Seedlings have more resources\nto be better competitors",
     pos = 2, cex = text_cex, font = 3)
text(-8.5, 0.4, "Many seeds produced, but\nare poorer competitors",
     pos = 4, cex = text_cex, font = 3)
text(-10, 4.2, "Cheaper,\naquisitive leaves", pos = 4, 
     cex = text_cex, font = 3, srt = -90)
text(-10, 1.5, "Thicker,\nconservative leaves", pos = 4, 
     cex = text_cex, font = 3, srt = -90)

curve(coef(mod)[1,1] + coef(mod)[1,2]*x , add = T,
      col = not_fleshy_col, lwd = 3)
curve(coef(mod)[2,1] + coef(mod)[2,2]*x, add = T, col = fleshy_col, lwd = 3)




# 




