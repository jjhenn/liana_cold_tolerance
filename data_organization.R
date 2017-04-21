## Dataset creation for cold tolerance data
library(dplyr)
library(tidyr)

xy <- read.csv("liana_xy.csv")
supercooling <- read.csv("liana_supercooling.csv")
PEL_lt50 <- read.csv("liana_pel_lt50.csv")
leaf_area <- read.csv("liana_leaf_area.csv")
leaf_mass <- read.csv("liana_leaf_mass.csv")

supercooling <- supercooling %>% 
  mutate(sample_id = paste(population, rep, leaf, sep = "")) %>% 
  select(-13)

samples <- leaf_mass %>% 
  full_join(leaf_area) %>% 
  full_join(PEL_lt50) %>% 
  #full_join(supercooling, by = "sample_id") %>% 
  distinct(sample_id)

samples = read.csv("samples.csv")

cold_tol <- samples %>% 
  full_join(PEL_lt50) %>% 
  full_join(leaf_mass) %>% 
  full_join(supercooling) %>% 
  full_join(leaf_area) %>% 
  full_join(xy, by = "population") %>% 
  select(-spp.x) %>% 
  filter(PEL_LT50 < 0) %>% 
  mutate(SLA = (area_cm2/(dry_mass/1000))) %>% 
  mutate(sc_LDMC = (sc_wet_mass/sc_dry_mass)) %>% 
  mutate(LDMC = (wet_mass/dry_mass)) %>% 
  filter(spp.y != "PATR")




plot(PEL_LT50 ~ wet_mass, data = cold_tol, col = spp.y)


test <- lmer(PEL_LT50 ~ latitude + spp.y + area_cm2 + latitiude:spp.y + 
             latitiude:area_cm2 + spp.y:area_cm2 + (1|population), data = cold_tol)


