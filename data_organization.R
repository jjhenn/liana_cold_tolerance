## Dataset creation for cold tolerance data
library(dplyr)

xy <- read.csv("liana_xy.csv")
supercooling <- read.csv("liana_supercooling.csv")
PEL_lt50 <- read.csv("liana_pel_lt50.csv")
leaf_area <- read.csv("liana_leaf_area.csv")
leaf_mass <- read.csv("liana_leaf_mass.csv")

supercooling <- supercooling %>% 
  mutate(sample_id = paste(population, rep, leaf, sep = "")) %>% 
  select(-13)

cold_tol <- leaf_area %>% 
  full_join(PEL_lt50, by = "sample_id") %>% 
  full_join(supercooling, by = "sample_id") %>% 
  full_join(leaf_mass, by = "sample_id") %>% 
  full_join(xy, by = "population")
