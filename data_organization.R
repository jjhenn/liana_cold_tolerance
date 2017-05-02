#hello!!

## Dataset creation for cold tolerance data
library(dplyr)
library(tidyr)
library(lmerTest)

xy <- read.csv("liana_xy.csv")
supercooling <- read.csv("liana_supercooling.csv")
PEL_lt50 <- read.csv("liana_pel_lt50.csv")
leaf_area <- read.csv("liana_leaf_area.csv")
leaf_mass <- read.csv("liana_leaf_mass.csv")
bioclim <- read.csv("liana_bioclim.csv")

supercooling <- supercooling %>% 
  mutate(sample_id = paste(population, rep, leaf, sep = "")) %>% 
  select(-13)

samples <- leaf_mass %>% 
  full_join(leaf_area) %>% 
  full_join(PEL_lt50) %>% 
  #full_join(supercooling, by = "sample_id") %>% 
  distinct(sample_id)

samples = read.csv("samples.csv") %>% 
  select(-X)



PEL <- samples %>% 
  full_join(PEL_lt50) %>% 
  full_join(leaf_mass) %>% 
  #full_join(supercooling) %>% 
  full_join(leaf_area) %>% 
  full_join(xy, by = "population") %>% 
  full_join(bioclim) %>% 
  select(-spp.x) %>% 
  filter(PEL_LT50 < 0) %>% 
  mutate(SLA = (area_cm2/(dry_mass/1000))) %>% 
  mutate(LDMC = (dry_mass/wet_mass)) %>% 
  filter(spp != "PATR") %>% 
  mutate(bio6 = (bio6-(mean(bio6)))/sd(bio6))

sc <- samples %>% 
  full_join(supercooling) %>%
  full_join(xy, by = "population") %>% 
  full_join(bioclim) %>% 
  mutate(ldmc = sc_dry_mass/sc_wet_mass) %>% 
  filter(spp != "PATR") %>% 
  filter(sc_wet_mass > 0) %>% 
  filter(spp != "VIRI") %>% 
  mutate(bio6 = (bio6-(mean(bio6)))/sd(bio6))

plot(PEL_LT50 ~ bio6, data = PEL, col = spp.y)


m.pel <- lmer(PEL_LT50 ~ latitude + spp + (1|population)
             , data = PEL)
m.pel1 <- lmer(PEL_LT50 ~ latitude*spp + (1|population)
               , data = PEL)
s.test <- step(m.pel)


m.sc <- lmer(sc_temp ~ bio6 + (1|population), data = sc)
sc.test <- step(m.sc)

sc.means <- sc %>% 
  group_by(population) %>% 
  summarize(mean = mean(sc_temp, na.rm = T)) %>% 
  full_join(xy)

means <- PEL %>% 
  group_by(population) %>% 
  summarize (mean = mean(PEL_LT50), ldmc = mean(LDMC), dry = mean(dry_mass)) %>% 
  full_join(xy)

plot(ldmc ~ latitude, col = spp, data = means)
test <- lm(mean ~ latitude*spp + spp*ldmc, data = means)
