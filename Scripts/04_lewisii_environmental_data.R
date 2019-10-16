######################################
### Density & Environmental Data #####
### By Mackenzie Urquhart-Cronish ####
############## 2019/10/10 ############
######################################

# to do at top of code, rename all colums to lower-case
# for eco_evo only onclude HM data

data <- read.csv("./data/02_Lewisii_surveys_2019.csv", na.strings = c("", "NA"))
# need to rename "Waypoints" to "Site" so that the data sheets talk to one another

data$Lat <- as.character(data$Lat)
data$Long <- as.character(data$Long)
data$Lat <- as.numeric(data$Lat)
data$Long <- as.numeric(data$Long)

data_1 <- data %>% 
  filter(survey_date != is.na(NA), 
         Trip != "Coleman_1") %>% 
  mutate(Long = Long * -1)

data_2 <- data_1 %>%
  filter(grepl("HM", Waypoints))

hist(data_1$Elevation)
hist(data_2$Elevation)

###############
###############
###############

lewisii_data <- read.csv("./data/01_Density_StageStructure_Data_Lewisii.csv")

data <- lewisii_data %>% 
  rename( dist_term = Distance_from_Glacial_Terminus_.m.) %>% 
  filter(!grepl("CM", Site)) %>% 
  filter(!grepl("EA", Site)) %>% 
  group_by(Site) %>% 
  mutate(foreland_code = str_replace(Site,"[0123456789]","")) %>% 
  mutate(foreland_code = str_replace(foreland_code,"[0123456789]",""))

# propotion seedling 
data_seedling <- data %>% 
  group_by(Site) %>% 
  filter(Stem_Count == 0) %>% 
  count(Site)

data_all <- data %>% 
  group_by(Site) %>% 
  count(Site) %>% 
  rename(nn = n) %>% 
  left_join(data_seedling, data_all, by = "Site")  %>% 
  mutate(prop_seedling = n/nn) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
  

data_1 <-  left_join(data_all, data, by = "Site")

# Model: proportion of seedlings ~ distance from glacier + elevation + random effect(foreland) + random effect(site)

library(lme4)
library(lmerTest)
library(visreg)

lmer_seed <- lmer(prop_seedling ~ dist_term + (1|foreland_code), data = data_1)
summary(lmer_seed)
anova(lmer_seed)

visreg(lmer_seed)


# data set to plot proportion of seedlings against distance from glacier terminus
prop_seed_data <- data_1 %>% 
  select(Site,
         prop_seedling, 
         dist_term) %>% 
  unique() %>% 
  filter(!grepl("EA", Site)) %>% 
  group_by(Site) %>% 
  mutate(foreland_code = str_replace(Site,"[0123456789]","")) %>% 
  mutate(foreland_code = str_replace(foreland_code,"[0123456789]",""))

# plotting proportion of seedlings against distance from the glacier terminus (from more recent habitat to older habitat)
ggplot(
  prop_seed_data,
  aes(x = dist_term, y = prop_seedling, colour = foreland_code)) +
  geom_point() +
  theme_classic()



# stem count vs. distance from glacier terminus
ggplot(
  data,
  aes(x = dist_term, y = Stem_Count, colour = foreland_code)) +
  geom_point() +
  theme_classic()

# stem count of adults only
data_adult <- data %>% 
  filter(Stem_Count != 0)


# Model: adult stem count ~ distance from glacier + elevation + random effect(foreland) + random effect (site)

lmer_stem <- lmer(Stem_Count ~ dist_term + (1|foreland_code), data = data_adult)
summary(lmer_stem)
anova(lmer_stem)

visreg(lmer_stem)

ggplot(
  data = data_adult,
  mapping = aes(x = dist_term, y = Stem_Count, colour = foreland_code)) +
  geom_point() +
  theme_classic()

# Model: number of individuals (does this include seedlings?) ~ distance from glacier + elevation + random effect(foreland) + random effect (site)

# number of individuals (density) vs. distance from glacier
data_density <- data %>% 
  group_by(Site) %>% 
  count(Site) 

data$X <- as.character(data$X)
                        
data_d <- data %>% 
  mutate(density = str_replace(X, " " ,"1")) 

data$indv_count <- ifelse(data$Elevation >= 1282, "1", "1")

data_density_2 <- left_join(data, data_density, by = "Site") %>% 
  select(n,
         dist_term, 
         Site, 
         foreland_code, 
         indv_count) %>% 
  mutate(n = n / 20)
  unique()

# I want to plot this as a boxplot with variance, but I am confused and I don't think I can. Average density 
ggplot(
  data_density_2,
  aes(x = dist_term, y = n, colour = foreland_code)) +
  geom_point() +
  theme_classic()

# this is wrong
ggplot(data_density_2, aes(x = dist_term, y = n, colour = foreland_code)) +
      geom_boxplot()

###############
###############
###############

env <- read.csv("./data/03_environmental_surveys.csv")
lewisii_data <- read.csv("./data/01_Density_StageStructure_Data_Lewisii.csv")

env_1 <-  env %>% 
  drop_na(per_veg) %>% 
  rename( Site = X) %>% 
  select(-latuca) %>% 
  filter(!grepl("EA", Site)) 

data <- lewisii_data %>% 
  rename( dist_term = Distance_from_Glacial_Terminus_.m.) %>% 
  filter(!grepl("CM", Site)) %>% 
  filter(!grepl("EA", Site)) %>% 
  group_by(Site) %>% 
  mutate(foreland_code = str_replace(Site,"[0123456789]","")) %>% 
  mutate(foreland_code = str_replace(foreland_code,"[0123456789]",""))

data_join <- data %>% 
  select(Site, 
         dist_term) %>% 
  unique()

# I want to add distance from the glacier to this dataset and have not been successful so far
env_data <- 
  left_join(env_1, data_join,  by = "Site")

env_data <- env_data %>% 
  mutate(foreland_code = str_replace(Site,"[0123456789]","")) %>% 
  mutate(foreland_code = str_replace(foreland_code,"[0123456789]",""))

str(env_data)

ggplot(
  data = env_data,
  mapping = aes(x = dist_term, y = per_veg, colour = foreland_code)) +
  geom_point() +
  theme_classic()

lmer_veg  <- lmer(per_veg ~ dist_term + (1|foreland_code), data = env_data)
summary(lmer_veg )
anova(lmer_veg )

visreg(lmer_veg)

ggplot(
  data = env_data,
  mapping = aes(x = dist_term, y = per_moss, colour = foreland_code)) +
  geom_point() +
  theme_classic()


lmer_rock  <- lmer(per_rock ~ dist_term + (1|foreland_code), data = env_data)
summary(lmer_rock )
anova(lmer_rock )

visreg(lmer_rock)

ggplot(
  data = env_data,
  mapping = aes(x = dist_term, y = per_rock, colour = foreland_code)) +
  geom_point() +
  theme_classic()

lmer_gravel  <- lmer(per_gravel ~ dist_term + (1|foreland_code), data = env_data)
summary(lmer_gravel )
anova(lmer_gravel )

visreg(lmer_gravel)

ggplot(
  data = env_data,
  mapping = aes(x = dist_term, y = per_gravel, colour = foreland_code)) +
  geom_point() +
  theme_classic()

lmer_soil  <- lmer(per_soil ~ dist_term + (1|foreland_code), data = env_data)
summary(lmer_soil )
anova(lmer_soil )

visreg(lmer_soil)

ggplot(
  data = env_data,
  mapping = aes(x = dist_term, y = per_soil, colour = foreland_code)) +
  geom_point() +
  theme_classic()

lmer_moss<- lmer(per_moss ~ dist_term + (1|foreland_code), data = env_data)
summary(lmer_moss )
anova(lmer_moss )

visreg(lmer_moss)

ggplot(
  data = env_data,
  mapping = aes(x = dist_term, y = willow , colour = foreland_code)) +
  geom_point() +
  theme_classic()
  