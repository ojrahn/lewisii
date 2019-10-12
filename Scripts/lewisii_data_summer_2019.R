####################Lewisii Stage Structure + Density Summer Data 2019##########################################

##
#control/shift/A reformats code

install.packages("ggpubr")
library("ggpubr")
install.packages("dplyr")

#new columns ect + creating columns for time zones and forelands

lewisii_data <- read.csv("./data/Density_StageStructure_Data_Lewisii.csv")

lewisii_data$stemcount <- as.numeric(lewisii_data$Stem_Count) 
lewisii_data$elev <- as.numeric(lewisii_data$Elevation)
lewisii_data$dist <- as.numeric(lewisii_data$Distance_from_Glacial_Terminus_.m.)
lewisii_data$Zone <- as.factor(lewisii_data$Zone)
timezones <- c(1928,1928,1928,1949,"Less than LIAM","Less than LIAM","Less than LIAM",1928,"Less than LIAM","Less than LIAM",1910,1928,1910,1910,1928,1928,1928,2003,2003,2003,1977,1977,1949,1949,1977,1928,1928,1928,1928,"Less than LIAM","Less than LIAM")
timezones_numerical <- c(1928,1928,1928,1949,1600,1600,1600,1928,1600,1600,1910,1928,1910,1910,1928,1928,1928,2003,2003,2003,1977,1977,1949,1949,1977,1928,1928,1928,1928,1600,1600)


#Mutating zone column so that "core" is an arbitrary number (to read in column as numerical)

library(tidyverse)

lewisii_data <- lewisii_data %>% 
  mutate(Time_Zone = str_replace(Zone, "core", "1600")) %>% 
  mutate(Time_Zone_Categorical = str_replace(Zone,"core","Less than LIAM"))

lewisii_data$Time_Zone <- as.numeric(lewisii_data$Time_Zone) 

#taking out Coleman data since all transects were done in the same zone

lewisii_data <- lewisii_data[!grepl("CM", lewisii_data$Site),]

#take out Easton data since all transects were done in the same zone

lewisii_data <- lewisii_data[!grepl("EA",lewisii_data$Site),]

#Add column with just site code letters, no numbers

lewisii_data$Foreland_Code <- lewisii_data$Site

lewisii_data$Foreland_Code <- gsub("[0,1,2,3,4,5,6,7,8,9]","", lewisii_data$Foreland_Code)

#make new dataframe with no duplicate timezones

timezone_reps <- lewisii_data[!duplicated(lewisii_data$Site), ]
#make column just for distance from 2016 line
distance_from_2016 <- timezone_reps$dist

#make new dataframe without seedlings 

df_no_seedlings <- filter(lewisii_data, Stem_Count > 0)
  

#########################correlation between time zone + elevation############################
#Time_Zone is numerical w/1600
#elev is numerical

###making separate dataframes with only HM, GB or EA site codes
lewisii_data$Time_Zones <- as.numeric(lewisii_data$Time_Zone)


HMdata <- lewisii_data[grep("HM", lewisii_data$Site), ]
GBdata <- lewisii_data[grep("GB", lewisii_data$Site), ]
EAdata <- lewisii_data[grep("EA", lewisii_data$Site), ]

#Correlation between time zone and elevation for each site

HM_elev_timezone_corr <- cor.test(HMdata$elev, HMdata$Time_Zones, method=c("pearson"))
#0.35 psig
GB_elev_timezone_corr <- cor.test(GBdata$elev, GBdata$Time_Zones, method=c("pearson"))
#0.05 p 0.11

####################correlation between distance from terminus and elevation##################

HM_elev_distance_corr <- cor.test(HMdata$elev, HMdata$dist, method=c("pearson"))
#-0.57 psig
GB_elev_distance_corr <- cor.test(GBdata$elev, GBdata$dist, method=c("pearson"))
#-0.46 psig


#Separating for Density- Making Separate Dataframe with just number of plants at each site
#37 sites

HM04_df <- subset(lewisii_data, Site == "HM04")
HM02_df <- subset(lewisii_data, Site == "HM02")
HM03_df <- subset(lewisii_data, Site == "HM03")
HM05_df <- subset(lewisii_data, Site == "HM05")
HM09_df <- subset(lewisii_data, Site == "HM09")
HM20_df <- subset(lewisii_data, Site == "HM20")
HM21_df<- subset(lewisii_data, Site == "HM21")
GB18_df<- subset(lewisii_data, Site == "GB18")
GB25_df<- subset(lewisii_data, Site == "GB25")
GB26_df<- subset(lewisii_data, Site == "GB26")
GB10_df<- subset(lewisii_data, Site == "GB10")
GB21_df<- subset(lewisii_data, Site == "GB21")
GB22_df<- subset(lewisii_data, Site == "GB22")
GB17_df<- subset(lewisii_data, Site == "GB17")
HM08_df<- subset(lewisii_data, Site == "HM08")
HM16_df<- subset(lewisii_data, Site == "HM16")
HM15_df<- subset(lewisii_data, Site == "HM15")
HM34_df<- subset(lewisii_data, Site == "HM34")
HM33_df<- subset(lewisii_data, Site == "HM33")
HM31_df<- subset(lewisii_data, Site == "HM31")
HM29_df<- subset(lewisii_data, Site == "HM29")
HM27_df<- subset(lewisii_data, Site == "HM27")
HM36_df<- subset(lewisii_data, Site == "HM36")
HM35_df<- subset(lewisii_data, Site == "HM35")
HM37_df<- subset(lewisii_data, Site == "HM37")
HM13_df<- subset(lewisii_data, Site == "HM13")
HM23_df<- subset(lewisii_data, Site == "HM23")
HM22_df<- subset(lewisii_data, Site == "HM22")
HM10_df<- subset(lewisii_data, Site == "HM10")
HM17_df<- subset(lewisii_data, Site == "HM17")
HM18_df<- subset(lewisii_data, Site == "HM18")

#number of plants per site

HM04_num_plants <- sum(lewisii_data$Site == "HM04")
HM02_num_plants <- sum(lewisii_data$Site == "HM02")
HM03_num_plants <- sum(lewisii_data$Site == "HM03")
HM05_num_plants <- sum(lewisii_data$Site == "HM05")
HM09_num_plants <- sum(lewisii_data$Site == "HM09")
HM20_num_plants <- sum(lewisii_data$Site == "HM20")
HM21_num_plants<- sum(lewisii_data$Site == "HM21")
GB18_num_plants<- sum(lewisii_data$Site == "GB18")
GB25_num_plants<- sum(lewisii_data$Site == "GB25")
GB26_num_plants<- sum(lewisii_data$Site == "GB26")
GB10_num_plants<- sum(lewisii_data$Site == "GB10")
GB21_num_plants<- sum(lewisii_data$Site == "GB21")
GB22_num_plants<- sum(lewisii_data$Site == "GB22")
GB17_num_plants<- sum(lewisii_data$Site == "GB17")
HM08_num_plants<- sum(lewisii_data$Site == "HM08")
HM16_num_plants<- sum(lewisii_data$Site == "HM16")
HM15_num_plants<- sum(lewisii_data$Site == "HM15")
HM34_num_plants<- sum(lewisii_data$Site == "HM34")
HM33_num_plants<- sum(lewisii_data$Site == "HM33")
HM31_num_plants<- sum(lewisii_data$Site == "HM31")
HM29_num_plants<- sum(lewisii_data$Site == "HM29")
HM27_num_plants<- sum(lewisii_data$Site == "HM27")
HM36_num_plants<- sum(lewisii_data$Site == "HM36")
HM35_num_plants<- sum(lewisii_data$Site == "HM35")
HM37_num_plants<- sum(lewisii_data$Site == "HM37")
HM13_num_plants<- sum(lewisii_data$Site == "HM13")
HM23_num_plants<- sum(lewisii_data$Site == "HM23")
HM22_num_plants<- sum(lewisii_data$Site == "HM22")
HM10_num_plants<- sum(lewisii_data$Site == "HM10")
HM17_num_plants<- sum(lewisii_data$Site == "HM17")
HM18_num_plants<- sum(lewisii_data$Site == "HM18")

#making new dataframe

Sites_2 <- c("HM04", "HM02" ,"HM03" ,"HM05" ,"HM09" ,"HM20" ,"HM21","GB18","GB25","GB26","GB10","GB21", "GB22","GB17","HM08","HM16","HM15","HM34","HM33","HM31","HM29","HM27","HM36","HM35","HM37","HM13","HM23","HM22","HM10","HM17","HM18")


Number_of_Plants <- c(HM04_num_plants,HM02_num_plants,HM03_num_plants,HM05_num_plants,HM09_num_plants,HM20_num_plants, HM21_num_plants, GB18_num_plants, GB25_num_plants, GB26_num_plants,GB10_num_plants, GB21_num_plants, GB22_num_plants, GB17_num_plants, HM08_num_plants, HM16_num_plants, HM15_num_plants, HM34_num_plants, HM33_num_plants, HM31_num_plants, HM29_num_plants, HM27_num_plants,HM36_num_plants, HM35_num_plants, HM37_num_plants, HM13_num_plants, HM23_num_plants, HM22_num_plants, HM10_num_plants,HM17_num_plants, HM18_num_plants)

Foreland_Code <- lewisii_data$Foreland_Code

density_old <- data.frame(Sites_2, Number_of_Plants, timezones)

#create new column with counts of seedlings at each site

seedling_dataframe <- lewisii_data %>% select(Site, Stem_Count) %>%
  group_by(Site) %>%
  mutate(seedling_count = sum(Stem_Count == 0)) %>%
  select(-Stem_Count) %>%
  unique()
number_of_seedlings <- seedling_dataframe$seedling_count
 
 

##################################Scatterplots + New Dataframes##################################################


##################################stem count and timezone#####################################
avg_stemcount <- lewisii_data %>% select(Site, Stem_Count,Foreland_Code, elev) %>%
  group_by(Site) %>%
  mutate(SC_new = mean(Stem_Count)) %>%
  select(-Stem_Count) %>%
  unique()
avg_stemcount$timezone <- timezones
avg_stemcount$numerical_timezones <- timezones_numerical
avg_stemcount$number_of_seedlings <- number_of_seedlings

###Scatterplot for timezone and stem count
stemcount_timezone_scatter <-
  ggplot(avg_stemcount, aes(x = numerical_timezones, y = SC_new, fill=Foreland_Code)) +
  geom_point() + geom_smooth(method = "lm") + labs(x = "Time Zone", y =
  "Average Stem Count", title = "Stem Count vs. Time Zone")

###Box plot for timezone and stem count
stemcount_timezone_boxplot <-  ggplot(avg_stemcount, aes(x=timezone, y=SC_new,fill=Foreland_Code)) + 
  geom_boxplot()+labs(x="Time Zone",y= "Average Stem Count", title="Stem Count vs. Time Zone") +scale_x_discrete(limits=c("Less than LIAM", "1910","1912","1928","1949","1977","2003"))
  
#linear model for timezone and stem count
model_stemcount_timezone <- lmer(SC_new ~ timezone + (1|Foreland_Code), data=avg_stemcount)
summary(model_stemcount_timezone)
anova(model_stemcount_timezone)

testmodel_stemcount_timezone <- visreg(model_stemcount_timezone, "Foreland_Code", type="contrast")

################################density and timezone##########################################
density_df <- density_old 
density_df$Foreland_Code <- avg_stemcount$Foreland_Code
density_df$numerical_timezones <- timezones_numerical
density_df$distance_from_2016 <- distance_from_2016
density_df$Foreland_Code <- Foreland_Code
density_df$elev <- Elevation

library(ggplot2)

###Scatter plot for timezone and density 

density_timezone_scatter <- ggplot(density_df, aes(x = numerical_timezones, y = Number_of_Plants, fill=Foreland_Code)) +
  geom_point()+geom_smooth(method = "lm")+ labs(x="Time Zone", y="Density", title="Density vs. Time Zone")

###Boxplot for timezone and density 
density_timezone_boxplot <-  ggplot(density_df, aes(x=timezones, y=Number_of_Plants,fill=Foreland_Code)) + 
  geom_boxplot()+labs(x="Time Zone",y= "Number of Plants", title="Density vs. Time Zone") +scale_x_discrete(limits=c("Less than LIAM", "1910","1912","1928","1949","1977","2003"))

###Linear model for timezone and density
model_density_timezone <- lmer(Number_of_Plants ~ timezones + (1|Foreland_Code), data = density_df )
summary(model_density_timezone)
anova(model_density_timezone)

install.packages("lme4")
library(lme4)
install.packages("lmerTest")
library(lmerTest)

#can add degree of freedom ddf="Kenward-Roger"

#########visreg for density and timezone###
install.packages("visreg")
library(visreg)

testmodel_density_timezone <- visreg(model_density_timezone)

##eg. code
testmodel <- visreg(model, xvar="",by= "",cond="",overlay="")
visreg(fit, "Wind", type="contrast")



################prop. flowering and timezone###################################################

#making column with yes/no as 0/1 for flowering
lewisii_data$flowering_numerical <- ifelse(lewisii_data$Flowering=='Y', 1,0)

#making dataframe for prop flowering
prop_flowering <- lewisii_data %>% select(Site,flowering_numerical,Foreland_Code, elev) %>% 
  group_by(Site) %>% 
  mutate(prop_f_site = mean(flowering_numerical)) %>% 
  select(-flowering_numerical) %>% 
  unique() 
prop_flowering$timezone <- timezones
prop_flowering$numerical_timezones <- timezones_numerical
prop_flowering$number_of_seedlings <- number_of_seedlings


#####Scatter plot with time zone and prop flowering

flowering_timezone_scatter <- ggplot(prop_flowering, aes(x = numerical_timezones, y = prop_f_site, fill= Foreland_Code)) + geom_point()+geom_smooth(method="lm")+ labs(x="Time Zone", y="Proportion Flowering",
  title="Proportion Flowering vs. Time Zone")

####box plot with time zone and prop flowering

flowering_timezone_boxplot <-  ggplot(prop_flowering, aes(x=timezone, y=prop_f_site,fill=Foreland_Code)) + 
  geom_boxplot()+labs(x="Time Zone",y= "Proportion Flowering", title="Proportion Flowering vs. Time Zone") + scale_x_discrete(limits=c("Less than LIAM", "1910","1912","1928","1949","1977","2003"))

###linear model for timezone and proportion flowering
model_flowering_timezone <- lmer(prop_f_site ~ timezone + (1|Foreland_Code), data=prop_flowering)
summary(model_flowering_timezone)
anova(model_flowering_timezone)

testmodel_flowering_timezone <- visreg(model_flowering_timezone)


##################################################Density and Distance from 2016######################################

density_df$distance_from_2016 <- distance_from_2016
density_df$Foreland_Code <- Foreland_Code

###scatterplot for density and distance
density_distance_scatter <- ggplot(density_df, aes(x = distance_from_2016, y = Number_of_Plants, fill=Foreland_Code)) +
geom_point()+geom_smooth(method = "lm")+ labs(x="Distance from 2016 Line (m)", y="Density", title="Density vs. Distance from 2016")

###model for density and distance

model_density_distance <- lmer(Number_of_Plants ~ distance_from_2016+ elev + (1|Foreland_Code), data=density_df )
summary(model_density_distance)
anova(model_density_distance)

testmodel_density_distance <- visreg(model_density_distance)


############################################Stem Count and Distance from 2016######################

#make different dataframe with no 0 count (seedlings) in Stem Count column

avg_stemcount_ns <- df_no_seedlings %>% select(Site, Stem_Count,Foreland_Code, elev) %>%
  group_by(Site) %>%
  mutate(SC_ns = mean(Stem_Count)) %>%
  select(-Stem_Count) %>%
  unique()
avg_stemcount_ns$timezone <- timezones
avg_stemcount_ns$numerical_timezones <- timezones_numerical
avg_stemcount_ns$number_of_seedlings <- number_of_seedlings

Foreland_Code <- avg_stemcount$Foreland_Code

avg_stemcount$distance_from_2016 <- distance_from_2016

###scatterplot for stem count and distance
stemcount_distance_scatter <-
  ggplot(avg_stemcount, aes(x = distance_from_2016, y = SC_new, fill=Foreland_Code)) +
  geom_point() + geom_smooth(method = "lm") + labs(x = "Distance from 2016 Line (m)", y =
  "Average Stem Count", title = "Stem Count vs. Distance from 2016")

###model for stem count and distance

model_stemcount_distance <- lmer(SC_ns ~ distance_from_2016 + (1|Foreland_Code) + elev,data=avg_stemcount_ns)
#want another model with seedling # as response variable

summary(model_stemcount_distance)
anova(model_stemcount_distance)

testmodel_stemcount_distance <- visreg(model_stemcount_distance)

#model for stem count and distance with no seedlings
avg_stemcount_ns$distance_from_2016 <- distance_from_2016

model_stemcount_distance_ns <- lmer(SC_ns ~ distance_from_2016 + elev+ (1|Foreland_Code) ,data=avg_stemcount_ns)
summary(model_stemcount_distance_ns)
anova(model_stemcount_distance_ns)
model_stemcount_distance_ns <- visreg(model_stemcount_distance_ns)


#########################################Proportion Flowering and Distance from 2016##############

#making dataframe for proportion flowering with seedlings removed
prop_flowering_ns <- df_no_seedlings %>% select(Site,flowering_numerical,Foreland_Code, elev) %>%
  group_by(Site) %>%
  mutate(prop_f_site = mean(flowering_numerical)) %>%
  select(-flowering_numerical) %>%
  unique()
prop_flowering_ns$timezone <- timezones
prop_flowering_ns$numerical_timezones <- timezones_numerical

prop_flowering$distance_from_2016 <- distance_from_2016

###scatterplot for flowering and distance

flowering_timezone_scatter <- ggplot(prop_flowering, aes(x = distance_from_2016, y = prop_f_site, fill= Foreland_Code)) + geom_point()+geom_smooth(method="lm")+ labs(x="Distance from 2016 Line (m)", y="Proportion Flowering", title="Proportion Flowering vs. Distance from 2016")

###model for flowering and distance

model_flowering_distance <- lmer(prop_f_site ~ distance_from_2016 * elev + (1|Foreland_Code), data=prop_flowering)
summary(model_flowering_distance)
anova(model_flowering_distance)

testmodel_flowering_distance <- visreg(model_flowering_distance)

###model for flowering and distance without seedlings

model_flowering_distance_ns <- lmer(prop_f_site ~ distance_from_2016 + elev + (1|Foreland_Code), data=prop_flowering_ns)
summary(model_flowering_distance)
anova(model_flowering_distance)
testmodel_flowering_distance_ns <- visreg(model_flowering_distance)

##########notes ect#####################
                            

timezone_stemcount <- glmer(Stem_Count ~ Zone + (1|Site), data=lewisii_data, family=poisson(
  link = log))

timezone_density <- glmer(Stem_Count ~ Zone + (1|Site), data=lewisii_data, family=poisson(
  link = log))


#think about using spatial analysis packages
#LR test or AIC values (which is lower) to see if slope and intercept is a better fit or just intercept

#change interactions in model (*)
#scale variables (probably just predictors)
#+ centre = true
#look at model diagnostics

#put anova tables into a csv


