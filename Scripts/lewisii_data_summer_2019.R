##################################################Density and Distance from 2016######################################

density_df2 <- merge(density_df,lewisii_data, by.x = 2, by.y = 0, all.x = TRUE)

###scatterplot for density and distance
density_distance_scatter <- ggplot(density_df, aes(x = distance_from_2016, y = Number_of_Plants, fill=Foreland_Code)) +
geom_point()+geom_smooth(method = "lm")+ labs(x="Distance from 2016 Line (m)", y="Density", title="Density vs. Distance from 2016")

###model for density and distance
#install.packages("standardize")
library(standardize)

#standardizing predictor variables
density_df2$distance_scaled <- scale(density_df2$distance_from_2016, center=TRUE)[,1]
#density_df$elev_scaled <- scale(density_df$elev, center=TRUE)[,1]

#making model
#log of plants to make residual plot better
model_density_distance <- lmer(log(Number_of_Plants) ~ distance_scaled + (1|Foreland_Code.x), data=density_df2)

summary(model_density_distance)
anova(model_density_distance)
#residuals
resid_plot_distance <- plot(model_density_distance)

vis_density_distance <- visreg(
  model_density_distance,
  main = "Predicted Population Density vs. Distance from 2016 Glacial
  Terminus",
  ylab = "log(Predicted Population Density)",
  xlab = "Scaled Distance from 2016 Terminus",
  gg = TRUE,
  line.par = list(col = 'orange'),
  points.par = list(col = 'red', cex = 1, pch =
                      1))
#add later?
geom_smooth(col='#FF4E37', fill='#FF4E37')

############################################Stem Count and Distance from 2016######################

#make different dataframe with no 0 count (seedlings) in Stem Count column

avg_stemcount_ns <- df_no_seedlings %>% select(Site, Stem_Count,Foreland_Code,elev,Distance_from_Glacial_Terminus_.m.) %>%
  group_by(Site) %>%
  mutate(SC_ns = mean(Stem_Count)) %>%
  select(-Stem_Count) %>%
  unique()
avg_stemcount_ns$timezone <- timezones
avg_stemcount_ns$numerical_timezones <- timezones_numerical
avg_stemcount_ns$number_of_seedlings <- number_of_seedlings


avg_stemcount_ns$distance_from_2016 <- avg_stemcount_ns$Distance_from_Glacial_Terminus_.m.
avg_stemcount_ns$Foreland_Code <- avg_stemcount_ns$Foreland_Code

###scatterplot for stem count and distance
stemcount_distance_scatter <-
  ggplot(avg_stemcount, aes(x = distance_from_2016, y = SC_new, fill=Foreland_Code)) +
  geom_point() + geom_smooth(method = "lm") + labs(x = "Distance from 2016 Line (m)", y =
  "Average Stem Count", title = "Stem Count vs. Distance from 2016")

#scaling predictor variables - don't need
avg_stemcount_ns$distance_scaled <- scale(avg_stemcount_ns$distance_from_2016, center=TRUE)[,1]
#avg_stemcount_ns$elev_scaled <- scale(avg_stemcount$elev, center=TRUE)[,1]
avg_stemcount_ns$stemcount_scaled <- scale(avg_stemcount_ns$SC_ns, center=TRUE)[,1]

#######model for stem count and distance (from averages)############

#scale distance
avg_stemcount_ns$distance_scaled <- scale(avg_stemcount_ns$distance_from_2016, center=TRUE)[,1]

model_stemcount_distance <- lmer(SC_ns ~ distance_scaled + (1|Foreland_Code),data=avg_stemcount_ns)

summary(model_stemcount_distance)
anova(model_stemcount_distance)
#residuals
modplot_stemcount <- plot(model_stemcount_distance)
#definitely cone shaped
#visreg model
vis_stemcount_distance <- visreg(model_stemcount_distance, 
                                 main="Predicted Average Stem Count vs. Distance from 2016  Glacial
                                 Terminus",
                                 ylab="Predicted Average Stem Count",
                                 xlab="Distance from 2016 Terminus",
                                 gg=TRUE,
                                 line.par=list(col='orange'),
                                 points.par=list(col='red',cex=1, pch=1))


#####################model for stemcount and distance (from main dataframe)##################
df_no_seedlings$distance_scaled <- scale(df_no_seedlings$dist, center=TRUE)[,1]
library(optimx)
model_stemcount_main <- lmer(Stem_Count ~ distance_scaled + (1|Foreland_Code/Site),data=df_no_seedlings)
anova(model_stemcount_main)

#visreg
vis_stemcount_main <- visreg(
  model_stemcount_main, 
  main = "Predicted Average Stem Count vs. Distance from 2016  Glacial
  Terminus",
  ylab = "Predicted Average Stem Count",
  xlab = "Distance from 2016 Terminus",
  gg = TRUE,
  line.par = list(col =
                    'orange'),
  points.par = list(col =
                      'red', cex = 1, pch = 1)
)
resid_plot_stemcount <- plot(model_stemcount_main)

#ggeffects
install.packages("ggeffects")
library(ggeffects)
stemcountggtest <- ggpredict(model_stemcount_main,"distance_scaled") %>% plot


#########################################Proportion Flowering and Distance from 2016##############

#making dataframe for proportion flowering with seedlings removed
prop_flowering_ns <- df_no_seedlings %>% 
  select(Site,flowering_numerical,Foreland_Code,elev,Distance_from_Glacial_Terminus_.m.) %>%
  group_by(Site) %>%
  mutate(prop_f_site = mean(flowering_numerical)) %>%
  select(-flowering_numerical) %>%
  unique()
prop_flowering_ns$timezone <- timezones
prop_flowering_ns$numerical_timezones <- timezones_numerical

prop_flowering_ns$distance_from_2016 <- prop_flowering_ns$Distance_from_Glacial_Terminus_.m.

###scatterplot for flowering and distance

flowering_distance_scatter <- ggplot(prop_flowering, aes(x = distance_from_2016, y = prop_f_site, fill= Foreland_Code)) + geom_point()+geom_smooth(method="lm")+ labs(x="Distance from 2016 Line (m)", y="Proportion Flowering", title="Proportion Flowering vs. Distance from 2016")

#scaling predictor variables
prop_flowering_ns$distance_scaled <- scale(prop_flowering_ns$distance_from_2016, center=TRUE)[,1]
#prop_flowering_ns$elev_scaled <- scale(prop_flowering_ns$elev, center=TRUE)[,1]

model_flowering_av <- lmer(prop_f_site ~ distance_scaled+ (1|Foreland_Code), data=prop_flowering_ns)

########################model for flowering and distance (from main dataframe)############

#change flowering to factor
df_no_seedlings$flowering_factor <- as.factor(df_no_seedlings$flowering_numerical)
#change flowering to factor in dataframe with julian days
lewisii_data2$flowering_factor <- as.factor(lewisii_data2$flowering_numerical)

#standardize predictor variable
library(standardize)
df_no_seedlings$distance_scaled <- scale(df_no_seedlings$dist, center=TRUE)[,1]
#doing this for julian days dataframe (using this instead to include date)
lewisii_data2$distance_scaled <- scale(lewisii_data2$dist, center=TRUE)[,1]
lewisii_data2$jday <- lewisii_data2$Julian.Day

#install.packages("optimx")
library(optimx)

model_flowering_main <- glmer(flowering_factor ~ distance_scaled +(1|Foreland_Code), data = df_no_seedlings, family = "binomial")
                
#optimizer code?                                           
control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
                              
summary(model_flowering_main)
anova(model_flowering_main)
resid_plot_flowering <- plot(model_flowering_main)

vis_model_flowering <- visreg(
  model_flowering_main,
  main = "Predicted Average Stem Count vs. Distance from 2016  Glacial
  Terminus",
  gg=TRUE,
  ylab = "log(Predicted Average Stem Count)",
  xlab = "Distance from 2016 Terminus",
  line.par = list(col = 'orange'),
  points.par = list(col =
                      'red', cex = 1, pch = 1))
 

#looking at residuals
modplot_f <- plot(model_flowering_main)
#looks okay
#glm instead of glmer (don't use)
#model_flowering_glm <- glm(flowering_factor ~ dist, data=df_no_seedlings, family="binomial")
#summary(model_flowering_glm)
#visflowering_glm <- visreg(model_flowering_glm)


########################model for flowering and distance from averages############

flowering_df_HM_only <- prop_flowering_ns %>% filter(Foreland_Code=="HM")
flowering_df_GB_only <- prop_flowering_ns %>% filter(Foreland_Code=="GB")

#Helm only
model_floweringHM_av <- lm(prop_f_site ~ distance_from_2016, data = flowering_df_HM_only)
summary(model_floweringHM_av)
anova(model_floweringHM_av)
visreg(model_floweringHM_av)

#Garibaldi only 
model_floweringGB_av <- lm(prop_f_site ~ distance_from_2016,data = flowering_df_GB_only)
summary(model_floweringGB_av)
anova(model_floweringGB_av)
visreg(model_floweringGB_av)

#for both
prop_flowering_ns$flowering_scaled <- scale(prop_flowering_ns$prop_f_site)
model_flowering_distance <-  lmer(prop_f_site ~ distance_scaled + (1|Foreland_Code), data=prop_flowering_ns)
summary(model_flowering_distance)
anova(model_flowering_distance)
#with lm 
model_flowering_lm <- lm(prop_f_site ~ distance_scaled, data=prop_flowering_ns)
summary(model_flowering_lm)
anova(model_flowering_lm)

####visreg code
vis_flowering_distance <- visreg(
  model_flowering_lm,
  main = "Predicted Proportion of Flowering Plants vs. Distance from 2016 Glacial
  Terminus",
  ylab = "Predicted Proportion of Flowering Plants",
  xlab = "Distance from 2016 Terminus",
  gg = TRUE,
  line.par = list(col = 'orange'),
  points.par = list(col = 'red', cex = 1, pch =
                      #add call
                      1))

#visreg output for flowering and distance by foreland (ignore)

vistest <- visreg(
  model_flowering_distance,
  "prop_f_site",
  strip.names = c("Helm", "Garibaldi"),
  by="Foreland_Code",
  main = "Predicted Proportion of Flowering Plants vs. Distance from 2016 Glacial
  Terminus",
  ylab = "Predicted Proportion of Flowering Plants",
  xlab = "Distance from 2016 Terminus",
  gg = TRUE,
  line.par = list(col = 'orange'),
  points.par = list(col = 'red', cex =
                      1, pch = 1)
)



############################Seedlings and Distance from 2016###########################
library(ggplot2)

#scale variables - don't need
#prop_flowering$distance_scaled <- scale(prop_flowering$distance_from_2016, center=TRUE)[,1]
#prop_flowering$elev_scaled <- scale(prop_flowering$elev, center=TRUE)[,1]

model_seedlings_distance <- lmer(number_of_seedlings ~ distance_from_2016 + (1|Foreland_Code), data=prop_flowering)

summary(model_seedlings_distance)
anova(model_seedlings_distance)
#looking at residuals
modplot_s <- plot(model_seedlings_distance)
#looks okay

########################make seedlings model with data from main dataframe####################

#make seedling (0 stem count) a "Y" "N" binary in new column
lewisii_data$seedling <- ifelse(lewisii_data$Stem_Count=='0', 1,0)
lewisii_data$seedling_factor <- as.factor(lewisii_data$seedling)

#model
#scale distance
lewisii_data$distance_scaled <- scale(lewisii_data$Distance_from_Glacial_Terminus_.m.,center=TRUE)[,1]
#make model
model_seedlings_main <- glmer(seedling_factor ~ distance_scaled + (1|Foreland_Code), data = lewisii_data, family = "binomial")
summary(model_seedlings_main)
anova(model_seedlings_main)
resid_plot_seedlings <- plot(model_seedlings_main)

#doing likelihood ratio test instead of anova (more appropriate)
install.packages("lmtest")
library(lmtest)
#null model
model_seedlings_null <- glmer(seedling_factor ~ (1|Foreland_Code), data = lewisii_data, family = "binomial")
#lrtest
lrtest(model_seedlings_main,model_seedlings_null)

#visreg
vis_seedlings_main <- visreg(
  model_seedlings_main,
  main = "Predicted Number of Seedlings vs. Distance from 2016 Glacial Terminus",
  ylab = "Predicted Number of Seedlings",
  xlab = "Distance from 2016 Terminus",
  gg = TRUE,
  scale=("response"),
  line.par = list(col = 'orange'),
  points.par = list(col = 'red', cex =
                      1, pch = 1)
)

#trying ggeffects
library(ggeffects)

ggseedling <- ggpredict(model_seedlings_main,terms="distance_scaled")
ggseedlingplot <- plot(ggtest,rawdata=TRUE) + labs(
  x = "Scaled Distance from Glacial Terminus", 
  y = "Predicted Seedling Probability", 
  title = "Probability of being a Seedling vs. Distance from Glacial Terminus")

####################################putting models on one page######################################################

library(gridExtra)
gridmodels <- grid.arrange(vis_density_distance,vis_flowering_main,vis_stemcount_main,vis_seedlings_main, ncol=2)

###residuals plots

gridresiduals <- grid.arrange(resid_plot_distance,resid_plot_flowering,resid_plot_seedlings,resid_plot_stemcount)

                        
##########notes ect#####################
                            

#think about using spatial analysis packages
#LR test or AIC values (which is lower) to see if slope and intercept is a better fit or just intercept

#change interactions in model (*)
#scale variables (just predictors)
#+ centre = true
#look at model diagnostics
#put anova tables into a csv


