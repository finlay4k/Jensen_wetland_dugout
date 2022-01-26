## Paired dugout and wetland analysis ##
## Author: Sydney Jensen ##
## Last Updated: January 25 2022 ##
## Introduction:
## Data was collected from 20 dugouts and 20 wetlands (1 dugout and 1 wetland = 1 pair) across Saskatchewan
## Each pair was located close to each other (usually on the same section of land) 
## To be considered close, they had to have natural landscape between each waterbody (no man-made structures) 
## the natural landscape that each waterbody was on needed to be of the same land use category

# Initial Setup ####
# load required packages
pkgs <- c("readxl","ggplot2", "tidyverse", "mgcv", "mgcViz")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

# read in master spreadsheet with all data
master <- read_xlsx("data/Paired_Master2019.xlsx", na = c("#VALUE!", "NA", "", "#N/A"))

# read in flux spreadsheet with all data
flux_full <- read.csv("data/Paired_GHGflux_2019_av&errors.csv") 

#take out only cole and caraco flux values
flux <- flux_full %>% 
  select(c('Site_ID', 'Waterbody', 'flux_CO2_CC', 'flux_CH4_CC', 'flux_N2O_CC'))

#add in column for type of flux
flux <- flux %>% mutate(Flux_Type = 'Diffusive')

# convert waterbody type to a factor
master$Waterbody <- as.factor(master$Waterbody)
flux$Waterbody <- as.factor(flux$Waterbody)

# set a theme for plots
SAE_theme  <- theme_bw(base_size = 12)

# transform possible predictor variables to be approximately normally distributed

master <- mutate(master, sqrt_bf = sqrt(b.f.max), 
                         log_DIN = log10(DIN.ug.N.L),
                         log_NH3 = log10(NH3.ug.N.L),
                         log_DOC = log10(DOC.uM),
                         log_DIC = log10(DIC.uM),
                         log_SRP = log10(SRP.ug.P.L),
                         log_CHL = log10(Chla),
                         sqrt_do = sqrt(Surface_DO.sat),
                         log_alk = log10(Meas_Alk.mg.L),
                         log_CO2 = log10(CO2.uM),
                         log_CH4 = log10(CH4.uM),
                         log_N2O = log10(N2O.nM),
                         log_TDN = log10(TN.ug.N.L),
                         log_TDP = log10(TP.ug.P.L),
                         log_cond = log10(Surface_Cond),
                         log_NOx = log10(Nitrate_Nitrite.ug.N.L),
                         log_depth = log10(Depth.m))

# test normality of variables - if p > 0.05 then data is not significantly different from normal
shapiro.test(master$sqrt_bf) #p = 0.07
shapiro.test(master$log_DIN) #p = 0.12
shapiro.test(master$log_DOC) #p = 0.27
shapiro.test(master$log_DIC) #p = 0.97
shapiro.test(master$log_SRP) #p = 0.22
shapiro.test(master$log_CHL) #p = 0.42
shapiro.test(master$sqrt_do) #p = 0.051
shapiro.test(master$log_alk) #p = 0.74
shapiro.test(master$log_NOx) #p = 0.018
shapiro.test(master$Surface_pH) #p = 0.7822
shapiro.test(master$log_cond) #p = 0.2625
shapiro.test(master$log_depth) #p = 0.006


# correlation table for measured variables

dugoutcor <- master %>% 
  filter(Waterbody == "Reservoir")

wetlandcor <- master %>% 
  filter(Waterbody == "Wetland")


#Correlation Table
library(PerformanceAnalytics)

dugoutcormat<-dugoutcor[c("Surface_Temp", 'Depth.m', "relative.depth.percent", 'b.f.max', "Surface_pH",
                          "Surface_DO.mg.L","Deep_DO.mg.L", "Surface_Cond", 'DIC.uM', 'DOC.uM', "Meas_Alk.mg.L", 
                          "TP.ug.P.L", "SRP.ug.P.L",  "TN.ug.N.L", "Nitrate_Nitrite.ug.N.L", "NH3.ug.N.L",
                          "Chla", 'CO2.uM', 'CH4.uM', "N2O.nM")]

wetlandcormat<-wetlandcor[c("Surface_Temp", 'Depth.m', "relative.depth.percent", 'b.f.max', "Surface_pH",
                            "Surface_DO.mg.L","Deep_DO.mg.L", "Surface_Cond", 'DIC.uM', 'DOC.uM', "Meas_Alk.mg.L", 
                            "TP.ug.P.L", "SRP.ug.P.L",  "TN.ug.N.L", "Nitrate_Nitrite.ug.N.L", "NH3.ug.N.L",
                            "Chla", 'CO2.uM', 'CH4.uM', "N2O.nM")]

colnames(dugoutcormat) <- c('Temperature', "Depth", "Rel. Depth", "Strat. Strength", "pH", 'Surf. DO', 'Deep DO', 'Conductivity',
                            'DIC', 'DOC', 'Alkalinity', 'TDP', "SRP", 'TDN', 'NOx', 'NH3', 'Chl. a',
                            'CO2', 'CH4', 'N2O')

colnames(wetlandcormat) <- c('Temperature', "Depth", "Rel. Depth", "Strat. Strength", "pH", 'Surf. DO', 'Deep DO', 'Conductivity',
                             'DIC', 'DOC', 'Alkalinity', 'TDP', "SRP", 'TDN', 'NOx', 'NH3', 'Chl. a',
                             'CO2', 'CH4', 'N2O')


#Remove rows with NAs

dogoutcormat2 <- dugoutcormat[complete.cases(dugoutcormat),]

wetlandcormat2 <- wetlandcormat[complete.cases(wetlandcormat),]

dugout.cor = cor(dogoutcormat2, method = c("spearman"))
wetland.cor = cor(wetlandcormat2, method = c("spearman"))


library(viridis)
library(ggcorrplot)

dugoutggcor <- ggcorrplot(dugout.cor, hc.order = FALSE, type = "lower",
                          lab = TRUE) + SAE_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_viridis() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), legend.position = 'top') 
dugoutggcor

ggsave("Dugout_Correlation_Table.png", dugoutggcor)


wetlandggcor <- ggcorrplot(wetland.cor, hc.order = FALSE, type = "lower",
                           lab = TRUE) + SAE_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_viridis() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), legend.position = 'top') 

wetlandggcor

ggsave("Wetland_Correlation_Table.png", wetlandggcor)


# exploring the differences between water quality parameters between dugouts and wetlands ####
# two sample t-test for each gas to determine if a significant difference is present between waterbodies
# Assumptions of a two sample t-test
# 1. Are the two samples independents? 
# 2. Are the data from each of the 2 groups following a normal distribution?
# 3. Do the two populations have the same variance?

# null hypothesis: the means of the different groups are the same
# alternative hypothesis: the means of the different groups are different

# a non-parametric two-samples wilcoxon rank test should be used to examine the differences
# this is because it violates assumption #2 of the two sample t-test

# test normality of water quality variables to determine if a t-test or wilcoxon rank test should be used
# t-test used when both waterbodies are normally distributed for each parameter
# wilcoxon rank test used when one or both waterbodies are not normally distributed

# surface temperature
# Shapiro-wilks normality test surface temperature
with(master, shapiro.test(Surface_Temp[Waterbody == "Reservoir"])) #p = 0.28 normal
with(master, shapiro.test(Surface_Temp[Waterbody == "Wetland"])) #p = 0.87 normal

temp_t.test <- t.test(Surface_Temp ~ Waterbody, data = master)
# p = 0.83
# accept null hypothesis - not different

# depth
# Shapiro-wilks normality test maximum depth
with(master, shapiro.test(Depth.m[Waterbody == "Reservoir"])) #p = 0.069 normal
with(master, shapiro.test(Depth.m[Waterbody == "Wetland"])) #p = 0.0001 not normal

depth_wilcox <- wilcox.test(Depth.m ~ Waterbody, data = master) 
# p = 2.7e-7
# reject null hypothesis - different

# relative depth
# Shapiro-wilks normality test relative depth
with(master, shapiro.test(relative.depth.percent[Waterbody == "Reservoir"])) #p = 0.71 normal
with(master, shapiro.test(relative.depth.percent[Waterbody == "Wetland"])) #p = 1.99e-5 not normal

reldepth_wilcox <- wilcox.test(relative.depth.percent ~ Waterbody, data = master) 
# p = 5.4e-9
# reject null hypothesis - different

# buoyancy frequency
# Shapiro-wilks normality test 
with(master, shapiro.test(b.f.max[Waterbody == "Reservoir"])) #p = 1.798e-05 not normal
with(master, shapiro.test(b.f.max[Waterbody == "Wetland"])) #p = 3.518e-05 not normal

secchi_wilcox <- wilcox.test(b.f.max ~ Waterbody, data = master) 
# p = 0.006729
# reject null hypothesis - different

# surface pH
# Shapiro-wilks normality test pH
with(master, shapiro.test(Surface_pH[Waterbody == "Reservoir"])) #p = 0.027 not normal
with(master, shapiro.test(Surface_pH[Waterbody == "Wetland"])) #p = 0.329 normal

pH_wilcox <- wilcox.test(Surface_pH ~ Waterbody, data = master) 
# p = 0.0658
# accept null hypothesis - not different

# dissolved oxygen saturation
# Shapiro-wilks normality test DO
with(master, shapiro.test(Surface_DO.sat[Waterbody == "Reservoir"])) #p = 0.11 normal
with(master, shapiro.test(Surface_DO.sat[Waterbody == "Wetland"])) #p = 0.8225 normal

DO_t.test <- t.test(Surface_DO.sat ~ Waterbody, data = master) 
# p = 0.2322
# accept null hypothesis - not different

# Shapiro-wilks normality test deep DO
with(master, shapiro.test(Deep_DO.sat[Waterbody == "Reservoir"])) #p = 0.9.623e-06 not normal
with(master, shapiro.test(Deep_DO.sat[Waterbody == "Wetland"])) #p = 0.4255 normal

DO_t.test <- t.test(Deep_DO.sat ~ Waterbody, data = master) 
# p = 0.001743
# reject null hypothesis - different

# conductivity
# Shapiro-wilks normality test conductivity
with(master, shapiro.test(Surface_Cond[Waterbody == "Reservoir"])) #p = 0.0014 not normal
with(master, shapiro.test(Surface_Cond[Waterbody == "Wetland"])) #p = 9.04e-6 not normal

Cond_wilcox <- wilcox.test(Surface_Cond ~ Waterbody, data = master) 
# p = 0.445
# accept null hypothesis - not different

# salinity
# Shapiro-wilks normality test salinity
with(master, shapiro.test(Surface_Sal.ppt[Waterbody == "Reservoir"])) #p = 0.0009 not normal
with(master, shapiro.test(Surface_Sal.ppt[Waterbody == "Wetland"])) #p = 3.9e-6 not normal

Sal_wilcox <- wilcox.test(Surface_Sal.ppt ~ Waterbody, data = master) 
# p = 0.5249
# accept null hypothesis - not different

# dissolved inorganic carbon  
# Shapiro-wilks normality test DIC
with(master, shapiro.test(DIC.mg.L[Waterbody == "Reservoir"])) #p = 0.1687 normal
with(master, shapiro.test(DIC.mg.L[Waterbody == "Wetland"])) #p = 0.04337 not normal

DIC_wilcox <- wilcox.test(DIC.mg.L ~ Waterbody, data = master) 
# p = 0.5648
# accept null hypothesis - not different

# dissolved organic carbon  
# Shapiro-wilks normality test DOC
with(master, shapiro.test(DOC.mg.L[Waterbody == "Reservoir"])) #p = 0.0105 not normal
with(master, shapiro.test(DOC.mg.L[Waterbody == "Wetland"])) #p = 0.0024 not normal

DOC_wilcox <- wilcox.test(DOC.mg.L ~ Waterbody, data = master) 
# p = 0.0002
# reject null hypothesis - different

# total dissolved phosphorus  
# Shapiro-wilks normality test TDP
with(master, shapiro.test(TP.ug.P.L[Waterbody == "Reservoir"])) #p = 2.02e-5 not normal
with(master, shapiro.test(TP.ug.P.L[Waterbody == "Wetland"])) #p = 1.895e-5 not normal

TDP_wilcox <- wilcox.test(TP.ug.P.L ~ Waterbody, data = master) 
# p = 0.02551
# reject null hypothesis - different



# soluble reactive phosphorus
# Shapiro-wilks normality test SRP
with(master, shapiro.test(SRP.ug.P.L[Waterbody == "Reservoir"])) #p = 1.18e-5 not normal
with(master, shapiro.test(SRP.ug.P.L[Waterbody == "Wetland"])) #p = 2.27e-6 not normal

SRP_wilcox <- wilcox.test(SRP.ug.P.L ~ Waterbody, data = master) 
# p = 0.1296
# accept null hypothesis - not different

# total dissolved nitrogen
# Shapiro-wilks normality test TDN
with(master, shapiro.test(TN.ug.N.L[Waterbody == "Reservoir"])) #p = 0.0012 not normal
with(master, shapiro.test(TN.ug.N.L[Waterbody == "Wetland"])) #p = 0.0014 not normal

TDN_wilcox <- wilcox.test(TN.ug.N.L ~ Waterbody, data = master) 
# p = 0.0003722
# reject null hypothesis - different

# combined nitrate and nitrite
# Shapiro-wilks normality test NOx
with(master, shapiro.test(Nitrate_Nitrite.ug.N.L[Waterbody == "Reservoir"])) #p = 8.253-5 not normal
with(master, shapiro.test(Nitrate_Nitrite.ug.N.L[Waterbody == "Wetland"])) #p = 0.0013 not normal

NOx_wilcox <- wilcox.test(Nitrate_Nitrite.ug.N.L ~ Waterbody, data = master) 
# p = 0.2534
# accept null hypothesis - not different

# ammonia
# Shapiro-wilks normality test NH3
with(master, shapiro.test(NH3.ug.N.L[Waterbody == "Reservoir"])) #p = 2.512e-5 not normal
with(master, shapiro.test(NH3.ug.N.L[Waterbody == "Wetland"])) #p = 0.0009 not normal

NH3_wilcox <- wilcox.test(NH3.ug.N.L ~ Waterbody, data = master) 
# p = 0.1283
# accept null hypothesis - not different

# Chlorophyll a
# Shapiro-wilks normality test Chl a 
with(master, shapiro.test(Chla[Waterbody == "Reservoir"])) #p = 7.699e-7 not normal
with(master, shapiro.test(Chla[Waterbody == "Wetland"])) #p = 1.832e-5 not normal

Chl_wilcox <- wilcox.test(Chla ~ Waterbody, data = master) 
# p = 0.3834
# accept null hypothesis - not different

# alkalinity
# Shapiro-wilks normality test alk
with(master, shapiro.test(Meas_Alk.mg.L[Waterbody == "Reservoir"])) #p = 0.1678 normal
with(master, shapiro.test(Meas_Alk.mg.L[Waterbody == "Wetland"])) #p = 0.01473 not normal

Alk_wilcox <- wilcox.test(Meas_Alk.mg.L ~ Waterbody, data = master) 
# p = 0.1417
# accept null hypothesis - not different

#buoyancy frequency
with(master, shapiro.test(b.f.max[Waterbody == "Reservoir"])) #p = 1.798e-05 not normal
with(master, shapiro.test(b.f.max[Waterbody == "Wetland"])) #p = 3.518e-05 not normal

bf_wilcox <- wilcox.test(b.f.max ~ Waterbody, data = master) 
# p = 0.006729
# accept null hypothesis - different

# summary of water quality parameters for table 1 ####

filtered <- master %>% 
  select(Waterbody, Surface_Temp, Depth.m, relative.depth.percent, b.f.max, Surface_pH, Surface_DO.sat, Deep_DO.sat, Surface_Cond, DIC.mg.L, DOC.mg.L, 
         Meas_Alk.mg.L, TP.ug.P.L, SRP.ug.P.L, TN.ug.N.L, Nitrate_Nitrite.ug.N.L, NH3.ug.N.L, Chla, CO2.uM, CH4.uM, N2O.nM)
library(vtable)
sumtable(data = filtered, group = 'Waterbody', group.test = TRUE, out = 'csv', file = 'Summary_Stats.csv')
#saved as CSV and formatted into table in powerpoint


# exploring the differences between GHG concentrations between dugouts and wetlands ####
# compute summary statistics by group for each gas and visualize using boxplot
# CO2 
group_by(master, Waterbody) %>%
  summarise(
    count = n(),
    mean = mean(CO2.uM, na.rm = TRUE),
    min = min(CO2.uM, na.rm = T),
    max = max(CO2.uM, na.rm = T),
    sd = sd(CO2.uM, na.rm = TRUE))

# CH4
group_by(master, Waterbody) %>%
  summarise(
    count = n(),
    mean = mean(CH4.uM, na.rm = TRUE),
    min = min(CH4.uM, na.rm = T),
    max = max(CH4.uM, na.rm = T),
    sd = sd(CH4.uM, na.rm = TRUE))


# N2O
group_by(master, Waterbody) %>%
  summarise(
    count = n(),
    mean = mean(N2O.nM, na.rm = TRUE),
    min = min(N2O.nM, na.rm = T),
    max = max(N2O.nM, na.rm = T),
    sd = sd(N2O.nM, na.rm = TRUE))


#flux 
summarise(flux,  mean = mean(flux_CO2_CC, na.rm = TRUE),  sd = sd(flux_CO2_CC, na.rm = TRUE))
summarise(flux,  mean = mean(flux_CH4_CC, na.rm = TRUE),  sd = sd(flux_CH4_CC, na.rm = TRUE))
summarise(flux,  mean = mean(flux_N2O_CC, na.rm = TRUE),  sd = sd(flux_N2O_CC, na.rm = TRUE))

group_by(flux, Waterbody) %>% 
  summarise(
    count = n(),
    mean = mean(flux_CO2_CC, na.rm = TRUE),
    min = min(flux_CO2_CC, na.rm = T),
    max = max(flux_CO2_CC, na.rm = T),
    sd = sd(flux_CO2_CC, na.rm = TRUE))

group_by(flux, Waterbody) %>% 
  summarise(
    count = n(),
    mean = mean(flux_CH4_CC, na.rm = TRUE),
    min = min(flux_CH4_CC, na.rm = T),
    max = max(flux_CH4_CC, na.rm = T),
    sd = sd(flux_CH4_CC, na.rm = TRUE))

group_by(flux, Waterbody) %>% 
  summarise(
    count = n(),
    mean = mean(flux_N2O_CC, na.rm = TRUE),
    min = min(flux_N2O_CC, na.rm = T),
    max = max(flux_N2O_CC, na.rm = T),
    sd = sd(flux_N2O_CC, na.rm = TRUE))

#measured ebullition
group_by(master, Waterbody) %>% 
  summarise(
    count = n(),
    mean = mean(CH4_Ebullition, na.rm = TRUE),
    min = min(CH4_Ebullition, na.rm = T),
    max = max(CH4_Ebullition, na.rm = T),
    sd = sd(CH4_Ebullition, na.rm = TRUE))

# two sample t-test for each gas to determine if a significant difference is present between waterbodies
# Assumptions of a two sample t-test
# 1. Are the two samples independents? Yes, samples are from 
# 2. Are the data from each of the 2 groups following a normal distribution?
# 3. Do the two populations have the same variance?

# CO2
# null hypothesis: the means of the different groups are the same
# alternative hypothesis: the means of the different groups are different
# test normality from each of the groups
# Shapiro-wilks normality test for dugouts
with(master, shapiro.test(CO2.uM[Waterbody == "Reservoir"])) #p = 6.6e-7 not normal

# Shapiro-wilks normality test for wetlands
with(master, shapiro.test(CO2.uM[Waterbody == "Wetland"])) #p = 9.0e-8 not normal

# CO2 concentrations are not normally distributed
# a non-parametric two-samples wilcoxon rank test should be used to examine the differences
# this is because it violates assumption #2 of the two sample t-test

CO2_wilcox <- wilcox.test(CO2.uM ~ Waterbody, data = master) 
# p = 0.05956 
# accept null hypothesis

# conclusion: CO2 concentrations between dugouts and wetlands are not different


# CH4
# null hypothesis: the means of the different groups are the same
# alternative hypothesis: the means of the different groups are different
# test normality from each of the groups
# Shapiro-wilks normality test for dugouts
with(master, shapiro.test(CH4.uM[Waterbody == "Reservoir"])) #p = 0.0002 not normal

# Shapiro-wilks normality test for wetlands
with(master, shapiro.test(CH4.uM[Waterbody == "Wetland"])) #p = 0.0006 not normal

# CH4 concentrations are not normally distributed
# a non-parametric two-samples wilcoxon rank test should be used to examine the differences
# this is because it violates assumption #2 of the two sample t-test

CH4_wilcox <- wilcox.test(CH4.uM ~ Waterbody, data = master) 
# p = 0.862 
# accept null hypothesis

# Conclusion: CH4 concentrations between dugouts and wetlands are not different

# N2O
# null hypothesis: the means of the different groups are the same
# alternative hypothesis: the means of the different groups are different
# test normality from each of the groups
# Shapiro-wilks normality test for dugouts
with(master, shapiro.test(N2O.nM[Waterbody == "Reservoir"])) # p = 0.001 not normal
 
# Shapiro-wilks normality test for wetlands
with(master, shapiro.test(N2O.nM[Waterbody == "Wetland"])) # p = 0.38 normal

# N2O concentrations are not normally distributed in reservoirs but are in wetlands
# a non-parametric two-samples wilcoxon rank test should be used to examine the differences
# this is because it violates assumption #2 of the two sample t-test

N2O_wilcox <- wilcox.test(N2O.nM ~ Waterbody, data = master) 
# p = 0.002643
# reject null hypothesis

# Conclusion: N2O concentrations between dugouts and wetlands are significantly different
# N2O concentrations are significantly lower in wetlands than they are in dugouts
# Based on visual inspection of the N2O plot and results of the statistical tests

# Models comparing the controls of GHG between dugouts and wetlands ####
# CO2 Models ####
# pH only model ####
paired_pH <- gam(CO2.uM ~ Waterbody + 
                   s(Surface_pH, by = Waterbody, k = 10),
                 data = master, family = Gamma(link = 'log'), select = TRUE,
                 method = 'REML')

# checking the diagnostics of the model to make sure assumptions aren't violated
layout(matrix(1:4, ncol = 2))
gam.check(paired_pH, rep = 100)
layout(1)

# summary output for the model 
summary(paired_pH)
# deviance explained = 83 %
# pH of the surface water is significant in dugouts and wetlands

# partial effects plots for the model
plot(paired_pH, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE)       
# Strong negative relationship between CO2 concentration and pH seen in both dugouts and wetlands
# expected based on carbonate equilibrium and previous studies (finlay et al. 2015 and webb et al. 2019b)

# CO2 control models ####
# look at nutrient ratios instead of straight relationships with nutrients
# DOC:NOx, DOC:SRP, NOx:SRP
# create columns to make all the units the same 
# DOC = mg C/L
# NOx = ug N/L / 1000 = mg N/L
# SRP = ug P/L /1000 = mg P/L
master <- master %>%
  mutate(NOx.mg.N.L = Nitrate_Nitrite.ug.N.L/1000,
         SRP.mg.P.L = SRP.ug.P.L/1000,
         DOC.mg.C.L = DOC.mg.L, 
         DIN.mg.N.L = DIN.ug.N.L/1000)

#create columns for nutrient ratios - log scale to normalize
master <- master %>% 
  mutate(log_DOC.NOx = log10(DOC.mg.C.L/NOx.mg.N.L), 
         log_DOC.SRP = log10(DOC.mg.C.L/SRP.mg.P.L), 
         log_NOx.SRP = log10(NOx.mg.N.L/SRP.mg.P.L),
         log_DIN.SRP = log10(DIN.mg.N.L/SRP.mg.P.L),
         log_DOC.DIN = log10(DOC.mg.C.L/DIN.mg.N.L))

#all three nutrient ratios
paired_CO2 <- gam(CO2.uM ~ Waterbody + 
  s(Surface_DO.sat, by = Waterbody, k = 3) +
  s(log_alk, by = Waterbody, k = 3) +
  s(sqrt_bf, by = Waterbody, k = 3) +
  s(log_CHL, by = Waterbody, k = 3) +
  s(log_DOC.NOx, by = Waterbody, k = 3) +
  s(log_DOC.SRP, by = Waterbody, k = 3), 
data = master, family = Gamma(link = 'log'), select = TRUE,
method = 'REML')

# checking the diagnostics of the model to make sure assumptions aren't violated
layout(matrix(1:4, ncol = 2))
gam.check(paired_CO2, rep = 100)
layout(1)
# model check looks good - histogram of residuals isn't perfect
# k-index is ~1.0 or greater for all variables


# summary output for the model 
summary(paired_CO2)
7# deviance explained = 87.7%
# wetland significant terms: alk, bf, DOC:NOx
# dugout significant terms: DO, CHL, DOC:NOx
# linear effect of waterbody is not significant

#check concurvity of best model (paired_CO2)
CO2conc <- concurvity(paired_CO2, full = F)
write.csv(CO2conc, "CO2_concurvity.csv") #concurvity looks okay - worst does not have any parameters over 0.8

# CH4 Models ####
paired_CH4 <- gam(CH4.uM ~ Waterbody + 
                      s(Surface_DO.sat, by = Waterbody, k = 3) +
                      s(Deep_DO.sat, by = Waterbody, k = 3) +
                      s(log_CHL, by = Waterbody, k = 3) +
                      s(log_cond, by = Waterbody, k = 3) +
                      s(sqrt_bf, by = Waterbody, k = 3) +
                      s(log_DOC.NOx, by = Waterbody, k = 3) +
                      s(log_NOx.SRP, by = Waterbody, k = 3),
                    data = master, family = tw(link = 'log'), select = TRUE,
                    method = 'REML')

# checking the diagnostics of the model to make sure assumptions aren't violated
layout(matrix(1:4, ncol = 2))
gam.check(paired_CH4, rep = 100)
layout(1)
# summary output for the model 
summary(paired_CH4)

# deviance explained = 86.2% 
# wetland significant terms: cond, bf
# dugout significant terms: do, doc:nox, cond, bf, chl

# partial effects plots for the model
plot(paired_CH4, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE) 

CH4concv <- concurvity(paired_CH4, full = F) #looks good - worst estimate isn't over 0.8
write.csv(CH4concv, "CH4_concurvity.csv")

# N2O Models ####

paired_N2O <- gam(N2O.nM ~ Waterbody + 
                       s(Surface_DO.sat, by = Waterbody, k = 3) +
                       s(Deep_DO.sat, by = Waterbody, k = 3) +
                       s(log_NOx, by = Waterbody, k = 3) +
                       s(log_CHL, by = Waterbody, k = 3) +
                       s(sqrt_bf, by = Waterbody, k = 3) +
                       s(log_DOC, by = Waterbody, k = 3),
                     data = master, family = Gamma(link = 'identity'), select = TRUE,
                     method = 'REML')

# checking the diagnostics of the model to make sure assumptions aren't violated
layout(matrix(1:4, ncol = 2))
gam.check(paired_N2O, rep = 100)
layout(1)
# model check looks good


# summary output for the model 
summary(paired_N2O)
# deviance explained = 95.8 %
# linear predictor of waterbody is significant
# wetland significant terms: surf DO, NOx, chl, bf
# dugout significant terms: surf DO, deep DO, NOx, chl, doc

# partial effects plots for the model
plot(paired_N2O, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE)  

N2O_concurvity <- concurvity(paired_N2O, full = F) #looks good - worst parameter isn't over 0.8 for any
write.csv(N2O_concurvity, "N2O_concurvity.csv") 

# Estimating CH4 ebullition from Conductivity Relationship ####
#ebullition rates
group_by(master, Waterbody) %>%
  summarise(
    count = n(),
    mean = mean(Ebullition_Rate, na.rm = TRUE),
    min = min(Ebullition_Rate, na.rm = T),
    max = max(Ebullition_Rate, na.rm = T),
    sd = sd(Ebullition_Rate, na.rm = TRUE))

#regression - dugouts
dugouts <- subset(master, Waterbody == "Reservoir")
#Log-Log Relationship
log_cond.lm <- lm(log10(CH4_Ebullition) ~ log10(Surface_Cond), data = dugouts)
print(log_cond.lm)
summary(log_cond.lm)

#intercept = 6.414, slope = -2.555, adj. r = 0.6044

#regression - wetlands
wetlands <- subset(master, Waterbody == "Wetland")
#Log-Log Relationship
log_wet.cond.lm <- lm(log10(CH4_Ebullition) ~ log10(Surface_Cond), data = wetlands)
print(log_wet.cond.lm)
summary(log_wet.cond.lm)

#intercept = 8.794, slope = -3.067, adj. r = 0.7678

# CH4 percent composition and conductivity relationship ####
perc.cond.lm <- lm(log10(CH4_PercentComp) ~ log10(Surface_Cond), data = dugouts)
print(perc.cond.lm)
summary(perc.cond.lm)
#intercept = 5.836, slope = -1.947, adj. r = 0.8268

wet.perc.cond.lm <- lm(log10(CH4_PercentComp) ~ log10(Surface_Cond), data = wetlands)
print(wet.perc.cond.lm)
summary(wet.perc.cond.lm)
#intercept = 7.996, slope = -2.402, adj. r = 0.9504

master %>% 
  group_by(Waterbody) %>% 
  summarize(Mean = mean(CH4_PercentComp, na.rm = T),
            SD = sd(CH4_PercentComp, na.rm = T))

#predict CH4 ebullition from all sites based on dugout and wetland regressions ####
#add column for predicted CH4 ebullition based on each LM and then join back into the master dataframe 
dugouts <- dugouts %>% 
mutate(log_Pred.CH4_Eb = 6.414-2.555*(log_cond))

wetlands <- wetlands %>% 
  mutate(log_Pred.CH4_Eb = 8.7940-3.067*(log_cond))

master <- bind_rows(dugouts, wetlands)

#calculate CH4 ebullition in mmol m-2 day-2 from the log10 
master <- mutate(master, Pred_CH4eb.mmol = 10^(log_Pred.CH4_Eb))

#test for significant difference
wilcox.test(Pred_CH4eb.mmol ~ Waterbody, data = master) #p-value = 0.3689 - not different 

#calculate CO2-eq flux  ####  
#using neubauer and megonigal 2015 100 yr sustained GWP and GCP
#set up if then statements, if flux is greater than zero use the GWP, if less than zero use GCP

diffusion <- flux %>% 
  mutate(CH4_co2eq = if_else(flux_CH4_CC > 0, 45*(flux_CH4_CC), 203*(flux_CH4_CC)),
         N2O_co2eq = if_else(flux_N2O_CC > 0, 270*(flux_N2O_CC), 349*(flux_N2O_CC))) %>% 
  mutate(N2O_co2eq_mmol = N2O_co2eq/1000)%>% 
  mutate(CO2_g = (flux_CO2_CC)/1000*44.01,  #convert mmol to g
         CH4_CO2eq_g = (CH4_co2eq)/1000*16.04246, #convert mmol to g
         N2O_CO2eq_g = (N2O_co2eq)/1000000*44.013) #convert umol to g

#pull in ebullitive flux into the flux dataframe
ebullition <- master %>% 
  select(c("Site_ID", "Waterbody","Pred_CH4eb.mmol")) %>% 
  mutate(Flux_Type = 'Ebullitive', 
         flux_CH4_CC = Pred_CH4eb.mmol,
         flux_CO2_CC = NA,
         flux_N2O_CC = NA)

#make sure all columns between flux and ebullition are the same
ebullition <- ebullition %>% 
  select(c("Site_ID", "Waterbody", "flux_CO2_CC", "flux_CH4_CC", "flux_N2O_CC", "Flux_Type"))

ebullition <- ebullition %>% 
  mutate(CH4_co2eq = if_else(flux_CH4_CC > 0, 45*(flux_CH4_CC), 203*(flux_CH4_CC)),
         N2O_co2eq = if_else(flux_N2O_CC > 0, 270*(flux_N2O_CC), 349*(flux_N2O_CC)),
         N2O_co2eq_mmol = N2O_co2eq/1000) %>%
mutate(CO2_g = (flux_CO2_CC)/1000*44.01,
       CH4_CO2eq_g = (CH4_co2eq)/1000*16.04246,
       N2O_CO2eq_g = (N2O_co2eq)/1000000*44.013)

ebullition %>% 
  group_by(Waterbody) %>% 
  summarize(Mean = mean(flux_CH4_CC),
            SD = sd(flux_CH4_CC), 
            SEM = (sd(flux_CH4_CC)/sqrt(length(flux_CH4_CC))))


dif_eb_flux <- bind_rows(diffusion, ebullition)

#predicted ebullition
group_by(ebullition, Waterbody) %>% 
  summarise(
    count = n(),
    mean = mean(flux_CH4_CC, na.rm = TRUE),
    min = min(flux_CH4_CC, na.rm = T),
    max = max(flux_CH4_CC, na.rm = T),
    sd = sd(flux_CH4_CC, na.rm = TRUE))

library(tidyr)

long_dif_eb <- pivot_longer(dif_eb_flux, names_to = "Gases", values_to = "Values", cols = c(flux_CO2_CC, flux_CH4_CC, flux_N2O_CC, CH4_co2eq, N2O_co2eq, 
                                                                                            N2O_co2eq_mmol, CO2_g, CH4_CO2eq_g, N2O_CO2eq_g))
                                 

long_dif_eb <- long_dif_eb %>% 
  drop_na()

#to figure out how many observations are included in co2_eq_flux for error calculation
count <- long_dif_eb %>% filter(Gases %in% c("flux_CO2_CC", "CH4_co2eq", "N2O_co2eq_mmol"))

CO2eq_flux <- long_dif_eb %>% 
  filter(Gases %in% c("flux_CO2_CC", "CH4_co2eq", "N2O_co2eq_mmol")) %>% 
  group_by(Gases, Waterbody, Flux_Type) %>% 
  summarise(Mean = mean(Values))

CO2eq_flux$Gases[c(1,3)] <- "CH4 Diffusive"
CO2eq_flux$Gases[c(2,4)] <- "CH4 Ebullitive"

co2eq_variance <- long_dif_eb %>% 
  filter(Gases %in% c("flux_CO2_CC", "CH4_co2eq", "N2O_co2eq_mmol")) %>% 
  group_by(Gases, Waterbody, Flux_Type) %>% 
  summarise(Mean = mean(Values),
            Variance = var(Values)) 

co2eq_error<- co2eq_variance %>% 
  group_by(Waterbody) %>% 
  summarise(SOM = sum(Mean), 
            MofVar = mean(Variance)) %>% 
  mutate(stdev = sqrt(MofVar)) %>% 
  mutate(SEM = stdev/sqrt((length(count)/2)))

mean_flux_CH4 <- long_dif_eb %>% 
  filter(Gases %in% c("flux_CH4_CC")) %>% 
  group_by(Gases, Waterbody, Flux_Type) %>% 
  summarise(Mean = mean(Values),
            SD = sd(Values),
            SEM = (sd(Values)/sqrt(length(Values))))

mean_flux_CO2 <- long_dif_eb %>% 
  filter(Gases %in% c("flux_CO2_CC")) %>% 
  group_by(Gases, Waterbody) %>% 
  summarise(Mean = mean(Values),
            SD = sd(Values),
            SEM = (sd(Values)/sqrt(length(Values))))

mean_flux_N2O <- long_dif_eb %>% 
  filter(Gases %in% c("flux_N2O_CC")) %>% 
  group_by(Gases, Waterbody) %>% 
  summarise(Mean = mean(Values),
            SD = sd(Values),
            SEM = (sd(Values)/sqrt(length(Values))))

#cond-SO4 figure from Jackie's excel file ####
SO4 <- read_xlsx("data/SO4_Cond_Relationship.xlsx")

lm <- lm(SO4.mg.L ~ Surface_Cond, data = SO4)
summary(lm)
#intercept -150.6104, slope = 0.6263, adj r2 - 0.8245

#ANCOVA for Percent Compostion and Conductivity LM
library(car)
master <- mutate(master, log_CH4_PercentComp = log10(CH4_PercentComp))
model.1 = lm (log_CH4_PercentComp ~ log_cond + Waterbody + log_cond:Waterbody,
              data = master)

Anova(model.1, type="II")

model.2 = lm (log_CH4_PercentComp ~ log_cond + Waterbody,
              data = master)

Anova(model.2, type="II")

summary(model.2)
hist(residuals(model.2),
     col="darkgray")
plot(fitted(model.2),
     residuals(model.2))

