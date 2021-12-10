# Best Practices Analyses
# Meghan A. Balk
# meghan.balk@gmail.com

##Load packages----
require(tidyverse)
require(nlme)
require(dplyr)
require(ggplot2)
require(reshape2)
require(plyr)
require(stringr)
library(tidyr)
require(PEIP)

##Upload data----
pan <- read.csv("https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv", header = TRUE, stringsAsFactors = FALSE)
#data derived from Best_Practices_Data_Cleaning script
data <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/Projects/BestPracticesData/BPP.data.csv", header = TRUE, stringsAsFactors = FALSE)
length(unique(data$scientificName)) #3164

#remove outliers and juveniles
outlier <- c("outlier", "possible juvenile",
             "too few records", "less than 10 recoreds")

data.trim <- data[!(data$measurementStatus %in% outlier) & 
                   data$lifeStage != "juvenile",]
nrow(data.trim) #1668901
length(unique(data.trim$scientificName)) #2898
unique(data.trim$measurementStatus)
unique(data.trim$lifeStage)

write.csv(data.trim, "BPP.data.trim.csv")

##combine with pantheria----
sp.data <- unique(data.trim$scientificName) 
nrow(pan[pan$MSW05_Binomial %in% sp.data,]) #2269 spp shared

taxonomy.data <- merge(pan, data.trim, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = TRUE, all.x = FALSE)

##write taxonomy----
write.csv(taxonomy.data, "BPP.pan.data.csv")

##data for pan analyses
pan.data <- taxonomy.data[taxonomy.data$measurementType == "body mass" &
                          !is.na(taxonomy.data$measurementValue) &
                          !is.na(taxonomy.data$X5.1_AdultBodyMass_g),]

length(unique(pan.data$MSW05_Binomial)) #1601
min(pan.data$measurementValue)

##write data for pan analyses----
write.csv(pan.data, "pan.data.mass.csv")

##Q1 compare to pantheria----
stats <- pan.data %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(N = n()) %>%
  as.data.frame()
keep <- stats$MSW05_Binomial[stats$N >= 10]
pan.data <- pan.data[pan.data$MSW05_Binomial %in% keep,]
length(unique(pan.data$MSW05_Binomial)) #773
pan.data$measurementValue <- as.numeric(pan.data$measurementValue)

pan.adult_stats <- pan.data %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size = length(measurementValue), 
                   min.mass = min(measurementValue, na.rm = TRUE),
                   max.mass  = max(measurementValue, na.rm = TRUE),
                   avg.mass = mean(measurementValue, na.rm = TRUE),
                   sd.err.mass = sd(measurementValue, na.rm = TRUE)/sqrt(sample.size),
                   pan.mass = X5.1_AdultBodyMass_g[1],
                   mass.diff = (pan.mass - avg.mass),
                   mass.diff.se = mass.diff / sd.err.mass,
                   abs.mass.diff.se = abs(mass.diff.se), #observed t; number in t-units (counts of standard errors from each mean)
                   outside.3.sigma = abs.mass.diff.se > 3, #if true then greater than 3 std errors outside
                   critical.t = abs(qt(p = 0.025, df = (sample.size-1))),
                   diff.amt = abs.mass.diff.se-critical.t,
                   sig = diff.amt > 0, # true means sig diff
                   p.value.fromt.crit.t = 1-pt(abs.mass.diff.se, df = (sample.size-1)), #if p=0.05 then most of it is outside
                   p.value = t.test(measurementValue, mu = pan.mass, conf.level = 0.95, alternative = "two.sided")$p.value,
                   per.diff = abs(((avg.mass - pan.mass)/avg.mass)*100)) %>%
  as.data.frame()

pan.adult_stats$corr <- p.adjust(pan.adult_stats$p.value, method = "BH")
pan.adult_stats$corr.t.value <- p.adjust(pan.adult_stats$p.value.fromt.crit.t, method = "BH")
pan.adult_stats$sig.corr <- pan.adult_stats$corr <= 0.05 #TRUE means sig diff
pan.adult_stats$sig.corr.t.value <- pan.adult_stats$corr.t.value <= 0.05 #TRUE means sig diff

##write mass difference results----
write.csv(pan.adult_stats, "pan.results.csv")

##test for drivers of patterns----
plot(x = pan.adult_stats$sample.size, y = pan.adult_stats$mass.diff,
     ylab = "Mass difference",
     xlab = "Sample size")
model.N <- lm(pan.adult_stats$mass.diff ~ pan.adult_stats$sample.size)
summary(model.N)

plot(x = pan.adult_stats$avg.mass, y = pan.adult_stats$mass.diff,
     xlab = "Average mass (g)",
     ylab = "Mass difference")
model.mass <- lm(pan.adult_stats$mass.diff ~ pan.adult_stats$avg.mass)
summary(model.mass)

##Table 1----

#total
nrow(pan.adult_stats) #773
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE]) #244  #w/in limits (31.6%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE])  #529 #outside limits (68.4)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0])  #422 #above; pan mean is greater than ours (79.8%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0])  #107 #below; pan mean is less than ours (20.2%)

#<100
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 100]) #559 #(72.2% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 100]) #171 (30.6%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 100]) #388 (69.4%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 100]) #301 (77.6%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 100]) #83 (21.4%)

#100-1000
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 1000 & pan.adult_stats$avg.mass > 100]) #125 #(16.2% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #44 (35.2%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #81 (64.8%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #64 (79%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #17 (21%)

#1000-10000
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 10000 & pan.adult_stats$avg.mass > 1000]) #46 #6(% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 10000 & pan.adult_stats$avg.mass > 1000]) #18 (39.1%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 10000 & pan.adult_stats$avg.mass > 1000]) #28 (60.9%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 10000  & pan.adult_stats$avg.mass > 1000]) #26 (92.9%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 10000  & pan.adult_stats$avg.mass > 1000]) #2 (7.1%)

#10000-100000
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 100000 & pan.adult_stats$avg.mass > 10000]) #31 #4.1% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 100000 & pan.adult_stats$avg.mass > 10000]) #7 (22.6%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 100000 & pan.adult_stats$avg.mass > 10000]) #24 (77.4%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 100000  & pan.adult_stats$avg.mass > 10000]) #23 (95.8%)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 100000  & pan.adult_stats$avg.mass > 10000]) #1 (4.2%)

##FIGURE: mass difference----
pan.adult_stats.trim <- pan.adult_stats[pan.adult_stats$mass.diff.se < 500,]
p <- ggplot(data = pan.adult_stats.trim, aes(x = mass.diff.se)) +
  geom_density(col = "slateblue4") +
  geom_rug(sides = "b", col = "slateblue4") +
  ggtitle("Difference in Mean Mass (PanTHERIA) to Mean Mass (this paper) over Standard Error") + 
  scale_x_continuous(name = 'Standard Deviations from Mean') + 
  scale_y_continuous(name = 'Probability') + 
  geom_vline(xintercept = c(-3, 3), linetype = "dashed", col = "darkgray") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(p, file=paste0("diff.mass", ".png"), width = 14, height = 10, units = "cm")

##central tendencies: median of FuTRES data to mean of PanTHERIA as a two sample t-test; non-parametric----

##Q2: estimating mass----
##Odocoileus virginianus

#all of Kitty's mass values are ok
deer <- data[data$scientificName == "Odocoileus virginianus" &
             data$lifeStage != "juvenile" &
             !is.na(data$measurementValue) &
             !(data$measurementStatus %in% outlier),]
nrow(deer) #3766

#combine deer by catalogNumber
deer.mass <- deer[deer$measurementType == "body mass",] #925 records
deer.astragalus.length <- deer[deer$measurementType == "talus lateral length",] #30 #talus lateral length is GLl in VDD
colnames(deer.mass)[colnames(deer.mass) == "measurementValue"] <- "mass"
colnames(deer.astragalus.length)[colnames(deer.astragalus.length) == "measurementValue"] <- "astragalus.length"
deer.combo <- merge(deer.mass, deer.astragalus.length, by = "individualID", all.x = TRUE, all.y = TRUE)
deer.combo$mass <- as.numeric(deer.combo$mass)
deer.combo$astragalus.length <- as.numeric(deer.combo$astragalus.length)

normal.odvi <- shapiro.test(deer.combo$mass[!is.na(deer.combo$mass)]) #if sig then not normally distributed

#test for normality

##write out data for example----
write.csv(deer.combo, "deer.csv")

## add paleo deer data----
#Not creating a long version because this is not yet accessioned into FuTRES
#for now, it is a separate dataset
old.deer.full <- read.csv("https://de.cyverse.org/dl/d/2BB5A147-B39A-4A06-A316-2D645F22613D/NEW_ArchaeoDeerAstragalusCalcaneus.csv", header = TRUE)
old.deer <- subset(old.deer.full, subset = old.deer.full$Site.Name == "Saint Catherines" | old.deer.full$Site.Name == "Fort Center")

x.species <- as.numeric(deer.combo$astragalus.length)
y.species <- as.numeric(deer.combo$mass)
#order should be the same
log.deer.mass.astragalus <- lm(log10(y.species) ~ log10(x.species), na.action = na.exclude)
#log.deer.mass.astragalus <- glm(log10(y.species) ~ log10(x.species), na.action = na.exclude)
log.sum.deer.mass.astragalus <- summary(log.deer.mass.astragalus)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)        1.4504     0.9676   1.499  0.14641   
# log10(x.species)   2.0382     0.6359   3.205  0.00367 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07776 on 25 degrees of freedom
# (901 observations deleted due to missingness)
# Multiple R-squared:  0.2912,	Adjusted R-squared:  0.2629 
# F-statistic: 10.27 on 1 and 25 DF,  p-value: 0.003671

anklepredict <- data.frame(specimenID = old.deer$SpecimenID, old.deer$Acc..., x.species = old.deer$astragalus.length)
predict.mass <- data.frame(predict(log.deer.mass.astragalus, anklepredict, se.fit = TRUE))
anklepredict$log.fit.mass <- predict.mass$fit
anklepredict$log.se.fit.mass <- predict.mass$se.fit
anklepredict$site <- as.factor(old.deer$Site.Name)

anklepredict$se.fit.mass <- 10^anklepredict$log.se.fit.mass
anklepredict$fit.mass <- 10^anklepredict$log.fit.mass

anklepredict$lower <- anklepredict$fit.mass - 2*anklepredict$se.fit.mass
anklepredict$upper <- anklepredict$fit.mass + 2*anklepredict$se.fit.mass

anklepredict$log.lower <- anklepredict$log.fit.mass - anklepredict$log.se.fit.mass
anklepredict$log.upper <- anklepredict$log.fit.mass + anklepredict$log.se.fit.mass

cc_Site = c("lightslateblue", "darkslateblue")

p <- ggplot() +
  geom_smooth(data = deer.combo, 
              aes(x = log10(astragalus.length), y = log10(mass)), method = "lm", color = "black", fill = "gray74")+
  geom_point(data = deer.combo, aes(x = log10(astragalus.length), y = log10(mass))) +
  geom_point(data = anklepredict, aes(x = log10(x.species), y = log.fit.mass, color = site), 
             shape = 18, size = 3) +#, color = "gray36") +
  scale_color_manual(values = cc_Site) + 
  geom_errorbar(data = anklepredict, aes(x = log10(x.species), 
                                         ymin = log.fit.mass - log.se.fit.mass, 
                                         ymax = log.fit.mass + log.se.fit.mass, color = site)) +
  #scale_color_manual(name = "siteName", values = cc_Site, labels = c("Fort Center, 200-800 ACE", "Saint Catherines Island, 1565-1763 ACE")) + 
  #scale_fill_discrete(name = "siteName", labels = c("Fort Center, 200-800 ACE", "Saint Catherines Island, 1565-1763 ACE", "Florida, modern" = "black")) + 
  ggtitle("Odocoileus virginianus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_y_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_x_continuous(name = expression(log[10]~Astragalus~Length~(mm)))
ggsave(p, file=paste0("plot_log_Odocoileus virginianus_astragalus_mass_predict.png"), width = 14, height = 10, units = "cm")

log.deer.mass.astragalus.stats <- data.frame(scientificName = "Odocoileus virginianus",
                                        comparison = ("mass/astragalus.length"),
                                        intercept = log.deer.mass.astragalus$coefficients[[1]],
                                        slope = log.deer.mass.astragalus$coefficients[[2]],
                                        resid.std.err = log.sum.deer.mass.astragalus$sigma,
                                        df = max(log.sum.deer.mass.astragalus$df),
                                        std.err.slope =  log.sum.deer.mass.astragalus$coefficients[4],
                                        std.err.intercept = log.sum.deer.mass.astragalus$coefficients[3],
                                        r.squared = log.sum.deer.mass.astragalus$r.squared,
                                        p.value = log.sum.deer.mass.astragalus$coefficients[,4][[2]],
                                        FLmodern.sample.size = nrow(deer.combo[!is.na(deer.combo$astragalus.length) & !is.na(deer.combo$mass),]),
                                        FortCenter.sample.size = nrow(anklepredict[!is.na(anklepredict$x.species) & anklepredict$site == "Fort Center",]),
                                        StCatherines.sample.size = nrow(anklepredict[!is.na(anklepredict$x.species) & anklepredict$site == "Saint Catherines",]))
write.csv(log.deer.mass.astragalus.stats, "log.deer.mass.astragalus.stats.csv")
#write.csv(log.deer.mass.astragalus.stats.glm, "log.deer.mass.astragalus.stats.glm.csv")

write.csv(anklepredict, "anklepredict.csv")
#write.csv(anklepredict, "anklepredict.glm.csv")

#predict using old method: logy = -6.71 + (5.29)logx
compare <- subset(anklepredict, select = c("specimenID", "site", "x.species", "fit.mass", "lower", "upper", "se.fit.mass"))
colnames(compare)[colnames(compare) == "x.species"] <- "astragalus.length"
compare$wing.mass <- 10^((-6.71)+(5.29)*log10(compare$astragalus.length)) * 1000
compare$within2se <- compare$wing.mass > compare$lower & compare$wing.mass < compare$upper

write.csv(compare, "old.deer.mass.comparison.csv")
#write.csv(compare, "old.deer.mass.comparison.glm.csv")


