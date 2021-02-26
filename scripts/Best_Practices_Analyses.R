# Best Practices Data Clean Up
# Meghan A. Balk
# balkm@email.arizona.edu

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
data <- read.csv("https://de.cyverse.org/dl/d/7B6941EE-1FED-4AAE-9D8A-8AC76DB0AC98/data.all.csv", header = TRUE, stringsAsFactors = FALSE)
length(unique(data$scientificName)) #3974

data.clean <- data[data$measurementStatus != "outlier" & 
                     data$lifeStage != "Juvenile",]
nrow(data.clean) #2094245
length(unique(data.clean$scientificName)) #3,958 

##combine with pantheria----
sp.data <- unique(data$scientificName) 
nrow(pan[pan$MSW05_Binomial %in% sp.data,]) #2672 spp shared

taxonomy.data <- merge(pan, data, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = TRUE, all.x = FALSE)

##write taxonomy----
write.csv(taxonomy.data, "taxonomy.data.csv")

##data for pan analyses
pan.data <- taxonomy.data[taxonomy.data$measurementType == "mass" &
                            taxonomy.data$lifeStage != "Juvenile" &
                            taxonomy.data$measurementStatus == "possibly good" &
                            !is.na(taxonomy.data$measurementValue) &
                            !is.na(taxonomy.data$X5.1_AdultBodyMass_g),]

length(unique(pan.data$MSW05_Binomial)) #110

##write data for pan analyses----
write.csv(pan.data, "pan.data.csv")

##Q1 compare to pantheria----
stats <- pan.data %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(N = n()) %>%
  as.data.frame()
keep <- stats$MSW05_Binomial[stats$N >= 10]
pan.data <- pan.data[pan.data$MSW05_Binomial %in% keep,]
length(unique(pan.data$MSW05_Binomial)) #108
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
plot(x = pan.adult_stats$sample.size, y = pan.adult_stats$mass.diff)
model.N <- lm(pan.adult_stats$mass.diff ~ pan.adult_stats$sample.size)
summary(model.N)

plot(x = pan.adult_stats$avg.mass, y = pan.adult_stats$mass.diff)
model.mass <- lm(pan.adult_stats$mass.diff ~ pan.adult_stats$avg.mass)
summary(model.mass)

##Table 1----

#total
nrow(pan.adult_stats) #108
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE]) #28  #w/in limits
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE])  #80 #outside limits
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0])  #54 #above; pan mean is greater than ours
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0])  #26 #below; pan mean is less than ours

#<100
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 100]) #75 #(% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 100]) #15
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 100]) #60
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 100]) #40
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 100]) #20

#100-1000
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 1000 & pan.adult_stats$avg.mass > 100]) #14 #(% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #7
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #7
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #4
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 1000  & pan.adult_stats$avg.mass > 100]) #3

#1000-10000
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 10000 & pan.adult_stats$avg.mass > 1000]) #13 #(% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 10000 & pan.adult_stats$avg.mass > 1000]) #4
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 10000 & pan.adult_stats$avg.mass > 1000]) #9
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 10000  & pan.adult_stats$avg.mass > 1000]) #7
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 10000  & pan.adult_stats$avg.mass > 1000]) #2

#10000-100000
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$avg.mass < 100000 & pan.adult_stats$avg.mass > 10000]) #4 #(% of total spp)
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == FALSE & pan.adult_stats$avg.mass < 100000 & pan.adult_stats$avg.mass > 10000]) #1
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$avg.mass < 100000 & pan.adult_stats$avg.mass > 10000]) #3
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff > 0 & pan.adult_stats$avg.mass < 100000  & pan.adult_stats$avg.mass > 10000]) #2
length(pan.adult_stats$MSW05_Binomial[pan.adult_stats$sig.corr == TRUE & pan.adult_stats$mass.diff < 0 & pan.adult_stats$avg.mass < 100000  & pan.adult_stats$avg.mass > 10000]) #1

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

##Q2: estimating mass----
##Odocoileus virginianus

deer <- data[data$scientificName == "Odocoileus virginianus" &
                     data$lifeStage != "Juvenile" &
                     !is.na(data$measurementValue),]

#combine deer by catalogNumber
deer.mass <- deer[deer$measurementType == "mass",]
deer.astragalus.length <- deer[deer$measurementType == "astragalus.length",]
colnames(deer.mass)[colnames(deer.mass) == "measurementValue"] <- "mass"
colnames(deer.astragalus.length)[colnames(deer.astragalus.length) == "measurementValue"] <- "astragalus.length"
deer.combo <- merge(deer.mass, deer.astragalus.length, by = "catalogNumber", all.x = FALSE, all.y = TRUE)
deer.combo$mass <- as.numeric(deer.combo$mass)
deer.combo$astragalus.length <- as.numeric(deer.combo$astragalus.length)

##write out data for example----
write.csv(deer.combo, "deer.csv")

## add paleo deer data----
#Not creating a long version because this is not yet accessioned into FuTRES
#for now, it is a separate dataset
old.deer.full <- read.csv("https://de.cyverse.org/dl/d/2BB5A147-B39A-4A06-A316-2D645F22613D/NEW_ArchaeoDeerAstragalusCalcaneus.csv", header = TRUE)
old.deer <- subset(old.deer.full, subset = old.deer.full$Site.Name == "Saint Catherines" | old.deer.full$Site.Name == "Fort Center")

x.species <- deer.combo$astragalus.length
y.species <- deer.combo$mass
#order should be the same
log.deer.mass.astragalus <- lm(log10(y.species) ~ log10(x.species), na.action = na.exclude)
log.sum.deer.mass.astragalus <- summary(log.deer.mass.astragalus)

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
write.csv(anklepredict, "anklepredict.csv")

#predict using old method: logy = -6.71 + (5.29)logx
compare <- subset(anklepredict, select = c("specimenID", "site", "x.species", "fit.mass", "lower", "upper", "se.fit.mass"))
colnames(compare)[colnames(compare) == "x.species"] <- "astragalus.length"
compare$wing.mass <- 10^((-6.71)+(5.29)*log10(compare$astragalus.length)) * 1000
compare$within2se <- compare$wing.mass > compare$lower & compare$wing.mass < compare$upper

write.csv(compare, "old.deer.mass.comparison.csv")
