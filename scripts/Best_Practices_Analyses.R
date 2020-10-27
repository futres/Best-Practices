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

##Upload data----
pan <- read.csv("https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("https://de.cyverse.org/dl/d/1B85CDF3-4852-459E-B8AE-98611D8F0EBA/data.for.analyses.csv", header = TRUE)
#data <- data.outlier #final dataset from Best_Practices_Data_Clean.R
data <- data[data$mass.status == "GOOD" & data$mass.units == "g" & !is.na(data$mass),]
length(unique(data$scientificName)) #869

###head-body-length----
data$head.body.length <- data$total.length - data$tail.length

#bunnies and deer are total length
#pan.data$head.body.length[pan.data$MSW05_Family == "Cervidae"] <- pan.data$total.length[pan.data$MSW05_Family == "Cervidae"]
#pan.data$head.body.length[pan.data$MSW05_Family == "Leporidae"] <- pan.data$total.length[pan.data$MSW05_Family == "Leporidae"]
#says NA not allowed for subscripts and can't get it working...

for(i in 1:nrow(data)){
  if(is.na(data$head.body.length[data$MSW05_Family == "Cervidae"][i])){
    data$head.body.length[i] <- data$total.length[i]
  }
  else if(isTRUE(data$total.length[data$MSW05_Family == "Cervidae"][i] >= 0)){
    data$head.body.length[i] <- data$total.length[i]
  }
  else{
    next
  }
}

for(i in 1:nrow(data)){
  if(is.na(data$head.body.length[data$MSW05_Family == "Leporidae"][i])){
    data$head.body.length[i] <- data$total.length[i]
  }
  else if(isTRUE(data$total.length[data$MSW05_Family == "Leporidae"][i] >= 0)){
    data$head.body.length[i] <- data$total.length[i]
  }
  else{
    next
  }
}

##combine with pantheria----
sp.data <- unique(data$scientificName) 
nrow(pan[pan$MSW05_Binomial %in% sp.data,]) #883 spp

pan.data <- merge(pan, data, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = TRUE, all.x = FALSE)
length(unique(pan.data$MSW05_Binomial)) #1020

write.csv(pan.data, "pan.data.csv")
write.csv(pan.data, "data.for.analyses.csv")

##data cleaned for mass----

pan.data.mass.sample <- pan.data %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size.mass.pan = length(mass[!is.na(mass)])) %>%
  as.data.frame()

pan.keep <- pan.data.mass.sample$MSW05_Binomial[pan.data.mass.sample$sample.size.mass.pan >= 10]
pan.data.10 <- pan.data[pan.data$MSW05_Binomial %in% pan.keep,]
pan.data.mass <- pan.data.10[!is.na(pan.data.10$X5.1_AdultBodyMass_g) & pan.data.10$X5.1_AdultBodyMass_g > 0 & pan.data.10$mass > 0,]

##Q1 compare to pantheria----
length(unique(pan.data.mass$MSW05_Binomial)) #690

pan.adult_stats <- pan.data.mass %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size = length(mass), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.err.mass = sd(mass, na.rm = TRUE)/sqrt(sample.size),
                   pan.mass = X5.1_AdultBodyMass_g[1],
                   mass.diff = (pan.mass - avg.mass) / sd.err.mass,
                   abs.mass.diff = abs((pan.mass - avg.mass) / sd.err.mass)) %>%
  as.data.frame()

##write mass difference results----
write.csv(pan.adult_stats, "pan.results.csv")

plot(x = log10(pan.adult_stats$avg.mass), y = log10(pan.adult_stats$sd.err.mass),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Standard Error of Mass",
     title(main = "Relationship between standard error and average mass",
           sub = "slope = 1.16, p<0.001"))
model <- lm(log10(pan.adult_stats$sd.err.mass) ~ log10(pan.adult_stats$avg.mass))
summary(model)

plot(x = pan.adult_stats$sample.size, y = pan.adult_stats$abs.mass.diff,
     xlab = "Sample Size",
     ylab = "Absolute Difference between Pan Mass and Avg. Mass",
     title(main = "Relationship between Sample Size on Mass differences",
           sub = "slope = 0, p=0.9"))
model3 <- lm(pan.adult_stats$abs.mass.diff ~ pan.adult_stats$sample.size + pan.adult_stats$avg.mass)
summary(model3)
model4 <- lm(pan.adult_stats$abs.mass.diff ~ pan.adult_stats$sample.size + pan.adult_stats$sd.err.mass)
summary(model4)

plot(x = log10(pan.adult_stats$avg.mass), y = log10(pan.adult_stats$mass.diff),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Mass Difference",
     title(main = "Relationship between animal size and degree of difference between masses",
           sub = "slope = 0.003, p=0.925"))
model2 <- lm(log10(pan.adult_stats$mass.diff) ~ log10(pan.adult_stats$avg.mass))
summary(model2)

##Table 1----
#some major outliers, let's ignore those for now
pan.adult_stats2 <- pan.adult_stats[pan.adult_stats$MSW05_Binomial != "Stenella longirostris" &
                                      pan.adult_stats$MSW05_Binomial != "Catagonus wagneri" &
                                      pan.adult_stats$MSW05_Binomial != "Alces alces" & 
                                      pan.adult_stats$MSW05_Binomial != "Hylobates lar" &
                                      pan.adult_stats$MSW05_Binomial != "Arctocephalus townsendi" &
                                      pan.adult_stats$MSW05_Binomial != "Lepus arcticus" &
                                      pan.adult_stats$MSW05_Binomial != "Enhydra lutris",] 

nrow(pan.adult_stats2) #683
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3]) #182 26.6%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3]) #501; 73.4%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3]) #420 61.5%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3]) #81; 11.9%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 100]) #504 (73.8% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 100]) #130; 25.8%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 100]) #374; 74.2%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 100]) #311; 61.7%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 100]) #63; 12.5%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 1000 & pan.adult_stats2$avg.mass > 100]) #108 (15.8% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #40; 37.0%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #68; 63.0%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #60; 55.6%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #8; 7.4%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) #34 (5.0% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) #6; 17.6%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) #28; 82.4%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 10000  & pan.adult_stats2$avg.mass > 1000]) #27; 79.4%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 10000  & pan.adult_stats2$avg.mass > 1000]) #1; 2.9%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) #19 (2.8% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) #1; 5.3%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) #18; 94.7%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 100000  & pan.adult_stats2$avg.mass > 10000]) #17; 89.5%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 100000  & pan.adult_stats2$avg.mass > 10000]) #1; 5.3%

##FIGURE: mass difference----
p <- ggplot(data = pan.adult_stats2, aes(x = mass.diff)) +
  geom_density(col = "slateblue4") +
  geom_rug(sides = "b", col = "slateblue4") +
  ggtitle("Difference in Mean Mass (PanTHERIA) to Mean Mass (this paper)") + 
  scale_x_continuous(name = 'Standard Deviations from Mean') + 
  scale_y_continuous(name = 'Probability') + 
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", col = "darkgray") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(p, file=paste0("diff.mass", ".png"), width = 14, height = 10, units = "cm")

##FIGURE: body mass distributions w/ line from PanTHERIA----

pan.data.stats <- pan.data.mass[pan.data.mass$MSW05_Binomial %in% pan.adult_stats2$MSW05_Binomial,] #222

uniq_species <- unique(pan.data.stats$MSW05_Binomial)
for (i in uniq_species) {
  p = ggplot(data = subset(pan.adult.clean.10, MSW05_Binomial == i)) + 
    geom_density(aes(log10(mass)), fill = "slateblue4") +
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_continuous(name = 'Probability') + 
    geom_vline(xintercept = log10(pan.adult.clean.10$X5.1_AdultBodyMass_g[pan.adult.clean.10$MSW05_Binomial == i][1]))
  ggsave(p, file=paste0("dist_", i,".png"), width = 14, height = 10, units = "cm")
}

##Q2: length v. mass----
#clean data
pan.data.mass.sample <- pan.data %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size.mass.pan = length(mass[!is.na(mass)])) %>%
  as.data.frame()

pan.keep <- pan.data.mass.sample$MSW05_Binomial[pan.data.mass.sample$sample.size.mass.pan >= 10]
pan.data.10 <- pan.data[pan.data$MSW05_Binomial %in% pan.keep,]
pan.data.mass <- pan.data.10[!is.na(pan.data.10$X5.1_AdultBodyMass_g) & pan.data.10$X5.1_AdultBodyMass_g > 0 & pan.data.10$mass > 0,]

pan.data.mass <- pan.data[!is.na(pan.data$mass) & pan.data$mass.status != "outlier",]
pan.data.mass.length <- pan.data.mass[!is.na(pan.data.mass$total.length) & pan.data.mass$total.length.status != "outlier",]
length(unique(pan.data.adult$MSW05_Binomial)) #998 sp

##some measurements are zero, oy vey
pan.data.adult.clean <- pan.data.adult[pan.data.adult$mass > 0 & pan.data.adult$head.body.length > 0,]
length(unique(pan.data.adult.clean$MSW05_Binomial)) #998

#recount sample sizes
pan.data.adult_stats <- pan.data.adult.clean %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(counts = n())

keep.10 <- pan.data.adult_stats$MSW05_Binomial[pan.data.adult_stats$counts >= 10] #849 sp
pan.data.adult.10 <- pan.data.adult.clean[pan.data.adult.clean$MSW05_Binomial %in% keep.10,]
length(unique(pan.data.adult.10$MSW05_Binomial)) #849 sp

sp.models <- unique(pan.data.adult.10$MSW05_Binomial)
model.results.species <- data.frame()
for(i in 1:length(sp.models)){
  sub.data <- as.data.frame(pan.data.adult.10[pan.data.adult.10$MSW05_Binomial == sp.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$head.body.length), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = sub.data$MSW05_Binomial[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(sub.data$mass))
  model.results.species <- rbind(model.results.species, sub)
}

#lots of repeats for Neofiber alleni; only 4 unique occurrence ids....why???

##model resutls----
write.csv(model.results.species, "model.results.species.csv")

##figures of mass v length----
uniq_species <- unique(pan.data.adult.10$MSW05_Binomial)
for (i in uniq_species) {
  p = ggplot(data = subset(pan.data.adult.10, MSW05_Binomial  == i)) + 
    geom_point(aes(x = log10(mass), y = log10(head.body.length))) +
    geom_smooth(aes(x = log10(mass), y = log10(head.body.length)),
                method = "lm", color = "slateblue4")
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_log10(name = expression(log[10]~Head~Body~Length~(mm))) + 
  ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

##Q3: transfer function----
#code for regressions of limb data vs bodymass
#Odocoileus virginianus: hind foot, mass, ankle measurement
#Spermophilus beecheyi: mass, toothrow


##Spermophilus beecheyi----

Sbeecheyi <- data[data$scientificName == "Spermophilus beecheyi",]

Sbeecheyi.clean <- Sbeecheyi[Sbeecheyi$mass.status == "GOOD" & Sbeecheyi$total.length.status == "GOOD" & Sbeecheyi$mass.units == "g" & Sbeecheyi$total.length.units == "mm",
                             select = c("scientificName","mass", "head.body.length", "tooth.row", "hindfoot.length")]
Sbeecheyi <- Sbeecheyi.cleaner

#mass vs toothrow length
model1 <- lm(log10(Sbeecheyi$mass) ~ log10(Sbeecheyi$tooth.row), na.action=na.exclude)
sum.model1 <- summary(model1)
sub1 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Mass/toothrow"),
                   intercept = model1$coefficients[[1]],
                   slope = model1$coefficients[[2]],
                   resid.std.err = sum.model1$sigma,
                   df = max(sum.model1$df),
                   std.err.slope =  sum.model1$coefficients[4],
                   std.err.intercept = sum.model1$coefficients[3],
                   r.squared = sum.model1$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$tooth.row),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(mass))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(mass)),
              method = "lm", color = "slateblue4") +
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Toothrow~Length~(mm)))
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow.png"), width = 14, height = 10, units = "cm")

#mass vs hindfoot
model2 <- lm(log10(Sbeecheyi$mass) ~ log10(Sbeecheyi$hindfoot.length), na.action=na.exclude)
sum.model2 <- summary(model2)
sub2 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Mass/hinfoot"),
                   intercept = model2$coefficients[[1]],
                   slope = model2$coefficients[[2]],
                   resid.std.err = sum.model2$sigma,
                   df = max(sum.model2$df),
                   std.err.slope =  sum.model2$coefficients[4],
                   std.err.intercept = sum.model2$coefficients[3],
                   r.squared = sum.model2$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$hindfoot.length) & !is.na(Sbeecheyi$mass),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(hindfoot.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(hindfoot.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Hindfoot~Length~(mm))) 
ggsave(p, file=paste0("plot_Spermophilus beecheyi_hingfoot.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(sub1, sub2)  

#tooth row vs hindfoot  

model3 <- lm(log10(Sbeecheyi$hindfoot.length) ~ log10(Sbeecheyi$tooth.row), na.action=na.exclude)
sum.model3 <- summary(model3)
sub3 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("hindfoot length/toothrow length"),
                   intercept = model3$coefficients[[1]],
                   slope = model3$coefficients[[2]],
                   resid.std.err = sum.model3$sigma,
                   df = max(sum.model3$df),
                   std.err.slope =  sum.model3$coefficients[4],
                   std.err.intercept = sum.model3$coefficients[3],
                   r.squared = sum.model3$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$hindfoot.length) & !is.na(Sbeecheyi$tooth.row),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(hindfoot.length))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(hindfoot.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Toothrow~Length~(mm))) +
  ylab(expression(log[10]~Hindfoot~Length~(mm))) 
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_hingfoot.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, sub3)  

#mass versus total length
model4 <- lm(log10(Sbeecheyi$mass) ~ log10(Sbeecheyi$head.body.length), na.action=na.exclude)
sum.model4 <- summary(model4)
sub4 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Mass/Total length"),
                   intercept = model4$coefficients[[1]],
                   slope = model4$coefficients[[2]],
                   resid.std.err = sum.model4$sigma,
                   df = max(sum.model4$df),
                   std.err.slope =  sum.model4$coefficients[4],
                   std.err.intercept = sum.model4$coefficients[3],
                   r.squared = sum.model4$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$total.length),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(head.body.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(head.body.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Total~Length~(mm)))

ggsave(p, file=paste0("plot_Spermophilus beecheyi_totallength.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, sub4)  

#toothrow versus total length
model5 <- lm(log10(Sbeecheyi$head.body.length) ~ log10(Sbeecheyi$tooth.row), na.action=na.exclude)
sum.model5 <- summary(model5)
sub5 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Total length/toothrow length"),
                   intercept = model5$coefficients[[1]],
                   slope = model5$coefficients[[2]],
                   resid.std.err = sum.model5$sigma,
                   df = max(sum.model5$df),
                   std.err.slope =  sum.model5$coefficients[4],
                   std.err.intercept = sum.model5$coefficients[3],
                   r.squared = sum.model5$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$total.length) & !is.na(Sbeecheyi$tooth.row),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(head.body.length))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(head.body.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Toothrow~Length~(mm))) +
  ylab(expression(log[10]~Total~Length~(mm))) 
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_totallength.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, sub5)  
#write.csv(model.Spermophilus.beecheyi, file= "model.results.Spermophilus.beecheyi.csv")

#Strangely Futres does not have the hindfoot data for Odocoileus virginianus so for now I am going to pull it from a different dataset?

## Odocoileus virginianus----
## add paleo deer data----
old.deer <- read.csv(ArchaeoDeerAstragalusCalcaneus.csv, header = TRUE)

Ovirginianus <- pan.data[pan.data$MSW05_Binomial == "Odocoileus virginianus",]

Ovirginianus.clean <- Ovirginianus[Ovirginianus$lifeStage != "Juvenile" & Ovirginianus$mass.status != "outlier" & Ovirginianus$total.length.status != "outlier",]
Ovirginianus <- Ovirginianus.clean

#mass vs hindfoot length
model1 <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$hindfoot.length), na.action=na.exclude)
sum.model1 <- summary(model1)
sub1 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("Mass/hindfoot"),
                   intercept = model$coefficients[[1]],
                  slope = model$coefficients[[2]],
                  resid.std.err = sum.model$sigma,
                  df = max(sum.model$df),
                  std.err.slope =  sum.model$coefficients[4],
                  std.err.intercept = sum.model$coefficients[3],
                  r.squared = sum.model$r.squared,
                  sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$hindfoot.length),]))

#ggtitle doesn't seem to be doing anything because it is not connected to p. When I do connect it scale_x and scale y takes away the axis tick marks. 
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(mass), y = log10(hindfoot.length))) +
  geom_smooth(aes(x = log10(mass), y = log10(hindfoot.length)),
              method = "lm", color = "slateblue4") +
ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Body~Mass~(g))) +
  ylab(expression(log[10]~Hindfoot~Length~(mm))) + 
  ggsave(p, file=paste0("plot_Odocoileus virginianus_mass_hindfoot.png"), width = 14, height = 10, units = "cm")

#mass vs astragalus
model2 <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$astragalus.length), na.action=na.exclude)
sum.model2 <- summary(model2)
sub2 <- data.frame(binomial = Ovirginianusi$MSW05_Binomial[1],
                   comparison = ("Mass/astragalus"),
                   intercept = model2$coefficients[[1]],
                   slope = model2$coefficients[[2]],
                   resid.std.err = sum.model2$sigma,
                   df = max(sum.model2$df),
                   std.err.slope =  sum.model2$coefficients[4],
                   std.err.intercept = sum.model2$coefficients[3],
                   r.squared = sum.model2$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$mass),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Astragalus~Length~(mm))) 
ggsave(p, file=paste0("plot_Odocoileus virginianus_mass_astragalus.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(sub1, sub2)  

#astragalus vs hindfoot  

model3 <- lm(log10(Ovirginianus$hindfoot.length) ~ log10(Ovirginianus$astragalus.length), na.action=na.exclude)
sum.model3 <- summary(model3)
sub3 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("hindfoot length/astraglus length"),
                   intercept = model3$coefficients[[1]],
                   slope = model3$coefficients[[2]],
                   resid.std.err = sum.model3$sigma,
                   df = max(sum.model3$df),
                   std.err.slope =  sum.model3$coefficients[4],
                   std.err.intercept = sum.model3$coefficients[3],
                   r.squared = sum.model3$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$hindfoot.length),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(hindfoot.length))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(hindfoot.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Astragalus~Length~(mm))) +
  ylab(expression(log[10]~Hindfoot~Length~(mm))) 
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_hingfoot.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, sub3)  

#mass versus total length
model4 <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$total.length), na.action=na.exclude)
sum.model4 <- summary(model4)
sub4 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("Mass/Total length"),
                   intercept = model4$coefficients[[1]],
                   slope = model4$coefficients[[2]],
                   resid.std.err = sum.model4$sigma,
                   df = max(sum.model4$df),
                   std.err.slope =  sum.model4$coefficients[4],
                   std.err.intercept = sum.model4$coefficients[3],
                   r.squared = sum.model4$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$total.length),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(total.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(total.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Total~Length~(mm)))

ggsave(p, file=paste0("plot_Odocoileus virginianus_mass_totallength.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, sub4)  

#astragalus versus total length
model5 <- lm(log10(Ovirginianus$total.length) ~ log10(Ovirginianus$astragalus.length), na.action=na.exclude)
sum.model5 <- summary(model5)
sub5 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("Total length/astragalus length"),
                   intercept = model5$coefficients[[1]],
                   slope = model5$coefficients[[2]],
                   resid.std.err = sum.model5$sigma,
                   df = max(sum.model5$df),
                   std.err.slope =  sum.model5$coefficients[4],
                   std.err.intercept = sum.model5$coefficients[3],
                   r.squared = sum.model5$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$total.length) & !is.na(Ovirginianus$astragalus.length),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(total.length))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(head.body.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Astragalus~Length~(mm))) +
  ylab(expression(log[10]~Total~Length~(mm))) 
ggsave(p, file=paste0("plot_Odocoileus virginanus_astragalus_totallength.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, sub5)  
#write.csv(model.Spermophilus.beecheyi, file= "model.results.Spermophilus.beecheyi.csv")



#the below code is for plotting many measurments.
#can switch out species name for whichever is the target
#test <- subset(data.limb, scientificName== "Aepyceros melampus", select = c("X","occurrenceID", "scientificName", "lifeStage", "sex", "catalogNumber", "mass", "astragalus.length", "astragalus.width", "calcaneus.GB", "calcaneus.GL", "femur.length", "humerus.length", "forearm.length", "tooth.row"))

test_reshape <- gather(data = test, 
                       key = Measurement, 
                       value = value, 
                       8:15)

sp.models.lim <- unique(test_reshape$Measurement)
model.results.species.limb <- data.frame()
for(i in 1:length(sp.models.lim)){
  sub.data <- as.data.frame(test_reshape[test_reshape$Measurement == sp.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$value), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = sub.data$scientificName[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(sub.data$mass))
  model.results.species.limb <- rbind(sub, model.results.species.limb)
}

#plotting, I need to still fix this
for (i in sp.models.lim) {
  p = ggplot(data = subset(test_reshape, Measurement  == i)) + 
    geom_point(aes(x = log10(mass), y = log10(value))) +
    geom_smooth(aes(x = log10(mass), y = log10(value)),
                method = "lm", color = "slateblue4")
  ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_log10(name = expression(log[10]~Total~Length~(mm))) + 
    ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}









