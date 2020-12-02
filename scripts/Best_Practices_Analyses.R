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
length(unique(pan.data.mass$MSW05_Binomial)) #1020
pan.data.mass <- pan.data.mass[!is.na(pan.data.mass$X5.1_AdultBodyMass_g),]
length(unique(pan.data.mass$MSW05_Binomial)) #781

stats <- pan.data.mass %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(N = n()) %>%
  as.data.frame()
keep <- stats$MSW05_Binomial[stats$N >= 10]
pan.data.mass <- pan.data.mass[pan.data.mass$MSW05_Binomial %in% keep,]
length(unique(pan.data.mass$MSW05_Binomial)) #690

pan.adult_stats <- pan.data.mass %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size = length(mass), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.err.mass = sd(mass, na.rm = TRUE)/sqrt(sample.size),
                   pan.mass = X5.1_AdultBodyMass_g[1],
                   mass.diff = (pan.mass - avg.mass),
                   mass.diff.se = mass.diff / sd.err.mass,
                   abs.mass.diff.se = abs(mass.diff.se), #observed t; number in t-units (counts of standard errors from each mean)
                   outside.3.sigma = abs.mass.diff.se > 3, #if true then greater than 3 std errors outside
                   critical.t = abs(qt(p = 0.025, df = (sample.size-1))),
                   diff.amt = abs.mass.diff.se-critical.t,
                   sig = diff.amt > 0, # true means sig diff
                   p.value.fromt.crit.t = 1-pt(abs.mass.diff.se, df = (sample.size-1)), #if p=0.05 then most of it is outside
                   p.value = t.test(mass, mu = pan.mass, conf.level = 0.95, alternative = "two.sided")$p.value) %>%
  as.data.frame()

pan.adult_stats$corr <- p.adjust(pan.adult_stats$p.value, method = "BH")
pan.adult_stats$corr.t.value <- p.adjust(pan.adult_stats$p.value.fromt.crit.t, method = "BH")
pan.adult_stats$sig.corr <- pan.adult_stats$corr <= 0.05 #TRUE means sig diff
pan.adult_stats$sig.corr.t.value <- pan.adult_stats$corr.t.value <= 0.05 #TRUE means sig diff

##write mass difference results----
write.csv(pan.adult_stats, "pan.results.csv")

plot(x = log10(pan.adult_stats$avg.mass), y = log10(pan.adult_stats$sd.err.mass),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Standard Error of Mass",
     title(main = "Relationship between standard error and average mass",
           sub = "slope = 1.16, p<0.001"))
model <- lm(log10(pan.adult_stats$sd.err.mass) ~ log10(pan.adult_stats$avg.mass))
summary(model)

plot(x = pan.adult_stats$sample.size, y = pan.adult_stats$abs.mass.diff.se,
     xlab = "Sample Size",
     ylab = "Absolute Difference between Pan Mass and Avg. Mass over Standard Error",
     title(main = "Relationship between Sample Size on Mass differences",
           sub = "slope = 0, p=0.9"))
model3 <- lm(pan.adult_stats$abs.mass.diff.se ~ pan.adult_stats$sample.size + pan.adult_stats$avg.mass)
summary(model3)
model4 <- lm(pan.adult_stats$abs.mass.diff.se ~ pan.adult_stats$sample.size + pan.adult_stats$sd.err.mass)
summary(model4)

plot(x = log10(pan.adult_stats$avg.mass), y = log10(pan.adult_stats$mass.diff.se),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Mass Difference",
     title(main = "Relationship between animal size and degree of difference between masses",
           sub = "slope = 0.003, p=.92"))
model2 <- lm(log10(pan.adult_stats$mass.diff.se) ~ log10(pan.adult_stats$avg.mass))
summary(model2)

##Table 1----

nrow(pan.adult_stats2) #690
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == FALSE])  #w/in limits
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == TRUE])  #outside limits
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff > 0])  #above; pan mean is greater than ours
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff < 0])  #below; pan mean is less than ours

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 100]) #(% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == FALSE & pan.adult_stats2$avg.mass < 100]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == TRUE & pan.adult_stats2$avg.mass < 100]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff > 0 & pan.adult_stats2$avg.mass < 100]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff < 0 & pan.adult_stats2$avg.mass < 100])

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 1000 & pan.adult_stats2$avg.mass > 100]) #(% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == FALSE & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == TRUE & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff > 0 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff < 0 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) 

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) #(% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == FALSE & pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000])
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == TRUE & pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff > 0 & pan.adult_stats2$avg.mass < 10000  & pan.adult_stats2$avg.mass > 1000]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff < 0 & pan.adult_stats2$avg.mass < 10000  & pan.adult_stats2$avg.mass > 1000]) 

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) #(% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == FALSE & pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig.corr == TRUE & pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff > 0 & pan.adult_stats2$avg.mass < 100000  & pan.adult_stats2$avg.mass > 10000]) 
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$sig == TRUE & pan.adult_stats2$mass.diff < 0 & pan.adult_stats2$avg.mass < 100000  & pan.adult_stats2$avg.mass > 10000]) 

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

##FIGURE: body mass distributions w/ line from PanTHERIA----

# pan.data.stats <- pan.data.mass[pan.data.mass$MSW05_Binomial %in% pan.adult_stats2$MSW05_Binomial,] #222
# 
# uniq_species <- unique(pan.data.stats$MSW05_Binomial)
# for (i in uniq_species) {
#   p = ggplot(data = subset(pan.adult.clean.10, MSW05_Binomial == i)) + 
#     geom_density(aes(log10(mass)), fill = "slateblue4") +
#     ggtitle(i) +
#     scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
#     scale_y_continuous(name = 'Probability') + 
#     geom_vline(xintercept = log10(pan.adult.clean.10$X5.1_AdultBodyMass_g[pan.adult.clean.10$MSW05_Binomial == i][1]))
#   ggsave(p, file=paste0("dist_", i,".png"), width = 14, height = 10, units = "cm")
# }

##Q2: length v. mass----
#clean data
pan.data.mass.length <- pan.data[pan.data$total.length.units == "mm" & !is.na(pan.data$total.length) &
                                   pan.data$mass.units == "g" & !is.na(pan.data$mass),]

pan.data.sample <- pan.data.mass.length %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size.mass.pan = length(mass),
                   sampe.size.length.pan = length(total.length)) %>%
  as.data.frame()

pan.keep <- pan.data.mass.sample$MSW05_Binomial[pan.data.sample$sample.size.mass.pan >= 10 & pan.data.sample$sampe.size.length.pan >= 10]
pan.data.10 <- pan.data.mass.length[pan.data.mass.length$MSW05_Binomial %in% pan.keep,]

##some measurements are zero, oy vey
pan.data.adult.clean <- pan.data.10[pan.data.10$mass > 0 & pan.data.10$head.body.length > 0,]
length(unique(pan.data.adult.clean$MSW05_Binomial)) #837

sp.models <- unique(pan.data.adult.clean$MSW05_Binomial)
model.results.species <- data.frame()
for(i in 1:length(sp.models)){
  sub.data <- as.data.frame(pan.data.adult.clean[pan.data.adult.clean$MSW05_Binomial == sp.models[i],])
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

write.csv(model.results.species, "model.results.species.csv")

##figures of mass v length----
# uniq_species <- unique(pan.data.adult.10$MSW05_Binomial)
# for (i in uniq_species) {
#   p = ggplot(data = subset(pan.data.adult.10, MSW05_Binomial  == i)) + 
#     geom_point(aes(x = log10(mass), y = log10(head.body.length))) +
#     geom_smooth(aes(x = log10(mass), y = log10(head.body.length)),
#                 method = "lm", color = "slateblue4")
#     ggtitle(i) +
#     scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
#     scale_y_log10(name = expression(log[10]~Head~Body~Length~(mm))) + 
#   ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
# }

##Q3: transfer function----
#code for regressions of limb data vs bodymass
#Odocoileus virginianus: hind foot, mass, ankle measurement
#Spermophilus beecheyi: mass, toothrow


##Spermophilus beecheyi----

##GENUS LEVEL

Spermophilus <- pan.data[pan.data$MSW05_Genus == "Spermophilus",]

Spermophilus.clean <- subset(Spermophilus, Spermophilus$mass.status == "GOOD" & Spermophilus$total.length.status == "GOOD" 
                          & Spermophilus$mass.units == "g" & Spermophilus$total.length.units == "mm"
                          & Spermophilus$mass > 0 & Spermophilus$total.length > 0,
                             select = c("MSW05_Binomial","mass", "head.body.length", "tooth.row", "hindfoot.length"))
Spermophilus <- Spermophilus.clean

#remove outlier
which.min(Spermophilus$mass)
Spermophilus <- Spermophilus[-483,]

write.csv(Spermophilus, "Spermophilus.csv")


#Code for plotting Spermophilus beecheyi

#Now we want to see if translating between the models is fruitful. The first step is to predict total length from tooth row length
#predict function works better if the naming scheme inside the lm model is simple. It will automatically log the values.
#toothrow versus total length
squirrel.hbl.tooth <- lm(Spermophilus$head.body.length[Spermophilus$MSW05_Binomial == "Spermophilus beecheyi"] ~ Spermophilus$tooth.row[Spermophilus$MSW05_Binomial == "Spermophilus beecheyi"], na.action = na.exclude)
sum.squirrel.hbl.tooth <- summary(squirrel.hbl.tooth)
toothpredict <- data.frame(x = Spermophilus$tooth.row[Spermophilus$MSW05_Binomial == "Spermophilus beecheyi"])
predict.hbl <- data.frame(predict(squirrel.hbl.tooth, toothpredict, se.fit = TRUE))
toothpredict$fit.hbl <- predict.hbl$fit
toothpredict$se.fit.hbl <- predict.hbl$se.fit

#plotting this relationship here
squirrel.hbl.tooth.stats <- data.frame(scientificName = "Spermophilus beecheyi",
                                       comparison = ("head.body.length/tooth.row"),
                                       intercept = squirrel.hbl.tooth$coefficients[[1]],
                                       slope = squirrel.hbl.tooth$coefficients[[2]],
                                       resid.std.err = sum.squirrel.hbl.tooth$sigma,
                                       df = max(sum.squirrel.hbl.tooth$df),
                                       std.err.slope =  sum.squirrel.hbl.tooth$coefficients[4],
                                       std.err.intercept = sum.squirrel.hbl.tooth$coefficients[3],
                                       r.squared = sum.squirrel.hbl.tooth$r.squared,
                                       p.value = sum.squirrel.hbl.tooth$coefficients[,4][[2]],
                                       sample.size = nrow(Spermophilus[!is.na(Spermophilus$tooth.row) & !is.na(Spermophilus$head.body.length & Spermophilus$MSW05_Binomial == "Spermophilus beecheyi"),]))

nrow(Spermophilus[!is.na(Spermophilus$tooth.row) & !is.na(Spermophilus$head.body.length) & Spermophilus$MSW05_Binomial == "Spermophilus beecheyi",])
p = ggplot(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi")) + 
  geom_point(aes(x = tooth.row, y = head.body.length)) +
  geom_smooth(aes(x = tooth.row, y = head.body.length), method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi, N = 83") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Head Body Length (mm)") +
  scale_x_continuous(name = "Toothrow Length (mm)")
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_hbl.png"), width = 14, height = 10, units = "cm")

#Now to push the toothrow points through the second regression. I am already in log 10 space with the predictions.
#need to sample the whole genus for this data
#some masses=0? below regression won't work if anything=0
y.genus <- Spermophilus$mass
x.genus <- Spermophilus$head.body.length
squirrel.hbl.mass <- lm(y.genus ~ x.genus, na.action = na.exclude)
#squirrel.hbl.mass <- lm(Spermophilus$mass ~ Spermophilus$head.body.length, na.action = na.exclude)
sum.squirrel.hbl.mass <- summary(squirrel.hbl.mass)

#remember the variable has to have the same name for the predict function to work so we have to name it x2
fit.tooth <- data.frame(x.genus = toothpredict$fit.hbl)
predict.mass <- data.frame(predict(squirrel.hbl.mass, newdata = fit.tooth, se.fit = TRUE))
toothpredict$fit.mass <- predict.mass$fit
toothpredict$se.fit.mass <- predict.mass$se.fit

#plotting this relationship
squirrel.hbl.mass.stats <- data.frame(scientificName = "Spermophilus",
                                      comparison = ("mass/head.body.length"),
                                      intercept = squirrel.hbl.mass$coefficients[[1]],
                                      slope = squirrel.hbl.mass$coefficients[[2]],
                                      resid.std.err = sum.squirrel.hbl.mass$sigma,
                                      df = max(sum.squirrel.hbl.mass$df),
                                      std.err.slope =  sum.squirrel.hbl.mass$coefficients[4],
                                      std.err.intercept = sum.squirrel.hbl.mass$coefficients[3],
                                      r.squared = sum.squirrel.hbl.mass$r.squared,
                                      p.value = sum.squirrel.hbl.mass$coefficients[,4][[2]],
                                      sample.size = nrow(Spermophilus[!is.na(Spermophilus$mass) & !is.na(Spermophilus$head.body.length),]))

nrow(Spermophilus[!is.na(Spermophilus$mass) & !is.na(Spermophilus$head.body.length),])
p = ggplot(data = Spermophilus) + 
  geom_point(aes(x = head.body.length, y = mass)) +
  geom_smooth(aes(x = head.body.length, y = mass), method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus N = 2300") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Body Mass (g)") +
  scale_x_log10(name = "Head Body Length (mm)")

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Spermophilus_hbl_mass.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(squirrel.hbl.tooth.stats, squirrel.hbl.mass.stats)

#gaussian error propogation
toothpredict$GauTRLp <- sqrt((sum.squirrel.hbl.mass$coefficients[[2]] * toothpredict$se.fit.hbl)^2)

#summing together the error from the gaussian propogation and the error from pushing the points through
toothpredict$sumerror <- sqrt((toothpredict$GauTRL)^2 + (toothpredict$se.fit.mass)^2)

#compare to the error in the relationship between tooth row and mass
squirrel.toothrow.mass <- lm(Spermophilus$mass[Spermophilus$MSW05_Binomial == "Spermophilus beecheyi"] ~ Spermophilus$tooth.row[Spermophilus$MSW05_Binomial == "Spermophilus beecheyi"], na.action = na.exclude)
sum.squirrel.toothrow.mass <- summary(squirrel.toothrow.mass)
predict.mass.tooth <- data.frame(predict(squirrel.toothrow.mass, toothpredict, se.fit = TRUE))
toothpredict$fit.mass.tooth <- predict.mass.tooth$fit
toothpredict$se.fit.mass.tooth <- predict.mass.tooth$se.fit

#plot this relationship
squirrel.toothrow.mass.stats <- data.frame(scientificName = "Spermophilus beecheyi",
                                           comparison = ("mass/tooth.row"),
                                           intercept = squirrel.toothrow.mass$coefficients[[1]],
                                           slope = squirrel.toothrow.mass$coefficients[[2]],
                                           resid.std.err = sum.squirrel.toothrow.mass$sigma,
                                           df = max(sum.squirrel.toothrow.mass$df),
                                           std.err.slope =  sum.squirrel.toothrow.mass$coefficients[4],
                                           std.err.intercept = sum.squirrel.toothrow.mass$coefficients[3],
                                           r.squared = sum.squirrel.toothrow.mass$r.squared,
                                           p.value = sum.squirrel.toothrow.mass$coefficients[,4][[2]],
                                           sample.size = nrow(Spermophilus[!is.na(Spermophilus$mass) & !is.na(Spermophilus$tooth.row) & Spermophilus$MSW05_Binomial == "Spermophilus beecheyi",]))

nrow(Spermophilus[!is.na(Spermophilus$mass) & !is.na(Spermophilus$tooth.row) & Spermophilus$MSW05_Binomial == "Spermophilus beecheyi",])
p = ggplot(data = filter(Spermophilus, Spermophilus$MSW05_Binomial == "Spermophilus beecheyi")) + 
  geom_point(aes(x = tooth.row, y = mass)) +
  geom_smooth(aes(x = tooth.row, y = mass), method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi N = 83") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Body Mass (g)") +
  scale_x_continuous(name = "Toothrow Length (mm)")

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_mass.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, squirrel.toothrow.mass.stats)

write.csv(model.Spermophilus.beecheyi, file= "model.results.whole.genus.Spermophilus.beecheyi.csv")

write.csv(toothpredict, file= "error.propogation.Spermophilus.beecheyi.csv")

#some questions: is it ok to be working in log log space this whole time? DO we want to transform them at the end of the analysis.

##Odocoileus virginianus----

Odocoileus <- pan.data[pan.data$MSW05_Genus == "Odocoileus",]

Odocoileus.clean <- subset(Odocoileus, Odocoileus$mass.status == "GOOD" & Odocoileus$mass.units == "g" 
                           & Odocoileus$total.length.status == "GOOD" & Odocoileus$total.length.units == "mm"
                           & Odocoileus$total.length > 0 & Odocoileus$mass > 0
                           & Odocoileus$hindfoot.length.status != "outlier" & Odocoileus$hindfoot.length.units == "mm",
                           select = c("MSW05_Binomial", "mass", "head.body.length", "hindfoot.length", "astragalus.length", "astragalus.width", "calcaneus.GB", "calcaneus.GL"))
Odocoileus <- Odocoileus.clean
write.csv(Odocoileus, "Odocoileus.csv")

## add paleo deer data----
old.deer <- read.csv("https://de.cyverse.org/dl/d/E237E454-9EE3-4169-8777-134D21B067FA/ArchaeoDeerAstragalusCalcaneus.csv", header = TRUE)

x.species <- Odocoileus$astragalus.length[Odocoileus$MSW05_Binomial == "Odocoileus virginianus" & !is.na(Odocoileus$astragalus.length)]
y.species <- Odocoileus$mass[Odocoileus$MSW05_Binomial == "Odocoileus virginianus" & !is.na(Odocoileus$astragalus.length)]
deer.mass.astragalus <- lm(y.species ~ x.species, na.action = na.exclude)
sum.deer.mass.astragalus <- summary(deer.mass.astragalus)
anklepredict <- data.frame(x.species = old.deer$astragalus.length) 
predict.mass <- data.frame(predict(deer.mass.astragalus, anklepredict, se.fit = TRUE))
anklepredict$fit.mass <- predict.mass$fit
anklepredict$se.fit.mass <- predict.mass$se.fit

p <- ggplot() +
  geom_smooth(data = Odocoileus, aes(x = astragalus.length, y = mass), method = "lm", color = "slateblue4", fill = "gray74")+
  geom_point(data = Odocoileus, aes(x = astragalus.length, y = mass)) +
  geom_point(data = anklepredict, aes(x = x.species, y = fit.mass), 
             shape = 18, size = 3, color = "gray36") +
  geom_errorbar(data = anklepredict, aes(x = x.species, ymin = fit.mass - se.fit.mass, ymax = fit.mass + se.fit.mass),
                color = "gray36") +
  ggtitle("Odocoileus virginianus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Body Mass (g)") +
  scale_x_continuous(name = "Astragalus Length (mm)") 
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_mass_predict.png"), width = 14, height = 10, units = "cm")

deer.mass.astragalus.stats <- data.frame(scientificName = "Odocoileus virginianus",
                                        comparison = ("mass/astragalus.length"),
                                        intercept = deer.mass.astragalus$coefficients[[1]],
                                        slope = deer.mass.astragalus$coefficients[[2]],
                                        resid.std.err = sum.deer.mass.astragalus$sigma,
                                        df = max(sum.deer.mass.astragalus$df),
                                        std.err.slope =  sum.deer.mass.astragalus$coefficients[4],
                                        std.err.intercept = sum.deer.mass.astragalus$coefficients[3],
                                        r.squared = sum.deer.mass.astragalus$r.squared,
                                        p.value = sum.deer.mass.astragalus$coefficients[,4][[2]],
                                        sample.size = nrow(Odocoileus[!is.na(Odocoileus$astragalus.length) & !is.na(Odocoileus$head.body.length & Odocoileus$MSW05_Binomial == "Odocoileus virginianus"),]))
write.csv(deer.mass.astragalus.stats, "deer.mass.astragalus.stats.csv")
write.csv(anklepredict, "anklepredict.csv")


#toothrow versus total length
x.species <- Odocoileus$astragalus.length[Odocoileus$MSW05_Binomial == "Odocoileus virginianus" & !is.na(Odocoileus$astragalus.length)]
y.species <- Odocoileus$head.body.length[Odocoileus$MSW05_Binomial == "Odocoileus virginianus" & !is.na(Odocoileus$astragalus.length)]
deer.hbl.astragalus <- lm(y.species ~ x.species, na.action = na.exclude)
sum.deer.hbl.astragalus <- summary(deer.hbl.astragalus)
anklepredict <- data.frame(x.species = old.deer$astragalus.length) #this will be the single point!
predict.hbl <- data.frame(predict(deer.hbl.astragalus, anklepredict, se.fit = TRUE))
anklepredict$fit.hbl <- predict.hbl$fit
anklepredict$se.fit.hbl <- predict.hbl$se.fit

#plotting this relationship here
deer.hbl.astragalus.stats <- data.frame(scientificName = "Odocoileus virginianus",
                                        comparison = ("head.body.length/astragalus.length"),
                                        intercept = deer.hbl.astragalus$coefficients[[1]],
                                        slope = deer.hbl.astragalus$coefficients[[2]],
                                        resid.std.err = sum.deer.hbl.astragalus$sigma,
                                        df = max(sum.deer.hbl.astragalus$df),
                                        std.err.slope =  sum.deer.hbl.astragalus$coefficients[4],
                                        std.err.intercept = sum.deer.hbl.astragalus$coefficients[3],
                                        r.squared = sum.deer.hbl.astragalus$r.squared,
                                        p.value = sum.deer.hbl.astragalus$coefficients[,4][[2]],
                                        sample.size = nrow(Odocoileus[!is.na(Odocoileus$astragalus.length) & !is.na(Odocoileus$head.body.length & Odocoileus$MSW05_Binomial == "Odocoileus virginianus"),]))

nrow(Odocoileus[!is.na(Odocoileus$astragalus.length) & !is.na(Odocoileus$head.body.length & Odocoileus$MSW05_Binomial == "Odocoileus virginianus"),])
p = ggplot(data = filter(Odocoileus, Odocoileus$MSW05_Binomial == "Odocoileus virginianus")) + 
  geom_point(aes(x = astragalus.length, y = head.body.length)) +
  geom_smooth(aes(x = astragalus.length, y = head.body.length), method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus, N = 24") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Head Body Length (mm)") +
  scale_x_continuous(name = "Astragalus Length (mm)")
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_hbl.png"), width = 14, height = 10, units = "cm")

#Now to push the toothrow points through the second regression. I am already in log 10 space with the predictions.
#need to sample the whole genus for this data
#some masses=0? below regression won't work if anything=0
x.genus <- Odocoileus$head.body.length
y.genus <- Odocoileus$mass
deer.hbl.mass <- lm(y.genus ~ x.genus, na.action = na.exclude)
sum.deer.hbl.mass <- summary(deer.hbl.mass)

#remember the variable has to have the same name for the predict function to work so we have to name it x2
fit.ankle <- data.frame(x.genus = anklepredict$fit.hbl)
predict.mass <- data.frame(predict(deer.hbl.mass, newdata = fit.ankle, se.fit = TRUE))
anklepredict$fit.mass <- predict.mass$fit
anklepredict$se.fit.mass <- predict.mass$se.fit

#plotting this relationship
deer.hbl.mass.stats <- data.frame(scientificName = "Odocoileus",
                                  comparison = ("mass/head.body.length"),
                                  intercept = deer.hbl.mass$coefficients[[1]],
                                  slope = deer.hbl.mass$coefficients[[2]],
                                  resid.std.err = sum.deer.hbl.mass$sigma,
                                  df = max(sum.deer.hbl.mass$df),
                                  std.err.slope =  sum.deer.hbl.mass$coefficients[4],
                                  std.err.intercept = sum.deer.hbl.mass$coefficients[3],
                                  r.squared = sum.deer.hbl.mass$r.squared,
                                  p.value = sum.deer.hbl.mass$coefficients[,4][[2]],
                                  sample.size = nrow(Odocoileus[!is.na(Odocoileus$mass) & !is.na(Odocoileus$head.body.length),]))

nrow(Odocoileus[!is.na(Odocoileus$mass) & !is.na(Odocoileus$head.body.length),])
p = ggplot(data = Odocoileus) + 
  geom_point(aes(x = head.body.length, y = mass)) +
  geom_smooth(aes(x = head.body.length, y = mass), method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus N = 753") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Body Mass (g)") +
  scale_x_continuous(name = "Head Body Length (mm)") 

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Odocoileus_hbl_mass.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(deer.hbl.astragalus.stats, deer.hbl.mass.stats)

#gaussian error propogation
anklepredict$GauTRLp <- sqrt((sum.deer.hbl.mass$coefficients[[2]] * anklepredict$se.fit.hbl)^2)

#summing together the error from the gaussian propogation and the error from pushing the points through
anklepredict$sumerror <- sqrt((anklepredict$GauTRL)^2 + (anklepredict$se.fit.mass)^2)

#compare to the error in the relationship between tooth row and mass
y.species2 <- Odocoileus$mass[Odocoileus$MSW05_Binomial == "Odocoileus virginianus" & !is.na(Odocoileus$astragalus.length)]
deer.astragalus.mass <- lm(y.species2 ~ x.species, na.action = na.exclude)
sum.deer.astragalus.mass <- summary(deer.astragalus.mass)
predict.mass.ankle <- data.frame(predict(deer.astragalus.mass, anklepredict, se.fit = TRUE))
anklepredict$fit.mass.astragalus <- predict.mass.ankle$fit
anklepredict$se.fit.mass.astragalus<- predict.mass.ankle$se.fit

#plot this relationship
deer.astragalus.mass.stats <- data.frame(scientificName = "Odocoileus virginianus",
                                         comparison = ("mass/astraglus.length"),
                                         intercept = deer.astragalus.mass$coefficients[[1]],
                                         slope = deer.astragalus.mass$coefficients[[2]],
                                         resid.std.err = sum.deer.astragalus.mass$sigma,
                                         df = max(sum.deer.astragalus.mass$df),
                                         std.err.slope =  sum.deer.astragalus.mass$coefficients[4],
                                         std.err.intercept = sum.deer.astragalus.mass$coefficients[3],
                                         r.squared = sum.deer.astragalus.mass$r.squared,
                                         p.value = sum.deer.astragalus.mass$coefficients[,4][[2]],
                                         sample.size = nrow(Odocoileus[!is.na(Odocoileus$mass) & !is.na(Odocoileus$astragalus.length) & Odocoileus$MSW05_Binomial == "Odocoileus virginianus",]))

nrow(Odocoileus[!is.na(Odocoileus$mass) & !is.na(Odocoileus$astragalus.length) & Odocoileus$MSW05_Binomial == "Odocoileus virginianus",])
p = ggplot(data = Odocoileus) + 
  geom_point(aes(x = astragalus.length, y = mass)) +
  geom_smooth(aes(x = astragalus.length, y = mass), method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus N = 24") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Body Mass (g)") +
  scale_x_continuous(name = "Astragalus Length (mm)")

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_mass.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, deer.astragalus.mass.stats)

write.csv(model.Odocoileus.virginianus, file= "model.results.whole.genus.Odoclileus.virginianus.csv")

write.csv(anklepredict, file= "error.propogation.Odoclileus.virginianus.csv")

##sum squared

colors <- c("se predicted mass from astragalus" = "darkslateblue", "sum error" = "lightslateblue")
                    
ggplot(data = anklepredict) +
  geom_density(aes(x = log10(se.fit.mass.astragalus)), color = "darkslateblue") + 
  geom_density(aes(x = log10(sumerror)), color = "lightslateblue")+ 
  ggtitle("Odocoileus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "Error Distribution", limits = c(2, 4))
  
ggplot(data = toothpredict) +
  geom_density(aes(x = log10(se.fit.mass.tooth)), col = "darkslateblue") + 
  geom_density(aes(x = log10(sumerror)), col = "lightslateblue") + 
  ggtitle("Spermophilus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "Error Distribution", limits = c(0, 2))

##Figure 3
toothpredict <- toothpredict[!is.na(toothpredict$fit.mass.tooth),]
which.minho(toothpredict$fit.mass.tooth)
toothpredict$fit.mass.tooth[61] #532.7334g
toothpredict$se.fit.mass.tooth[61]
toothpredict$sumerror[61]
which.max(toothpredict$fit.mass.tooth)
toothpredict$fit.mass.tooth[8] #596.594
toothpredict$se.fit.mass.tooth[8]
toothpredict$sumerror[8]

anklepredict <- anklepredict[!is.na(anklepredict$fit.mass.astragalus),]
which.min(anklepredict$fit.mass.astragalus)
anklepredict$fit.mass.astragalus[8] #31266.24
anklepredict$se.fit.mass.astragalus[8]
anklepredict$sumerror[8]
which.max(anklepredict$fit.mass.astragalus)
anklepredict$fit.mass.astragalus[14] #46048.74
anklepredict$se.fit.mass.astragalus[14]
anklepredict$sumerror[14]

toothpredict.se.fit.mass <- toothpredict$se.fit.mass.tooth
toothpredict.summerror <- toothpredict$sumerror
toothpredict.fit <- toothpredict$fit.mass.tooth
anklepreiict.se.fit.mass <- anklepredict$se.fit.mass.astragalus
anklepredict.fit <- anklepredict$fit.mass.astragalus
anklepredict.sumerror <- anklepredict$sumerror
squirrel.mass <- Spermophilus$mass
beecheyi.mass <- Spermophilus$mass[Spermophilus$MSW05_Binomial == "Spermophilus beecheyi"]
blois.mass <- Spermophilus$mass[Spermophilus$MSW05_Binomial == "Spermophilus beecheyi" & !is.na(Spermophilus$tooth.row)]
deer.mass <- Odocoileus$mass
virginianus.mass <- Odocoileus$mass[Odocoileus$MSW05_Binomial == "Odocoileus virginianus"]


p <- ggplot() +
  geom_boxplot(data = Spermophilus, aes(y = mass, x = .5), color = "darkslateblue",
               width = 0.25) +
  geom_boxplot(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi"), aes(y = mass, x = 1), color = "darkslateblue",
               width = 0.25) +
  geom_boxplot(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi" & !is.na(tooth.row)), 
                             aes(y = mass, x = 1.5), color = "darkslateblue", width = 0.25) +
  geom_boxplot(data = toothpredict, aes(y = fit.mass.tooth, x = 2), color = "darkslateblue",
               width = 0.25) +
  geom_point(data = toothpredict, aes(y = min(fit.mass.tooth) - sumerror[which.min(fit.mass.tooth)], x = 2), color = "gray34") + 
  geom_point(data = toothpredict, aes(y = max(fit.mass.tooth) + sumerror[which.max(fit.mass.tooth)], x = 2), color= "gray34") + 
  ggtitle("Spermophilus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text.y = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(name = "Body Mass (g)") +
  scale_x_continuous(name = "", breaks = seq(0, 2, .5), labels = c("", "Genus Level", "Species level","Original" , "Estimated")) 
ggsave(p, file=paste0("plot_squirrel_mass_estimates.png"), width = 14, height = 10, units = "cm")

p <- ggplot() +
  geom_boxplot(data = Odocoileus, aes(y = mass, x = .5), color = "darkslateblue",
               width = 0.25) +
  geom_boxplot(data = filter(Odocoileus, MSW05_Binomial == "Odocoileus virginianus"), aes(y = mass, x = 1), color = "darkslateblue",
               width = 0.25) +
  geom_boxplot(data = anklepredict, aes(y = fit.mass.astragalus, x = 1.5), color = "darkslateblue",
               width = 0.25) +
  geom_point(data = anklepredict, aes(y = sumerror[which.min(fit.mass.astragalus)], x =1.5), color = "gray34") + 
  geom_point(data = anklepredict, aes(y = sumerror[which.max(fit.mass.astragalus)], x = 1.5), color= "gray34") + 
  ggtitle("Odocoileus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text.y = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(name = "Body Mass (g)") +
  scale_x_continuous(name = "", breaks = seq(0, 1.5, .5), labels = c("", "Genus Level", "Species level", "Estimated")) 
ggsave(p, file=paste0("plot_deer_mass_estimates.png"), width = 14, height = 10, units = "cm")

  
    #genus level
  geom_segment(data = Spermophilus, aes(x = min(mass, na.rm = TRUE),xend = max(mass, na.rm = TRUE), y = 2, yend = 2), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = Spermophilus, aes(xmin = min(mass), xmax = min(mass), y = 2), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = Spermophilus, aes(xmin = max(mass), xmax = max(mass), y = 2), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #geom_point(data = Spermophilus, aes(x = min(mass, na.rm = TRUE), y = 2), color = "darkslateblue") +
  #geom_point(data = Spermophilus, aes(x = max(mass, na.rm = TRUE), y = 2), color = "darkslateblue") +
  #species level
  geom_segment(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi"), aes(x = min(mass, na.rm = TRUE), xend = max(mass, na.rm = TRUE), y = 1.5, yend = 1.5), color = "darkslateblue") +
  #geom_point(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi"), aes(x = min(mass, na.rm = TRUE), y = 1.5), color = "darkslateblue") +
  #geom_point(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi"), aes(x = max(mass, na.rm = TRUE), y = 1.5), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi"), aes(xmin = min(mass), xmax = min(mass), y = 1.5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi"), aes(xmin = max(mass), xmax = max(mass), y = 1.5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #blois distribution
  geom_segment(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi" & !is.na(tooth.row)), aes(x = min(mass, na.rm = TRUE), xend = max(mass, na.rm = TRUE), y = 1, yend = 1), color = "darkslateblue") +
  #geom_point(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi" & !is.na(tooth.row)), aes(x = min(mass, na.rm = TRUE), y = 1), color = "darkslateblue") +
  #geom_point(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi" & !is.na(tooth.row)), aes(x = max(mass, na.rm = TRUE), y = 1), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi" & !is.na(tooth.row)), aes(xmin = min(mass), xmax = min(mass), y = 1), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = filter(Spermophilus, MSW05_Binomial == "Spermophilus beecheyi" & !is.na(tooth.row)), aes(xmin = max(mass), xmax = max(mass), y = 1), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #estimated
  geom_segment(data = toothpredict, aes(x = min(fit.mass.tooth, na.rm = TRUE), xend = max(fit.mass.tooth, na.rm = TRUE), y = .5, yend = .5), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = toothpredict, aes(xmin = fit.mass.tooth[61] - sumerror[61], xmax = fit.mass.tooth[61], y = .5), 
                 height = 0.05, color = "darkgray", linetype = "dashed", alpha = 0.5) +
  #upper
  geom_errorbarh(data = toothpredict, aes(xmin = fit.mass.tooth[8], xmax = sumerror[8] + fit.mass.tooth[8], y = .5), 
                 height = 0.05, color = "darkgray", linetype = "dashed", alpha = 0.5) +
  #geom_point(data = toothpredict, aes(x = fit.mass.tooth[61], y = .5)) + 
  #geom_point(data = toothpredict, aes(x = fit.mass.tooth[8], y = .5)) + 
  #lower
  geom_errorbarh(data = toothpredict, aes(xmin = min(fit.mass.tooth), xmax = min(fit.mass.tooth), y = .5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = toothpredict, aes(xmin = max(fit.mass.tooth), xmax = max(fit.mass.tooth), y = .5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  ggtitle("Spermophilus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)") +
  scale_y_continuous(name = "", breaks = seq(0, 2, .5), labels = c("", "Estimated", "Original","Species level", "Genus level")) 



  
  
   #genus level
  geom_segment(data = Odocoileus, aes(x = min(mass, na.rm = TRUE),xend = max(mass, na.rm = TRUE), y = 1.5, yend = 1.5), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = Odocoileus, aes(xmin = min(mass), xmax = min(mass), y = 1.5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = Odocoileus, aes(xmin = max(mass), xmax = max(mass), y = 1.5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #species level
  geom_segment(data = filter(Odocoileus, MSW05_Binomial == "Odocoileus virginianus"), aes(x = min(mass, na.rm = TRUE), xend = max(mass, na.rm = TRUE), y = 1, yend = 1), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = filter(Odocoileus, MSW05_Binomial == "Odocoileus virginianus"), aes(xmin = min(mass), xmax = min(mass), y = 1), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = filter(Odocoileus, MSW05_Binomial == "Odocoileus virginianus"), aes(xmin = max(mass), xmax = max(mass), y = 1), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #estimated
  geom_segment(data = anklepredict, aes(x = min(fit.mass.astragalus, na.rm = TRUE), xend = max(fit.mass.astragalus, na.rm = TRUE), y = .5, yend = .5), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = anklepredict, aes(xmin = fit.mass.astragalus[8] - sumerror[8], xmax = fit.mass.astragalus[8], y = .5), 
                 height = 0.05, color = "darkgray", alpha = 0.5) +
  #upper
  geom_errorbarh(data = anklepredict, aes(xmin = fit.mass.astragalus[14], xmax = sumerror[14] + fit.mass.astragalus[14], y = .5), 
                 height = 0.05, color = "darkgray", alpha = 0.5) +
  #lower
  geom_errorbarh(data = anklepredict, aes(xmin = min(fit.mass.astragalus), xmax = min(fit.mass.astragalus), y = .5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = anklepredict, aes(xmin = max(fit.mass.astragalus), xmax = max(fit.mass.astragalus), y = .5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  ggtitle("Odocoileus") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)") +
  scale_y_continuous(name = "", breaks = seq(0, 1.5, .5), labels = c("", "Estimated", "Species level", "Genus level")) 

ggsave(p, file=paste0("plot_deer_mass_estimates.png"), width = 14, height = 10, units = "cm")




geom_density(data = Odocoileus, aes(x = mass), color = "NA", alpha = .7) +
  geom_segment(data = Odocoileus, aes(x = min(mass, na.rm = TRUE),xend = max(mass, na.rm = TRUE), y = 1, yend = 1), color = "darkgray") +
  geom_point(data = Odocoileus, aes(x = min(mass, na.rm = TRUE), y = 1), color = "darkgray") +
  geom_point(data = Odocoileus, aes(x = max(mass, na.rm = TRUE), y = 1), color = "darkgray") +
  geom_segment(data = anklepredict, aes(x = min(fit.mass.astragalus, na.rm = TRUE), xend = max(fit.mass.astragalus, na.rm = TRUE), y = .5, yend = .5), color = "darkslateblue") +
  #lower
  geom_errorbarh(data = anklepredict, aes(xmin = fit.mass.astragalus[8] - sumerror[8], xmax = fit.mass.astragalus[8], y = .5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  #upper
  geom_errorbarh(data = anklepredict, aes(xmin = fit.mass.astragalus[14], xmax = sumerror[14] + fit.mass.astragalus[14], y = .5), 
                 height = 0.05, color = "darkslateblue", alpha = 0.5) +
  geom_point(data = anklepredict, aes(x = fit.mass.astragalus[8], y = .5)) + 
  geom_point(data = anklepredict, aes(x = fit.mass.astragalus[14], y = .5)) + 
  ggtitle("Mass Estimation compared to Original Distribution of Masses") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)") +
  scale_y_continuous(name = "Taxa", breaks = seq(0, 1, .5), 
                     labels = c("","predicted mass range", "original mass range")) 

  
