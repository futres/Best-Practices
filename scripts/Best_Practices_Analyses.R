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

pan.adult_stats2 <- pan.data.mass %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size = length(mass), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.err.mass = sd(mass, na.rm = TRUE)/sqrt(sample.size),
                   pan.mass = X5.1_AdultBodyMass_g[1],
                   mass.diff = (pan.mass - avg.mass),
                   mass.diff.se = mass.diff / sd.err.mass,
                   abs.mass.diff.se = abs(mass.diff.se),
                   p.value = t.test(mass, mu = pan.mass, conf.level = 0.95)$p.value,
                   critical.t = abs(tinv(p = 0.05, nu = (sample.size-1))), #get a different result than from Edward
                   diff.amt = abs.mass.diff.se-critical.t,
                   sig = diff.amt > 0, # true means sig diff
                   #p.value = pt(-abs(critical.t), df = (sample.size-1)) #trying to calculate p-value from critical t...
                   ) %>%
  as.data.frame()

pan.adult_stats2$corr <- p.adjust(pan.adult_stats2$p.value, method = "BH")
pan.adult_stats2$sig.corr <- pan.adult_stats2$corr <= 0.05 #TRUE means sig diff

##write mass difference results----
write.csv(pan.adult_stats, "pan.results.csv")
write.csv(pan.adult_stats2, "pan.results.t.test.csv")

plot(x = log10(pan.adult_stats2$avg.mass), y = log10(pan.adult_stats2$sd.err.mass),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Standard Error of Mass",
     title(main = "Relationship between standard error and average mass",
           sub = "slope = 1.16, p<0.001"))
model <- lm(log10(pan.adult_stats2$sd.err.mass) ~ log10(pan.adult_stats2$avg.mass))
summary(model)

plot(x = pan.adult_stats2$sample.size, y = pan.adult_stats2$abs.mass.diff.se,
     xlab = "Sample Size",
     ylab = "Absolute Difference between Pan Mass and Avg. Mass over Standard Error",
     title(main = "Relationship between Sample Size on Mass differences",
           sub = "slope = 0, p=0.9"))
model3 <- lm(pan.adult_stats2$abs.mass.diff.se ~ pan.adult_stats2$sample.size + pan.adult_stats2$avg.mass)
summary(model3)
model4 <- lm(pan.adult_stats2$abs.mass.diff.se ~ pan.adult_stats2$sample.size + pan.adult_stats2$sd.err.mass)
summary(model4)

plot(x = log10(pan.adult_stats2$avg.mass), y = log10(pan.adult_stats2$mass.diff.se),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Mass Difference",
     title(main = "Relationship between animal size and degree of difference between masses",
           sub = "slope = 0.003, p=.92"))
model2 <- lm(log10(pan.adult_stats2$mass.diff.se) ~ log10(pan.adult_stats2$avg.mass))
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
pan.adult_stats3 <- pan.adult_stats2[pan.adult_stats2$mass.diff.se < 500,]
p <- ggplot(data = pan.adult_stats3, aes(x = mass.diff.se)) +
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

Sbeecheyi <- data[data$scientificName == "Spermophilus beecheyi",]

Sbeecheyi.clean <- subset(Sbeecheyi, Sbeecheyi$mass.status == "GOOD" & Sbeecheyi$total.length.status == "GOOD" 
                          & Sbeecheyi$mass.units == "g" & Sbeecheyi$total.length.units == "mm"
                          & Sbeecheyi$mass > 0 & Sbeecheyi$total.length > 0,
                             select = c("scientificName","mass", "head.body.length", "tooth.row", "hindfoot.length"))
Sbeecheyi <- Sbeecheyi.cleaner
write.csv(Sbeecheyi, "Sbeecheyi.csv")

#Code for plotting Spermophilus beecheyi

#Now we want to see if translating between the models is fruitful. The first step is to predict total length from tooth row length
#predict function works better if the naming scheme inside the lm model is simple. It will automatically log the values.
#toothrow versus total length
squirrel.hbl.tooth <- lm(log10(Sbeecheyi$head.body.length) ~ log10(Sbeecheyi$tooth.row), na.action = na.exclude)
sum.squirrel.hbl.tooth <- summary(squirrel.hbl.tooth)
toothpredict <- data.frame(x = Sbeecheyi$tooth.row)
predict.hbl <- data.frame(predict(squirrel.hbl.tooth, toothpredict, se.fit = TRUE))
toothpredict$fit.hbl <- predict.hbl$fit
toothpredict$se.fit.hbl <- predict.hbl$se.fit

#plotting this relationship here
squirrel.hbl.tooth.stats <- data.frame(Binomial = Sbeecheyi$scientificName[1],
                                       comparison = ("head.body.length/tooth.row"),
                                       intercept = squirrel.hbl.tooth$coefficients[[1]],
                                       slope = squirrel.hbl.tooth$coefficients[[2]],
                                       resid.std.err = sum.squirrel.hbl.tooth$sigma,
                                       df = max(sum.squirrel.hbl.tooth$df),
                                       std.err.slope =  sum.squirrel.hbl.tooth$coefficients[4],
                                       std.err.intercept = sum.squirrel.hbl.tooth$coefficients[3],
                                       r.squared = sum.squirrel.hbl.tooth$r.squared,
                                       sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$tooth.row) & !is.na(Sbeecheyi$head.body.length),]))

nrow(Sbeecheyi[!is.na(Sbeecheyi$tooth.row) & !is.na(Sbeecheyi$head.body.length),])
p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(head.body.length))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(head.body.length)), method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi, N = 85") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Head~Body~Length~(mm))) +
  scale_x_log10(expression(log[10]~Toothrow~Length~(mm)))
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_hbl.png"), width = 14, height = 10, units = "cm")

#Now to push the toothrow points through the second regression. I am already in log 10 space with the predictions.
#need to sample the whole genus for this data
#some masses=0? below regression won't work if anything=0
squirrel.hbl.mass <- lm(Sbeecheyi$mass ~ Sbeecheyi$head.body.length, na.action = na.exclude)
sum.squirrel.hbl.mass <- summary(squirrel.hbl.mass)

#remember the variable has to have the same name for the predict function to work so we have to name it x2
fit.tooth <- data.frame(x2 = toothpredict$fit.hbl)
predict.mass <- data.frame(predict(squirrel.hbl.mass, newdata = fit.tooth, se.fit = TRUE))
toothpredict$fit.mass <- predict.mass$fit
toothpredict$se.fit.mass <- predict.mass$se.fit

#plotting this relationship
squirrel.hbl.mass.stats <- data.frame(Binomial = Sbeecheyi$scientificName[1],
                                      comparison = ("mass/head.body.length"),
                                      intercept = squirrel.hbl.mass$coefficients[[1]],
                                      slope = squirrel.hbl.mass$coefficients[[2]],
                                      resid.std.err = sum.squirrel.hbl.mass$sigma,
                                      df = max(sum.squirrel.hbl.mass$df),
                                      std.err.slope =  sum.squirrel.hbl.mass$coefficients[4],
                                      std.err.intercept = sum.squirrel.hbl.mass$coefficients[3],
                                      r.squared = sum.squirrel.hbl.mass$r.squared,
                                      sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$head.body.length),]))

nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$head.body.length),])
p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(head.body.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(head.body.length), y = log10(mass)), method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi N = 204") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Body~Mass~(g))) +
  scale_x_log10(expression(log[10]~Head~Body~Length~(mm)))

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Spermophilus beecheyi_hbl_mass.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(squirrel.hbl.tooth.stats, squirrel.hbl.mass.stats)

#gaussian error propogation
toothpredict$GauTRLp <- sqrt((sum.squirrel.hbl.mass$coefficients[[2]] * toothpredict$se.fit.hbl)^2)

#summing together the error from the gaussian propogation and the error from pushing the points through
toothpredict$sumerror <- sqrt((toothpredict$GauTRL)^2 + (toothpredict$se.fit.mass)^2)

#compare to the error in the relationship between tooth row and mass
squirrel.toothrow.mass <- lm(log10(Sbeecheyi$mass) ~ log10(Sbeecheyi$tooth.row), na.action = na.exclude)
sum.squirrel.toothrow.mass <- summary(squirrel.toothrow.mass)
predict.mass.tooth <- data.frame(predict(squirrel.toothrow.mass, toothpredict, se.fit = TRUE))
toothpredict$fit.mass.tooth <- predict.mass.tooth$fit
toothpredict$se.fit.mass.tooth <- predict.mass.tooth$se.fit

#plot this relationship
squirrel.toothrow.mass.stats <- data.frame(Binomial = Sbeecheyi$scientificName[1],
                                           comparison = ("mass/tooth.row"),
                                           intercept = squirrel.toothrow.mass$coefficients[[1]],
                                           slope = squirrel.toothrow.mass$coefficients[[2]],
                                           resid.std.err = sum.squirrel.toothrow.mass$sigma,
                                           df = max(sum.squirrel.toothrow.mass$df),
                                           std.err.slope =  sum.squirrel.toothrow.mass$coefficients[4],
                                           std.err.intercept = sum.squirrel.toothrow.mass$coefficients[3],
                                           r.squared = sum.squirrel.toothrow.mass$r.squared,
                                           sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$tooth.row),]))

nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$tooth.row),])
p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(mass))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(mass)), method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi N = 86") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Body~Mass~(g))) +
  scale_x_log10(expression(log[10]~Tooth~row~Length~(mm)))

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_mass.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, squirrel.toothrow.mass.stats)

write.csv(model.Spermophilus.beecheyi, file= "model.results.whole.genus.Spermophilus.beecheyi.csv")

write.csv(toothpredict, file= "error.propogation.Spermophilus.beecheyi.csv")

#some questions: is it ok to be working in log log space this whole time? DO we want to transform them at the end of the analysis.

##Odocoileus virginianus----

Ovirginianus <- data[data$scientificName == "Odocoileus virginianus",]

Ovirginianus.clean <- subset(Ovirginianus, Ovirginianus$mass.status == "GOOD" & Ovirginianus$mass.units == "g" 
                             & Ovirginianus$total.length.status == "GOOD" & Ovirginianus$total.length.units == "mm"
                             & Ovirginianus$total.length > 0 & Ovirginianus$mass > 0
                             & Ovirginianus$hindfoot.length.status != "outlier" & Ovirginianus$hindfoot.length.units == "mm",
                             select = c("scientificName", "mass", "head.body.length", "hindfoot.length", "astragalus.length", "astragalus.width", "calcaneus.GB", "calcaneus.GL"))
Ovirginianus <- Ovirginianus.clean
write.csv(Ovirginianus, "Ovirginianus.csv")

#toothrow versus total length
deer.hbl.astragalus <- lm(log10(Ovirginianus$head.body.length) ~ log10(Ovirginianus$astragalus.length), na.action = na.exclude)
sum.squirrel.hbl.astragalus <- summary(deer.hbl.astragalus)
anklepredict <- data.frame(x = Ovirginianus$astragalus.length)
predict.hbl <- data.frame(predict(deer.hbl.astragalus, anklepredict, se.fit = TRUE))
anklepredict$fit.hbl <- predict.hbl$fit
anklepredict$se.fit.hbl <- predict.hbl$se.fit

#plotting this relationship here
deer.hbl.tooth.stats <- data.frame(Binomial = Ovirginianus$scientificName[1],
                                   comparison = ("head.body.length/astragalus.length"),
                                   intercept = deer.hbl.astragalus$coefficients[[1]],
                                   slope = deer.hbl.astragalus$coefficients[[2]],
                                   resid.std.err = sum.deer.hbl.astragalus$sigma,
                                   df = max(sum.deer.hbl.astragalus$df),
                                   std.err.slope =  sum.deer.hbl.astragalus$coefficients[4],
                                   std.err.intercept = sum.deer.hbl.astragalus$coefficients[3],
                                   r.squared = sum.deer.hbl.astragalus$r.squared,
                                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$head.body.length),]))

nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$head.body.length),])
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(head.body.length))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(head.body.length)), method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus, N = 85") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Head~Body~Length~(mm))) +
  scale_x_log10(expression(log[10]~Astragalus~Length~(mm)))
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_hbl.png"), width = 14, height = 10, units = "cm")

#Now to push the toothrow points through the second regression. I am already in log 10 space with the predictions.
#need to sample the whole genus for this data
#some masses=0? below regression won't work if anything=0
deer.hbl.mass <- lm(Ovirginianus$mass ~ Ovirginianus$head.body.length, na.action = na.exclude)
sum.deer.hbl.mass <- summary(deer.hbl.mass)

#remember the variable has to have the same name for the predict function to work so we have to name it x2
fit.ankle <- data.frame(x2 = anklepredict$fit.hbl)
predict.mass <- data.frame(predict(deer.hbl.mass, newdata = fit.tooth, se.fit = TRUE))
anklepredict$fit.mass <- predict.mass$fit
anklepredict$se.fit.mass <- predict.mass$se.fit

#plotting this relationship
deer.hbl.mass.stats <- data.frame(Binomial = Ovirginianus$scientificName[1],
                                  comparison = ("mass/head.body.length"),
                                  intercept = deer.hbl.mass$coefficients[[1]],
                                  slope = deer.hbl.mass$coefficients[[2]],
                                  resid.std.err = sum.deer.hbl.mass$sigma,
                                  df = max(sum.deer.hbl.mass$df),
                                  std.err.slope =  sum.deer.hbl.mass$coefficients[4],
                                  std.err.intercept = sum.deer.hbl.mass$coefficients[3],
                                  r.squared = sum.deer.hbl.mass$r.squared,
                                  sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$head.body.length),]))

nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$head.body.length),])
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(head.body.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(head.body.length), y = log10(mass)), method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus N = 204") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Body~Mass~(g))) +
  scale_x_log10(expression(log[10]~Head~Body~Length~(mm)))

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Odocoileus virginianus_hbl_mass.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(deer.hbl.astragalus.stats, deer.hbl.mass.stats)

#gaussian error propogation
anklepredict$GauTRLp <- sqrt((sum.deer.hbl.mass$coefficients[[2]] * anklepredict$se.fit.hbl)^2)

#summing together the error from the gaussian propogation and the error from pushing the points through
anklepredict$sumerror <- sqrt((anklepredict$GauTRL)^2 + (anklepredict$se.fit.mass)^2)

#compare to the error in the relationship between tooth row and mass
deer.astragalus.mass <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$astragalus.length), na.action = na.exclude)
sum.deer.astragalus.mass <- summary(deer.astragalus.mass)
predict.mass.ankle <- data.frame(predict(deer.astragalus.mass, anklepredict, se.fit = TRUE))
anklepredict$fit.mass.astragalus <- predict.mass.ankle$fit
anklepredict$se.fit.mass.astragalus<- predict.mass.ankle$se.fit

#plot this relationship
deer.astragalus.mass.stats <- data.frame(Binomial = Ovirginianus$scientificName[1],
                                         comparison = ("mass/astraglus.length"),
                                         intercept = deer.astragalus.mass$coefficients[[1]],
                                         slope = deer.astragalus.mass$coefficients[[2]],
                                         resid.std.err = sum.deer.astragalus.mass$sigma,
                                         df = max(sum.deer.astragalus.mass$df),
                                         std.err.slope =  sum.deer.astragalus.mass$coefficients[4],
                                         std.err.intercept = sum.deer.astragalus.mass$coefficients[3],
                                         r.squared = sum.deer.astragalus.mass$r.squared,
                                         sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$astragalus.length),]))

nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$tooth.row),])nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$astragalus.length),])
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(mass)), method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus N = 86") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Body~Mass~(g))) +
  scale_x_log10(expression(log[10]~Astragalus~Length~(mm)))

#seems like there is a outlier on this plot.
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_mass.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, deer.astragalus.mass.stats)

write.csv(model.Odocoileus.virginianus, file= "model.results.whole.genus.Odoclileus.virginianus.csv")

write.csv(toothpredict, file= "error.propogation.Odoclileus.virginianus.csv")







deer.mass.astragalus <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$astragalus.length), na.action = na.exclude)
sum.deer.mass.astragalus <- summary(deer.mass.astragalus)
deer.mass.astragalus.stats <- data.frame(Binomial = Ovirginianus$scientificName[1],
                                       intercept = deer.mass.astragalus$coefficients[[1]],
                                       slope = deer.mass.astragalus$coefficients[[2]],
                                       resid.std.err = sum.deer.mass.astragalus$sigma,
                                       df = max(sum.deer.mass.astragalus$df),
                                       std.err.slope =  sum.deer.mass.astragalus$coefficients[4],
                                       std.err.intercept = sum.deer.mass.astragalus$coefficients[3],
                                       r.squared = sum.deer.mass.astragalus$r.squared,
                                       sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$astragalus.length),]))

nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$astragalus.length),])
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(mass)),
              method = "lm", color = "slateblue4") +
  ggtitle("Odocoileus virginianus N = 24") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Body~Mass~(g))) +
  scale_x_log10(expression(log[10]~Astragalus~Length~(mm)))
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_mass.png"), width = 14, height = 10, units = "cm")

deer.hbl.astragalus <- lm(log10(Ovirginianus$head.body.length) ~ log10(Ovirginianus$astragalus.length), na.action = na.exclude)
sum.deer.hbl.astragalus <- summary(deer.hbl.astragalus)
deer.hbl.astragalus.stats <- data.frame(Binomial = Ovirginianus$scientificName[1],
                                        intercept = deer.hbl.astragalus$coefficients[[1]],
                                        slope = deer.hbl.astragalus$coefficients[[2]],
                                        resid.std.err = sum.deer.hbl.astragalus$sigma,
                                        df = max(sum.deer.hbl.astragalus$df),
                                        std.err.slope =  sum.deer.hbl.astragalus$coefficients[4],
                                        std.err.intercept = sum.deer.hbl.astragalus$coefficients[3],
                                        r.squared = sum.deer.hbl.astragalus$r.squared,
                                        sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$head.body.length),]))

nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$head.body.length),])
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(head.body.length))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(head.body.length)),
              method = "lm", color = "slateblue4") +
  ggtitle("Odocoileus virginianus N = 24") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Head~Body~Length~(mm))) +
  scale_x_log10(expression(log[10]~Astragalus~Length~(mm)))
ggsave(p, file=paste0("plot_Odocoileus virginianus_hbl_astragalus.png"), width = 14, height = 10, units = "cm")

deer.hbl.mass <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$head.body.length), na.action = na.exclude)
sum.deer.hbl.mass <- summary(deer.hbl.mass)
deer.hbl.mass.stats <- data.frame(Binomial = Ovirginianus$scientificName[1],
                                  intercept = deer.hbl.mass$coefficients[[1]],
                                  slope = deer.hbl.mass$coefficients[[2]],
                                  resid.std.err = sum.deer.hbl.mass$sigma,
                                  df = max(sum.deer.hbl.mass$df),
                                  std.err.slope =  sum.deer.hbl.mass$coefficients[4],
                                  std.err.intercept = sum.deer.hbl.mass$coefficients[3],
                                  r.squared = sum.deer.hbl.mass$r.squared,
                                  sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$head.body.length),]))

nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$head.body.length),])
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(head.body.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(head.body.length), y = log10(mass)),
              method = "lm", color = "slateblue4") +
  ggtitle("Odocoileus virginianus N = 671") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_log10(expression(log[10]~Body~Mass~(g))) +
  scale_x_log10(expression(log[10]~Head~Body~Length~(mm)))
ggsave(p, file=paste0("plot_Odocoileus virginianus_hbl_mass.png"), width = 14, height = 10, units = "cm")

               
## add paleo deer data----
old.deer <- read.csv("https://de.cyverse.org/dl/d/E237E454-9EE3-4169-8777-134D21B067FA/ArchaeoDeerAstragalusCalcaneus.csv", header = TRUE)







