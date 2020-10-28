# Best Practices Data Clean Up
# Meghan A. Balk
# balkm@email.arizona.edu

## Load packages
require(tidyverse)
require(nlme)
require(dplyr)
require(ggplot2)
require(reshape2)
require(plyr)
require(stringr)

## Upload data
pan <- read.csv("https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("https://de.cyverse.org/dl/d/D0FE0589-D2A9-4826-9A33-8ECE00B88965/clean.futres.data.csv", header = TRUE, stringsAsFactors = FALSE)

#Q1 compare to pantheria 
sp.data <- unique(data$scientificName) #565 spp
pan <- pan[pan$MSW05_Binomial %in% sp.data,] #502 spp
pan.sub <- subset(pan, select = c("MSW05_Order", "MSW05_Family", "MSW05_Genus", "MSW05_Binomial", "X5.1_AdultBodyMass_g"))
pan.sub.clean <- pan.sub[!is.na(pan.sub$X5.1_AdultBodyMass_g),] #474 sp

pan.data.adult <- merge(pan.sub.clean, data, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = FALSE, all.x = FALSE)

#write.csv(pan.data.adult, "data.taxonomy.csv")

pan.data.adult.clean <- pan.data.adult[!is.na(pan.data.adult$mass),] #16 sp

pan.data.adult_stats <- pan.data.adult.clean %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.err.mass = sd(mass, na.rm = TRUE)/sqrt(sample.size),
                   pan.mass = X5.1_AdultBodyMass_g[1],
                   mass.diff = abs((pan.mass - avg.mass) / sd.err.mass))
pan.data.adult_stats.10 <- pan.data.adult_stats %>%
  filter(sample.size >= 10) #8 spp
#write.csv(pan.data.adult_stats.10, "pan.results.csv")

length(pan.data.adult_stats.10$MSW05_Binomial[pan.data.adult_stats.10$mass.diff <= 2]) #136
length(pan.data.adult_stats.10$MSW05_Binomial[pan.data.adult_stats.10$mass.diff > 2]) #273

#136 out of 409 are different; 33.3%

# FIGURE: body mass distributions w/ line from PanTHERIA

pan.data.adult.clean.10 <- pan.data.adult.clean[pan.data.adult.clean$MSW05_Binomial %in% pan.data.adult_stats.10$MSW05_Binomial,]

uniq_species <- unique(pan.data.adult.clean.10$MSW05_Binomial)
for (i in uniq_species) {
  p = ggplot(data = subset(pan.data.adult.clean.10, MSW05_Binomial == i)) + 
    geom_density(aes(log10(mass)), fill = "darkslateblue") +
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_continuous(name = 'Probability') + 
    geom_vline(xintercept = log10(pan.data.adult.clean.10$X5.1_AdultBodyMass_g[pan.data.adult.clean.10$MSW05_Binomial == i][1]))
  ggsave(p, file=paste0("dist_", i,".png"), width = 14, height = 10, units = "cm")
}

################################
#Q2: length v. mass
data.adult.trim.clean <- data[!is.na(data$mass),]
data.adult.trim.cleaner <- data.adult.trim.clean[!is.na(data.adult.trim.clean$total.length),]
#13 spp

#recount sample sizes
data.adult.trim.cleaner_stats <- data.adult.trim.cleaner %>%
  group_by(scientificName) %>%
  dplyr::summarise(counts = n())

keep.adult.trim.clean <- data.adult.trim.cleaner_stats$scientificName[data.adult.trim.cleaner_stats$counts >= 10]
data.adult.trim.cleaner.10 <- data.adult.trim.cleaner[data.adult.trim.cleaner$scientificName %in% keep.adult.trim.clean,]
#8 sp

sp.models <- unique(data.adult.trim.cleaner.10$scientificName)
model.results.species <- data.frame()
for(i in 1:length(sp.models)){
  sub.data <- as.data.frame(data.adult.trim.cleaner.10[data.adult.trim.cleaner.10$scientificName == sp.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$total.length), na.action=na.exclude)
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
  model.results.species <- rbind(sub, model.results.species)
}

#lots of repeats for Neofiber alleni; only 4 unique occurrence ids....why???

#write.csv(model.results.species, "model.results.species.csv")

uniq_species <- unique(data.adult.trim.cleaner.10$scientificName)
for (i in uniq_species) {
  p = ggplot(data = subset(data.adult.trim.cleaner.10, scientificName  == i)) + 
    geom_point(aes(x = log10(mass), y = log10(total.length))) +
    geom_smooth(aes(x = log10(mass), y = log10(total.length)),
                method = "lm", color = "slateblue4")
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_log10(name = expression(log[10]~Total~Length~(mm))) + 
  ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

#WEIRD ONES: 
#Akodon mimus
#Aplodontia rufa
#Artibeus lituratus, jamaicensis

#family, order
#link species to higher taxonomy
#how do confidence in these to compare?
#how many individuals or spp do you need for it to be sensible

sp.pan <- unique(data.adult.trim.cleaner$scientificName) #73 spp
pan.models <- pan[pan$MSW05_Binomial %in% sp.pan,] #68 spp
pan.models <- pan.models[,1:5]

taxon.data.adult.trim.cleaner <- merge(data.adult.trim.cleaner, pan.models, by.x = "scientificName", by.y = "MSW05_Binomial", all.x = TRUE, all.y = FALSE)
na.taxon <- taxon.data.adult.trim.cleaner[is.na(taxon.data.adult.trim.cleaner$MSW05_Order),]
#"Clethrionomys gapperi" "Lutra canadensis"      "Myotis aurascens"     "Peromyscus sp." "Urocitellus parryii"  

taxon.data.adult.trim.cleaner$MSW05_Order[taxon.data.adult.trim.cleaner$scientificName == "Clethrionomys gapperi"] <- "Rodentia"
taxon.data.adult.trim.cleaner$MSW05_Family[taxon.data.adult.trim.cleaner$scientificName == "Clethrionomys gapperi"] <- "Cricetidae"
taxon.data.adult.trim.cleaner$MSW05_Genus[taxon.data.adult.trim.cleaner$scientificName == "Clethrionomys gapperi"] <- "Clethrionomys"

taxon.data.adult.trim.cleaner$MSW05_Order[taxon.data.adult.trim.cleaner$scientificName == "Lutra canadensis"] <- "Carnivora"
taxon.data.adult.trim.cleaner$MSW05_Family[taxon.data.adult.trim.cleaner$scientificName == "Lutra canadensis"] <- "Mustelidae"
taxon.data.adult.trim.cleaner$MSW05_Genus[taxon.data.adult.trim.cleaner$scientificName == "Lutra canadensis"] <- "Lutra"

taxon.data.adult.trim.cleaner$MSW05_Order[taxon.data.adult.trim.cleaner$scientificName == "Myotis aurascens"] <- "Chiroptera"
taxon.data.adult.trim.cleaner$MSW05_Family[taxon.data.adult.trim.cleaner$scientificName == "Myotis aurascens"] <- "Vespertilionidae"
taxon.data.adult.trim.cleaner$MSW05_Genus[taxon.data.adult.trim.cleaner$scientificName == "Myotis aurascens"] <- "Myotis"

taxon.data.adult.trim.cleaner$MSW05_Order[taxon.data.adult.trim.cleaner$scientificName == "Peromyscus sp."] <- "Rodentia"
taxon.data.adult.trim.cleaner$MSW05_Family[taxon.data.adult.trim.cleaner$scientificName == "Peromyscus sp."] <- "Cricetidae"
taxon.data.adult.trim.cleaner$MSW05_Genus[taxon.data.adult.trim.cleaner$scientificName == "Peromyscus sp."] <- "Peromyscus"

taxon.data.adult.trim.cleaner$MSW05_Order[taxon.data.adult.trim.cleaner$scientificName == "Urocitellus parryii"] <- "Rodentia"
taxon.data.adult.trim.cleaner$MSW05_Family[taxon.data.adult.trim.cleaner$scientificName == "Urocitellus parryii"] <- "Sciuridae"
taxon.data.adult.trim.cleaner$MSW05_Genus[taxon.data.adult.trim.cleaner$scientificName == "Urocitellus parryii"] <- "Urocitellus"

#recount for sample size to 10 for genus, family, and order
#genus
data.genus_stats <- taxon.data.adult.trim.cleaner %>%
  group_by(MSW05_Genus) %>%
  dplyr::summarise(counts = n())
#49 genera

keep.genus <- data.genus_stats$MSW05_Genus[data.genus_stats$counts >= 10] #29 genera
data.genus.10 <- taxon.data.adult.trim.cleaner[taxon.data.adult.trim.cleaner$MSW05_Genus %in% keep.genus,]

#family
data.family_stats <- taxon.data.adult.trim.cleaner %>%
  group_by(MSW05_Family) %>%
  dplyr::summarise(counts = n()) #19 families

keep.family <- data.family_stats$MSW05_Family[data.family_stats$counts >= 10] #12 families
data.family.10 <- taxon.data.adult.trim.cleaner[taxon.data.adult.trim.cleaner$MSW05_Family %in% keep.family,]

#order
data.order_stats <- taxon.data.adult.trim.cleaner %>%
  group_by(MSW05_Order) %>%
  dplyr::summarise(counts = n()) #7 orders

keep.order <- data.order_stats$MSW05_Order[data.order_stats$counts >= 10] #4 orders
data.order.10 <- taxon.data.adult.trim.cleaner[taxon.data.adult.trim.cleaner$MSW05_Order %in% keep.order,]

#genus
gn.models <- unique(data.genus.10$MSW05_Genus)
model.results.genus <- data.frame()
for(i in 1:length(gn.models)){
  sub.data <- as.data.frame(data.genus.10[data.genus.10$MSW05_Genus == gn.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$total.length), na.action=na.exclude)
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
  model.results.genus <- rbind(sub, model.results.genus)
}

#write.csv(model.results.genus, "model.results.genus.csv")

#family
fm.models <- unique(data.family.10$MSW05_Family)
model.results.family <- data.frame()
for(i in 1:length(fm.models)){
  sub.data <- as.data.frame(data.family.10[data.family.10$MSW05_Family == fm.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$total.length), na.action=na.exclude)
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
  model.results.family <- rbind(sub, model.results.family)
}

#write.csv(model.results.family, "model.results.family.csv")

#order
or.models <- unique(data.order.10$MSW05_Order)
model.results.order <- data.frame()
for(i in 1:length(or.models)){
  sub.data <- as.data.frame(data.order.10[data.order.10$MSW05_Order == or.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$total.length), na.action=na.exclude)
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
  model.results.order <- rbind(sub, model.results.order)
}

#write.csv(model.results.order, "model.results.order.csv")

#everything #2383 obs
model <- lm(log10(data.adult.trim.cleaner$mass) ~ log10(data.adult.trim.cleaner$total.length), na.action=na.exclude)
sum.model <- summary(model)
everything.model.results <- data.frame(binomial = sub.data$scientificName[1],
                                       intercept = model$coefficients[[1]],slope = model$coefficients[[2]],
                                       resid.std.err = sum.model$sigma,
                                       df = max(sum.model$df),
                                       std.err.slope =  sum.model$coefficients[4],
                                       std.err.intercept = sum.model$coefficients[3],
                                       r.squared = sum.model$r.squared,
                                       sample.size = length(sub.data$mass))
#write.csv(everything.model.results, "everything.model.results.csv")

#show skeletal v vertnet

##THOUGHT: exclude juvs, keep everything else,and then do three sigma

#plots!
#1. show confidence in line as function of sample size
con.genus.sample <- lm(model.results.genus$std.err.slope ~ model.results.genus$sample.size)
con.family.sample <- lm(model.results.family$std.err.slope ~ model.results.family$sample.size)
con.order.sample <- lm(model.results.order$std.err.slope ~ model.results.order$sample.size)
sum.con.sample.species <- summary(con.species.sample)
sum.con.sample.genus <- summary(con.genus.sample)
sum.con.sample.family <- summary(con.family.sample)
sum.con.sample.order <- summary(con.order.sample)

con.samples.models <- list(model.results.species, model.results.genus, model.results.family, model.results.order)
model.names <- c("model.results.species", "model.results.genus", "model.results.family", "model.results.order")

con.sample.results <- data.frame()
for(i in 1:length(model.names)){
  model <- lm(con.samples.models[[i]][6] ~ con.samples.models$sample.size[[i]][9])
  sum.model <- summary(model)
  sub <- data.frame(binomial = model.names[i],
                                   intercept = model$coefficients[[1]],
                                   slope = model$coefficients[[2]],
                                   resid.std.err = sum.model$sigma,
                                   df = max(sum.model$df),
                                   std.err.slope =  sum.model$coefficients[4],
                                   std.err.intercept = sum.model$coefficients[3],
                                   r.squared = sum.model$r.squared)
  con.sample.results <- rbind(con.sample.results, sub)
}



plot(model.results$std.err.slope ~ model.results$df) #make nicer





#Q3: or other paper compare to Scotty Dog Book



#Q4



# transfer function
# log linear model; log10 or natural log
clean.length.mass$total.length <- as.numeric(clean.length.mass$total.length)
clean.length.mass$mass <- as.numeric(clean.length.mass$mass)
model <- lm(log10(clean.length.mass$total.length) ~ log10(clean.length.mass$mass))
#use fitted models for the prediction
#how uncertainty in line is distributed with use many individuals v. many individuals to make this prediction
#within and across species regression; able to get precise line (average for that group of species)
#can get prediction interval (and predicted values)
#because in log-log space, not symmetrical around the line
#take some case studies in scotty dog book and compare (look at sampling and compare)
#estimate and the std err or; residual error on df

#std.err of slope
#uncertainty in intercept (really, at center of the line; y mean)
#s(y.x) = error uncertainty in residuals; amount of variance in y that is leftover after the regression

#mean and std of slope and intecept and resids
#reconstruct regression uncertainty

# case 1: vertnet; have diff species and individ; can run regression with a large sample size = Best Practice
# also do best practices for other ones (tell parameters about how they'll be constrained)
# case 2: individ representing each species; can take vertnet data and run random replicates and run regression and compare dist of slopes to actual slope and look at uncertainty in lines to see how much it varies (how much less info do you have?)
# case 3: avg. all individ in species and run regression across averages; can do this with vertnet (avg. mass and length) and tell us what that line looks like compared to when have all individ data; and apparent uncertainty
# in the ms from Ray & Juha: they say that this approach is the best (#3); they claim you'd get too much noise because body mass varies so much throughout the year
# in discussion: plant a flag that we're going to look at the effect of intra and inter-annual cyclicity on body masses
# also call to action to get measurements of modern specimens that have mass

#for this paper just show how; and that it can be applied to other things; leave case 2 & 3 for another paper
#include Ray & Juha stuff in discussion; use reported masses adn see which regressions fit best; and that we need big mammals masses
#show whether or not our masses line up to masses in panthera (fall w/in 2 sigma of means of vertnet data); if unbiased random sample then 95% of them should fall within 2 sigmas

#discussion header: alternative approaches (Ray's data); applications (case 2 & 3; juha study w/ cougar data)
#subset cougar to intact masses and see which it's along

#vertnet: everything; family; how different are the slopes of the lines from the different families
# can do an anova (all slopes come from the same parent distribution)

# ggplot(clean.length.mass) + 
#   facet_wrap(~ scientificName,  ncol = 2) +
#   geom_point(aes(x = log10(clean.length.mass$mass), y = clean.length.mass$total.length)) + 
#   geom_smooth(aes(x = log10(clean.length.mass$mass), y = clean.length.mass$total.length), method = "lm") + 
#   ggtitle(~ scientificName) +
#   scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
#   scale_y_continuous(name = "Total Length (cm)")


#translating between models-------------
library(tidyr)

futres <- read.csv("data.for.analyses.csv", header = TRUE, stringsAsFactors = FALSE)
#futres[futres=="--"]<-NA
#futres[futres==""]<-NA
#futres$Total.Fresh.Weight..g. <- as.numeric(futres$Total.Fresh.Weight..g.)  
#futres$HF..mm...Hind.Foot.Length. <- as.numeric(futres$HF..mm...Hind.Foot.Length.)
#futres$TL..mm...Total.Length. <- as.numeric(futres$TL..mm...Total.Length.) 

#Subsetting data
data.adult.trim.clean <- futres[!is.na(futres$mass),]
#make sure to only use records for which total.length.status == "GOOD" & total.length.units == "mm" to subset it some more. head-body-length was calculated for all records, regardless of how good the data is for total body length
data.adult.trim.clean <- subset(data.adult.trim.clean, total.length.status == "GOOD" & total.length.units == "mm")
data.adult.trim.clean <- data.adult.trim.clean[!is.na(data.adult.trim.clean$head.body.length),]
data.adult.trim.clean <- subset(data.adult.trim.clean, mass!=0)

#Code for plotting Spermophilus beecheyi
test2 <- subset(data.adult.trim.clean, MSW05_Genus== "Spermophilus")
test1 <- subset(test2, MSW05_Species== "beecheyi")
test1 <- test1[!is.na(test1$tooth.row),]
#no juveniles so that is good
unique(test1$lifeStage)
#outliers?
unique(test1$tooth.row)
  
#Now we want to see if translating between the models is fruitful. The first step is to predict total length from tooth row length
  #predict function works better if the naming scheme inside the lm model is simple. It will automatically log the values.
  #toothrow versus total length
  y <- test1$head.body.length
  x <- test1$tooth.row
  model1 <- lm(log10(y) ~ log10(x), na.action=na.exclude)
  
  toothpredict <- data.frame(x = test1$tooth.row)
  p1 <- data.frame(predict(model1, toothpredict, se.fit = TRUE))
  toothpredict$fit1 <- p1$fit
  toothpredict$se <- p1$se.fit
  
  #plotting this relationship here
  sum.model1 <- summary(model1)
  sub1 <- data.frame(Genus = test1$MSW05_Genus[1],
                     Species = test1$MSW05_Species[1],
                     comparison = ("head.body.length/tooth.row"),
                     intercept = model1$coefficients[[1]],
                     slope = model1$coefficients[[2]],
                     resid.std.err = sum.model1$sigma,
                     df = max(sum.model1$df),
                     std.err.slope =  sum.model1$coefficients[4],
                     std.err.intercept = sum.model1$coefficients[3],
                     r.squared = sum.model1$r.squared,
                     sample.size = length(test1$mass))
  
  p = ggplot(data = test1) + 
    geom_point(aes(x = log10(tooth.row), y = log10(head.body.length))) +
    geom_smooth(aes(x = log10(tooth.row), y = log10(head.body.length)),method = "lm", color = "slateblue4")+
    ggtitle("Spermophilus beecheyi") +
    theme(plot.title = element_text(face = "italic"))+
    ylab(expression(log[10]~Head~Body~Length~(mm))) +
    xlab(expression(log[10]~Toothrow~Length~(mm)))
  
  ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_Headbodylength.png"), width = 14, height = 10, units = "cm")
  
  #Now to push the toothrow points through the second regression. I am already in log 10 space with the predictions.
  #need to sample the whole genus for this data
  #some masses=0? below regression won't work if anything=0
  y2 <- log10(test2$mass)
  x2 <- log10(test2$head.body.length)
  model2 <- lm(y2 ~ x2, na.action=na.exclude)
  sum.model2 <- summary(model2)
  
  #remember the variable has to have the same name for the predict function to work so we have to name it x2
   fit1<- data.frame(x2 = toothpredict$fit1)
  p2 <- data.frame(predict(model2, newdata=fit1, se.fit = TRUE))
  toothpredict$fit2 <- p2$fit
  toothpredict$se2 <- p2$se.fit
  
  #plotting this relationship
  sub2 <- data.frame(Genus = test2$MSW05_Genus[1],
                     Species = "Whole genus",
                     comparison = ("mass/head.body.length"),
                     intercept = model2$coefficients[[1]],
                     slope = model2$coefficients[[2]],
                     resid.std.err = sum.model2$sigma,
                     df = max(sum.model2$df),
                     std.err.slope =  sum.model2$coefficients[4],
                     std.err.intercept = sum.model2$coefficients[3],
                     r.squared = sum.model2$r.squared,
                     sample.size = length(test2$mass))
  
  p = ggplot(data = test2) + 
    geom_point(aes(x = log10(head.body.length), y = log10(mass))) +
    geom_smooth(aes(x = log10(head.body.length), y = log10(mass)),method = "lm", color = "slateblue4")+
    ggtitle("Spermophilus beecheyi") +
    theme(plot.title = element_text(face = "italic"))+
    ylab(expression(log[10]~Body~Mass~(g))) +
    xlab(expression(log[10]~Head~Body~Length~(mm)))
  
 #seems like there is a outlier on this plot.
  ggsave(p, file=paste0("plot_Spermophilus beecheyi_Headbodylength_mass.png"), width = 14, height = 10, units = "cm")
  
  model.Spermophilus.beecheyi <- rbind(sub1, sub2)
  
  #gaussian error propogation
  toothpredict$GauTRL <- sqrt((model2$coefficients[[2]] * toothpredict$se)^2)
  
  #summing together the error from the gaussian propogation and the error from pushing the points through
  toothpredict$sumerror <- sqrt((toothpredict$GauTRL)^2 + (toothpredict$se2 )^2)
  
  #compare to the error in the relationship between tooth row and mass
  model3 <- lm(log10(test1$mass) ~ log10(test1$tooth.row), na.action=na.exclude)
  sum.model3 <- summary(model3)
  toothpredict2 <- data.frame(tooth.row = test1$tooth.row)
  p3 <- data.frame(predict(model3, toothpredict2, se.fit = TRUE))
  toothpredict$fit3 <- p3$fit
  toothpredict$se3 <- p3$se.fit
  
  #plot this relationship
  sub3 <- data.frame(Genus = test1$MSW05_Genus[1],
                     Species = test1$MSW05_Species[1],
                     comparison = ("mass/tooth.row"),
                     intercept = model3$coefficients[[1]],
                     slope = model3$coefficients[[2]],
                     resid.std.err = sum.model3$sigma,
                     df = max(sum.model3$df),
                     std.err.slope =  sum.model3$coefficients[4],
                     std.err.intercept = sum.model3$coefficients[3],
                     r.squared = sum.model3$r.squared,
                     sample.size = length(test1$mass))
  
  p = ggplot(data = test1) + 
    geom_point(aes(x = log10(tooth.row), y = log10(mass))) +
    geom_smooth(aes(x = log10(tooth.row), y = log10(mass)),method = "lm", color = "slateblue4")+
    ggtitle("Spermophilus beecheyi") +
    theme(plot.title = element_text(face = "italic"))+
    ylab(expression(log[10]~Body~Mass~(g))) +
    xlab(expression(log[10]~Toothrow~Length~(mm)))
  
  #seems like there is a outlier on this plot.
  ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_mass.png"), width = 14, height = 10, units = "cm")
  
  model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, sub3)
  
  #write.csv(model.Spermophilus.beecheyi, file= "model.results.whole.genus.Spermophilus.beecheyi.csv")
  
  #write.csv(toothpredict, file= "error.propogation.Spermophilus.beecheyi.csv")
  
#some questions: is it ok to be working in log log space this whole time? DO we want to transform them at the end of the analysis.
  

#Now for the deer species------
  test2 <- subset(data.adult.trim.clean, MSW05_Genus== "Odocoileus")
  test1 <- subset(test2, MSW05_Species== "virginianus")
  test1 <- test1[!is.na(test1$astragalus.length),]
  #no juveniles so that is good
  unique(test1$lifeStage)
  #outliers?
  unique(test1$tooth.row)
  
  
  test3 <- subset(data.adult.trim.clean, scientificName== "Odocoileus virginianus")
  #any juneniles? There are some young adults but no juveniles
  unique(test$lifeStage)
  #seems to be no foot data?
  unique(test$HF..mm...Hind.Foot.Length.)
  #seems to be no astragalus data either
  unique(test$Astragalus.Length)
  
  model <- lm(log10(test$mass) ~ log10(test$HF..mm...Hind.Foot.Length.), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = test$scientificName[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(test$mass))
  
  #ggtitle doesn't seem to be doing anything because it is not connected to p. When I do connect it scale_x and scale y takes away the axis tick marks. 
  p = ggplot(data = test) + 
    geom_point(aes(x = log10(Total.Fresh.Weight..g.), y = log10(HF..mm...Hind.Foot.Length.))) +
    geom_smooth(aes(x = log10(Total.Fresh.Weight..g.), y = log10(HF..mm...Hind.Foot.Length.)),
                method = "lm", color = "slateblue4")
  ggtitle("Odocoileus virginianus") +
    theme(plot.title = element_text(face = "italic"))+
    xlab(expression(log[10]~Body~Mass~(g))) +
    ylab(expression(log[10]~Hindfoot~Length~(mm))) + 
    ggsave(p, file=paste0("plot_Odocoileus virginianus_hindfoot2.png"), width = 14, height = 10, units = "cm")
  
  
  
  
  #the below code predicts the predictions and makes the statistically bankrupt plot. WE can delete
  newdata<- data.frame(x = toothpredict$lwr1)
  p2 <- data.frame(predict(model2, newdata, interval="predict"))
  toothpredict$lwr1fit <- p2$fit
  toothpredict$lwr1lwr <- p2$lwr
  toothpredict$lwr1upr <- p2$upr
  
  newdata<- data.frame(x = toothpredict$upr1)
  p2 <- data.frame(predict(model2, newdata, interval="predict"))
  toothpredict$upr1fit <- p2$fit
  toothpredict$upr1lwr <- p2$lwr
  toothpredict$upr1upr <- p2$upr
  
  toothpredict$TL..mm...Total.Length. <- test1$TL..mm...Total.Length.
  toothpredict$Total.Fresh.Weight..g. <- test1$Total.Fresh.Weight..g.
  
  p = ggplot(data = toothpredict) + 
    geom_point(aes(x = log10(TL..mm...Total.Length.), y = log10(Total.Fresh.Weight..g.))) +
    geom_smooth(aes(x = log10(TL..mm...Total.Length.), y = log10(Total.Fresh.Weight..g.)),
                method = "lm", color = "slateblue4")+
    ggtitle("Spermophilus beecheyi") +
    theme(plot.title = element_text(face = "italic"))+
    ylab(expression(log[10]~Body~Mass~(g))) +
    xlab(expression(log[10]~Total~Length~(mm))) +
    geom_point(aes(x = fit1, y = fit2), color= "green")+
    geom_point(aes(x = lwr1, y = lwr1fit), color= "orange")+
    geom_point(aes(x = upr1, y = upr1fit), color= "orange")+
    geom_line(aes(x = lwr1, y = lwr1lwr), linetype = "dashed", color = "red")+
    geom_line(aes(x = lwr1, y = lwr1upr), linetype = "dashed", color = "red")+
    geom_line(aes(x = upr1, y = upr1lwr), linetype = "dashed", color = "red")+
    geom_line(aes(x = upr1, y = upr1upr), linetype = "dashed", color = "red")
  
  ggsave(p, file=paste0("plot_Spermophilus beecheyi_totallength.png"), width = 14, height = 10, units = "cm")


  
    
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






