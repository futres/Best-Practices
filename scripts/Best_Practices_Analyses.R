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

##Upload data----
pan <- read.csv("https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("https://de.cyverse.org/dl/d/9A1CB483-4ABF-49F2-9F4C-1201EFA774E6/labeled.clean.data.csv", header = TRUE, stringsAsFactors = FALSE)

##Q1 compare to pantheria----
sp.data <- unique(data$scientificName) #605 spp
pan <- pan[pan$MSW05_Binomial %in% sp.data,] #538 spp
pan.sub <- subset(pan, select = c("MSW05_Order", "MSW05_Family", "MSW05_Genus", "MSW05_Binomial", "X5.1_AdultBodyMass_g"))
pan.sub.clean <- pan.sub[!is.na(pan.sub$X5.1_AdultBodyMass_g),] #474 sp

pan.data.adult <- merge(pan.sub.clean, data, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = FALSE, all.x = FALSE)

#write.csv(pan.data.adult, "data.taxonomy.csv")

pan.data.adult.clean <- pan.data.adult[!is.na(pan.data.adult$mass) & pan.data.adult$mass.status != "outlier",] #489 sp
length(unique(pan.data.adult.clean$MSW05_Binomial)) #344

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
  filter(sample.size >= 10) #298 spp
#write.csv(pan.data.adult_stats.10, "pan.results.csv")

length(pan.data.adult_stats.10$MSW05_Binomial[pan.data.adult_stats.10$mass.diff <= 2]) #67
length(pan.data.adult_stats.10$MSW05_Binomial[pan.data.adult_stats.10$mass.diff > 2]) #231
#22.5% are ok; 77.5% are not

##FIGURE: body mass distributions w/ line from PanTHERIA----

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

##Q2: length v. mass----
#clean data
data.mass <- data[!is.na(data$mass) & data$mass.status != "outlier",]
data.mass.length <- data.mass[!is.na(data.mass$total.length) & data.mass$total.length.status != "outlier",]
data.adult <- data.mass.length[data.mass.length$lifeStage != "Juvenile",]
length(unique(data.adult$scientificName)) #338 sp

##some measurements are zero, oy vey
data.adult.clean <- data.adult[data.adult$mass > 0 & data.adult$total.length > 0,]

#recount sample sizes
data.adult_stats <- data.adult.clean %>%
  group_by(scientificName) %>%
  dplyr::summarise(counts = n())

keep.10 <- data.adult_stats$scientificName[data.adult_stats$counts >= 10] #286 sp
data.adult.10 <- data.adult.clean[data.adult.clean$scientificName %in% keep.10,]
length(unique(data.adult.10$scientificName)) #286 sp


sp.models <- unique(data.adult.10$scientificName)
model.results.species <- data.frame()
for(i in 1:length(sp.models)){
  sub.data <- as.data.frame(data.adult.10[data.adult.10$scientificName == sp.models[i],])
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
  model.results.species <- rbind(model.results.species, sub)
}

#lots of repeats for Neofiber alleni; only 4 unique occurrence ids....why???

#write.csv(model.results.species, "model.results.species.csv")

uniq_species <- unique(data.adult.10$scientificName)
for (i in uniq_species) {
  p = ggplot(data = subset(data.adult.10, scientificName  == i)) + 
    geom_point(aes(x = log10(mass), y = log10(total.length))) +
    geom_smooth(aes(x = log10(mass), y = log10(total.length)),
                method = "lm", color = "slateblue4")
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_log10(name = expression(log[10]~Total~Length~(mm))) + 
  ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}


data.adult.10$infer.type <- rep("", nrow(data.adult.10))
for(i in 1:nrow(data.adult.10)){
  if(isTRUE(data.adult.10$total.length.units.inferred[i] ==" TRUE" | data.adult.10$total.length.units.inferred[i] == "True" | data.adult.10$total.length.units.inferred[i] == "CONVERTED" & data.adult.10$mass.units.inferred[i] == "TRUE" | data.adult.10$mass.units.inferred[i] == "True" | data.adult.10$mass.units.inferred[i] == "CONVERTED")){
    data.adult.10$infer.type[i] <- "both"
  }
  else if(isTRUE(data.adult.10$total.length.units.inferred[i] ==" TRUE" | data.adult.10$total.length.units.inferred[i] == "True" | data.adult.10$total.length.units.inferred[i] == "CONVERTED")){
    data.adult.10$infer.type[i] <- "length"
  }
  else if(isTRUE(data.adult.10$mass.units.inferred[i] == "TRUE" | data.adult.10$mass.units.inferred[i] == "True" | data.adult.10$mass.units.inferred[i] == "CONVERTED")){
    data.adult.10$infer.type[i] <- "mass"
  }
  else{
    data.adult.10$infer.type[i] <- "none"
  }
}

ggplot(data = subset(data.adult.10, scientificName  == "Artibeus jamaicensis")) + 
    geom_point(aes(x = log10(mass), y = log10(total.length), color = infer.type)) +
    geom_smooth(aes(x = log10(mass), y = log10(total.length)),
                method = "lm", color = "slateblue4")+
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_log10(name = expression(log[10]~Total~Length~(mm)))

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
deer <- subset(pan.data.adult, pan.data.adult$MSW05_Binomial == "Odocoileus virginianus")
bats <- subset(pan.data.adult, pan.data.adult$MSW05_Order == "Chiroptera")
bats.clean.mass <- subset(bats, !is.na(bats$mass) & bats$mass != 0)
bats.clean.length <- subset(bats.clean.mass, !is.na(bats.clean.mass$total.length) & bats.clean.mass$total.length != 0)
bats.clean <- bats.clean.length


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








