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
data.clean <- read.csv("https://de.cyverse.org/dl/d/B5D15FDC-B0ED-4F78-B733-2658A4502AE9/clean.data.csv", header = TRUE, stringsAsFactors = FALSE)

data.adult <- data.clean[data.clean$lifeStage == "Adult",]
length(unique(data.adult$scientificName)) #327 spp

data.adult$mass <- as.numeric(data.adult$mass)
data.adult$total.length <- as.numeric(data.adult$total.length)
data.adult$hindfoot.length <- as.numeric(data.adult$hindfoot.length)
data.adult$Calcaneus.GB <- as.numeric(data.adult$Calcaneus.GB)
data.adult$Calcaneus.GL <- as.numeric(data.adult$Calcaneus.GL)
data.adult$tooth.row <- as.numeric(data.adult$tooth.row)

data.adult_stats <- data.adult %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   avg.mass = 
                     mean(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   min.length = min(total.length, na.rm = TRUE),
                   max.length = max(total.length, na.rm = TRUE),
                   avg.length = mean(total.length, na.rm = TRUE))

keep.adult <- data.adult_stats$scientificName[data.adult_stats$sample.size >= 10]
data.adult.10 <- data.adult[data.adult$scientificName %in% keep.adult,]
length(unique(data.adult.10$scientificName)) #118

# remove samples that are 3 sigmas outside of distribution
# find 3xsigma
data.adult.sigma <- data.adult.10 %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(),
    mass.sigma = sd(mass, na.rm = TRUE),
    mass.upper.limit = 3*mass.sigma,
    mass.lower.limt = -3*mass.sigma,
    length.sigma = sd(total.length, na.rm = TRUE),
    length.upper.limit = 3*length.sigma,
    length.lower.limt = -3*length.sigma) %>%
    as.data.frame()

# get rid of outliers
sp.adult <- unique(data.adult.10$scientificName)
data.adult.trim <- data.frame()
for(i in 1:length(sp.adult)){
  data.sub <- subset(data.adult.10, data.adult.10$scientificName == sp.adult[i])
  sigma.sub <- subset(data.adult.sigma, data.adult.sigma$scientificName == sp.adult[i])
  for(j in 1:length(data.sub$scientificName)){
    if(isTRUE(data.sub$mass[j] < sigma.sub$mass.lower.limit | data.sub$mass[j] > sigma.sub$mass.upper.limit)){
      data.sub$mass[j] <- "oulier"
    }
    else if(isTRUE(data.sub$total.length[j] < sigma.sub$length.lower.limit | data.sub$total.length[j] > sigma.sub$mass.upper.limit)){
      data.sub$total.length[j] <- "outlier"
    }
    else{
      next
    }
  }
  data.adult.trim <- rbind(data.adult.trim, data.sub)
}

data.adult.trim_stats <- data.adult.trim %>%
  group_by(scientificName) %>%
  dplyr::summarise(counts = n()) %>%
  as.data.frame()

keep.trim <- data.adult.trim_stats$scientificName[data.adult.trim_stats$counts >= 10]
data.adult.trim.10 <- data.adult.trim[data.adult.trim$scientificName %in% keep.trim,]
length(unique(data.adult.trim.10$scientificName)) #117

#Q1 compare to pantheria 
sp.data <- unique(data.adult.trim.10$scientificName) #117 spp
pan <- pan[pan$MSW05_Binomial %in% sp.data,] #105 spp
pan.sub <- subset(pan, select = c("MSW05_Binomial", "X5.1_AdultBodyMass_g"))
pan.sub.clean <- pan.sub[!is.na(pan.sub$X5.1_AdultBodyMass_g),] #98 sp

pan.data.adult <- merge(pan.sub.clean, data.adult.trim.10, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = FALSE, all.x = FALSE)
#98 sp

pan.data.adult.clean <- pan.data.adult[!is.na(pan.data.adult$mass),] #81 sp

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
  filter(sample.size >= 10)
#write.csv(pan.data.adult_stats.10, "pan.results.csv")

keep.pan <- pan.data.adult_stats.10$MSW05_Binomial[pan.data.adult_stats.10 >= 10]
pan.data.adult.clean.10 <- pan.data.adult.clean[pan.data.adult.clean$MSW05_Binomial %in% keep.pan,]
#59 sp

#13 out of 59 are not significant; 46 that are different; 22% are ok, 78%of them are diff
#if assume VertNet is randomly collected, it is a better representation than pan
#but if biased, then a problem (e.g., during a season, sex); or newer data
#is the difference more pronounced in smaller or larger mammals?

# FIGURE: body mass distributions w/ line from PanTHERIA

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
data.adult.trim.clean <- data.adult.trim.10[!is.na(data.adult.trim.10$mass),]
data.adult.trim.cleaner <- data.adult.trim.clean[!is.na(data.adult.trim.clean$total.length),]
#73 spp

#recount sample sizes
data.adult.trim.cleaner_stats <- data.adult.trim.cleaner %>%
  group_by(scientificName) %>%
  dplyr::summarise(counts = n())

keep.adult.trim.clean <- data.adult.trim.cleaner_stats$scientificName[data.adult.trim.cleaner_stats$counts >= 10]
data.adult.trim.cleaner.10 <- data.adult.trim.cleaner[data.adult.trim.cleaner$scientificName %in% keep.adult.trim.clean,]
#40 sp

sp.models <- unique(data.adult.trim.cleaner.10$scientificName)
model.results <- data.frame()
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
                    r.squared = sum.model$r.squared)
  model.results <- rbind(sub, model.results)
}

#write.csv(model.results, "model.results.csv")

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
                    r.squared = sum.model$r.squared)
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
                    r.squared = sum.model$r.squared)
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
                    r.squared = sum.model$r.squared)
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
                                       r.squared = sum.model$r.squared)
#write.csv(everything.model.results, "everything.model.results.csv")

#show skeletal v vertnet

##THOUGHT: exclude juvs, keep everything else,and then do three sigma

#plots!
#1. show confidence in line as function of sample size
plot(model.results$std.err.slope ~ model.results$df) #make nicer




#Q3: or other paper compare to Scotty Dog Book






#need to figure out how to get this to go clean
ggplot(melt(data.adult.10), aes(x = log10(value))) + 
  facet_wrap(~ scientificName,  ncol = 2) +
  ggtitle(~ scientificName) +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Counts")

stats.mass <- clean.masses.10 %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(mean.mass = mean(mass), median.mass = median(mass),
                   min.mass = min(mass), max.mass = max(mass))












# remove species above the +3.dev and -3 dev
sp.adult <- unique(data.adults.10$scientificName)
data.mass.adults.trim <- data.frame()
for(i in 1:length(sp.adult)){
  data.sub <- subset(data.adults.10, data.adults.10$scientificName == sp.adult[i])
  sigma.sub <- subset(data.adult.sigma, data.adult.sigma$scientificName == sp.adult[i])
  for(j in 1:length(data.sub$scientificName)){
    if(isTRUE(data.sub$mass[j] < sigma.sub$mass.lower.limit | data.sub$mass[j] > sigma.sub$mass.upper.limit)){
      data.sub$mass[j] <- "oulier"
    }
  else if(isTRUE(data.sub$total.length[j] < sigma.sub$length.lower.limit | data.sub2$total.length[j] > sigma.sub$mass.upper.limit)){
    data.sub$total.length[j] <- "outlier"
  }
else{
  next
}
  }
  data.mass.adults.trim <- rbind(data.mass.adults.trim, data.sub)
}

length(unique(data.mass.adults.trim$scientificName)) #44 species

## do the same for LENGTH now
## need to get rid of subspecies
bat_length$scientificName <- word(bat_length$scientificname, 1,2, sep = " ") #435 unique spp
mamm_length$scientificName <- word(mamm_length$scientificname, 1,2, sep = " ") #1190 unique spp

#combine VertNet datasets
vertnet.length <- rbind(bat_length, mamm_length)



futres.sub.length <- futres %>%
  select(scientificName, length = TL..mm...Total.Length., lifeStage = lifeStage, sex = sex)
vertnet.sub.length <- vertnet.length_allmm %>%
  select(scientificName = scientificname, length = total_length_1.value, lifeStage = lifestage_cor, sex = sex)

data.length <- rbind(futres.sub.length, vertnet.sub.length)
data.length$length <- as.numeric(data.length$length)
length(unique(data.mass$scientificName)) #1284

data.length.adults <- subset(data.length, data.length$lifeStage == "Adult")
length(unique(data.length.adults$scientificName)) #333

data.length.adults_stats <- data.length.adults %>%
  group_by(scientificName, lifeStage) %>%
  dplyr::summarise(sample.size = n(), 
                   min.length = min(length, na.rm = TRUE),
                   max.length  = max(length, na.rm = TRUE))

keep.length <- data.length.adults_stats$scientificName[data.length.adults_stats$sample.size >= 10]
data.length.adults.10 <- data.length.adults[data.length.adults$scientificName %in% keep.length,]
length(unique(data.length.adults.10$scientificName)) #75

# remove samples that are 3 sigmas outside of distribution
# find 3xsigma
data.length.adults.sigma <- data.length.adults.10 %>%
  group_by(scientificName) %>%
  dplyr::summarise(sigma = sd(length, na.rm = TRUE),
                   sample.size = n(),
                   upper.limit = 3*sigma,
                   lower.limit = -3*sigma)

# remove species above the +three.dev and -three.dev
sp.adult <- unique(data.length.adults.10$scientificName)
data.length.adults.trim <- data.frame()
for(i in 1:length(sp.adult)){
  sub <- subset(data.length.adults.10, data.length.adults.10$scientificName == sp.adult[i])
  sub2 <- subset(sub, sub$length < data.length.adults.sigma$three.dev[data.length.adults.sigma$scientificName == sp.adult[i]] & sub$length > -1*(data.length.adults.sigma$three.dev[data.length.adults.sigma$scientificName == sp.adult[i]]))
  data.length.adults.trim <- rbind(data.length.adults.trim, sub2)
}

length(unique(data.length.adults.trim$scientificName)) #7 species

## Q1. How do distributions compare to other, recorded species' averages or ranges?
# create loop of histograms and insert pan line

#align datasets
sp.futres <- unique(futres$scientificName)

#subset panthera to only include species that are in futres
pan <- panthera[(panthera$MSW05_Binomial %in% sp.futres),]

#weight
futres.mass <- futres.adult %>%
  select(scientificName, mass = Total.Fresh.Weight..g.)
bat.mass <- bat.adult %>%
  select(scientificName, mass = body_mass_1.value)
mamm.mass <- mamm.adult %>%
  select(scientificName, mass = body_mass_1.value)

futres.all.mass <- rbind(futres.mass, bat.mass, mamm.mass)
futres.all.mass$mass <- as.numeric(futres.all.mass$mass)

# clean up data
clean.masses <- futres.mass %>%
  na.omit()

counts.mass<- clean.masses %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(n = length(mass)) 

omit.mass <- counts.mass$scientificName[counts.mass$n < 10]

clean.masses.10 <- clean.masses[!(clean.masses$scientificName %in% omit.mass),]
length(unique(clean.masses$scientificName)) #98 spp

#need to figure out how to get this to go clean
clean.masses$mass <- as.numeric(clean.masses$mass)
clean.masses.10$mass <- as.numeric(clean.masses.10$mass)
ggplot(melt(clean.masses), aes(x = log10(value))) + 
  facet_wrap(~ scientificName,  ncol = 2) +
  ggtitle(~ scientificName) +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Counts")

stats.mass <- clean.masses.10 %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(mean.mass = mean(mass), median.mass = median(mass),
            min.mass = min(mass), max.mass = max(mass))

# length by mass regression
futres.length.mass <- futres.adult %>%
  select(scientificName, mass = Total.Fresh.Weight..g., total.length = TL..mm...Total.Length.)
bat.length.mass <- bat.adult %>%
  select(scientificName, mass = body_mass_1.value, total.length = total_length_1.value)
mamm.length.mass <- mamm.adult %>%
  select(scientificName, mass = body_mass_1.value, total.length = total_length_1.value)

futres.all.length.mass <- rbind(futres.length.mass, bat.length.mass, mamm.length.mass)
futres.all.length.mass$total.length <- as.numeric(futres.all.length.mass$total.length)
futres.all.length.mass$mass <- as.numeric(futres.all.length.mass$mass)


# clean up data
clean.length.mass <- futres.length.mass %>%
  na.omit()

counts <- clean.length.mass %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(n.mass = length(mass), n.length = length(total.length)) 

omit.length.mass <- counts$scientificName[counts$n.mass < 10 | counts$n.length < 10]

clean.length.mass.10 <- clean.length.mass[!(clean.length.mass$scientificName %in% omit.length.mass),] #103 species

length(unique(clean.length.mass.10$scientificName)) #94

models <- plyr::dlply(clean.length.mass, "scientificName", function(df) 
  lm(total.length ~ mass, data = df))
plyr::ldply(models, coef)
plyr::l_ply(models, summary, .print = TRUE)

ggplot(clean.length.mass, aes(x = log10(mass), y = log10(total.length), color = scientificName)) + 
  geom_point() + 
  geom_smooth(method = "lm", #se = FALSE
              alpha = .15, aes(fill = scientificName))

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








