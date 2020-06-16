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
panthera <- read.csv("https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv", header = TRUE, stringsAsFactors = FALSE)

#kitty_deer <- read.csv("EAP Florida Modern Deer Measurements_FORFUTRES_1_23_2020.csv", header = TRUE, stringsAsFactors = FALSE)
#blois_ground.squirrel <- read.csv("J.Biogeo.2008.AllData.Final.csv", header = TRUE, stringsAsFactors = FALSE)
#amelia_impala <- read.csv("Extant Aepyceros database_updated 11_2016.csv", header = TRUE, stringsAsFactors = FALSE)
#bernor_equid <- read.csv("ToFuTRESVER_12_4_16_2020_REV_19.csv", header = TRUE, stringsAsFactors = FALSE)
#cougar_OR <- read.csv("1987-2019 Cougar Weight-Length Public Request.csv", header = TRUE, stringsAsFactors = FALSE)

futres <- read.csv("https://de.cyverse.org/dl/d/888175F3-F04D-4AB3-AB1C-FB7F9447C3ED/futres.csv", header = TRUE, stringsAsFactors = FALSE)
#about futres data:
#has various terms for adult and juvenile
#currently aligned manually


## VertNet data
bat_mass <- read.csv("https://de.cyverse.org/dl/d/2A542CDF-BDED-4486-AB15-445B53F80F08/vertnet_bats_body_mass_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
bat_length <- read.csv("https://de.cyverse.org/dl/d/896A54B4-1E52-4976-95AB-71449384B3A4/vertnet_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#mamm has no bats
mamm_mass <- read.csv("https://de.cyverse.org/dl/d/EF537422-2246-4B25-A9BC-D8259C78BFA2/vertnet_no_bats_body_mass_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
mamm_length <- read.csv("https://de.cyverse.org/dl/d/DA7E36EF-0008-4DED-A49C-C7DCCC71E98C/vertnet_no_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)

#about VertNet data:
#has adult, juvenile, and NA for lifestage
#has mass in different units, sometimes inferreed

#bernor_equid$binomial <- paste(bernor_equid$GENUS, bernor_equid$SPECIES)
#deal with this once mapped

## need to get rid of subspecies
bat_mass$scientificName <- word(bat_mass$scientificname, 1,2, sep = " ") #435 unique spp
mamm_mass$scientificName <- word(mamm_mass$scientificname, 1,2, sep = " ") #1190 unique spp
#mamm_mass has some weird spp. names: e.g.,: (new SW
#will deal with later

# group lifeStages
futres$lifeStage[futres$lifeStage == "young adult" | futres$lifeStage == "Adult" | futres$lifeStage == "Prime Adult" | futres$lifeStage == "adult"] <- "Adult"
futres$lifeStage[futres$lifeStage == "Juvenile" | futres$lifeStage == "juvenile"] <- "Juvenile"

#combine VertNet datasets
vertnet.mass <- rbind(bat_mass, mamm_mass)

#convert to g for mammal & bat mass
vertnet.mass$body_mass_1.value <- as.numeric(vertnet.mass$body_mass_1.value)

vertnet.mass_g <- subset(vertnet.mass, vertnet.mass$body_mass_1.units == "g" | vertnet.mass$body_mass_1.units == "Grams")
vertnet.mass_g$body_mass_1.units <- "g"

vertnet.mass_kg <- subset(vertnet.mass, vertnet.mass$body_mass_1.units == "kg")
vertnet.mass_kg$body_mass_1.value <- vertnet.mass_kg$body_mass_1.value / 1000
vertnet.mass_kg$body_mass_1.units <- "g"

vertnet.mass_lb <- subset(vertnet.mass, vertnet.mass$body_mass_1.units == "lb" | vertnet.mass$body_mass_1.units == "lbs" | vertnet.mass$body_mass_1.units == "pounds")
# 1 lb = 453.592g
vertnet.mass_lb$body_mass_1.value <- vertnet.mass_lb$body_mass_1.value * 453.592
vertnet.mass_lb$body_mass_1.units <- "g"

vertnet.mass_oz <- subset(vertnet.mass, vertnet.mass$body_mass_1.units == "oz")
#1 oz = 28.3495 g
vertnet.mass_oz$body_mass_1.value <- vertnet.mass_oz$body_mass_1.value * 28.3495
vertnet.mass_oz$body_mass_1.units <- "g"

vertnet.mass_allg <- rbind(vertnet.mass_g, vertnet.mass_kg, vertnet.mass_lb, vertnet.mass_oz)

#deal with these later
#vertnet.mass_mix <- subset(vertnet.mass, vertnet.mass$body_mass_1.units == "['lb', 'oz']" | vertnet.mass$body_mass_1.units == "['lbs', 'oz']")

futres.sub.mass <- futres %>%
  select(scientificName, mass = Total.Fresh.Weight..g., lifeStage = lifeStage, sex = sex)
vertnet.sub.mass <- vertnet.mass_allg %>%
  select(scientificName = scientificname, mass = body_mass_1.value, lifeStage = lifestage_cor, sex = sex)

data.mass <- rbind(futres.sub.mass, vertnet.sub.mass)
data.mass$mass <- as.numeric(data.mass$mass)
length(unique(data.mass$scientificName)) #876

data.mass_stats <- data.mass %>%
  group_by(scientificName, lifeStage) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE))


# remove samples that are 3 sigmas outside of distribution


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








