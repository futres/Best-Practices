## Upload data
panthera <- read.csv("pantheria.csv", header = TRUE, stringsAsFactors = FALSE)
#https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv

#kitty_deer <- read.csv("EAP Florida Modern Deer Measurements_FORFUTRES_1_23_2020.csv", header = TRUE, stringsAsFactors = FALSE)
#blois_ground.squirrel <- read.csv("J.Biogeo.2008.AllData.Final.csv", header = TRUE, stringsAsFactors = FALSE)
#amelia_impala <- read.csv("Extant Aepyceros database_updated 11_2016.csv", header = TRUE, stringsAsFactors = FALSE)
bernor_equid <- read.csv("ToFuTRESVER_12_4_16_2020_REV_19.csv", header = TRUE, stringsAsFactors = FALSE)

futres <- read.csv("futres.csv", header = TRUE, stringsAsFactors = FALSE)
#https://de.cyverse.org/dl/d/888175F3-F04D-4AB3-AB1C-FB7F9447C3ED/futres.csv

bat_mass <- read.csv("vertnet_bats_body_mass_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)
#
bat_length <- read.csv("vertnet_bats_total_len_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)
#
mamm_mass <- read.csv("vertnet_no_bats_body_mass_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)
#
mamm_length <- read.csv("vertnet_no_bats_total_len_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)
#

## Load packages
require(tidyverse)
require(nlme)
require(dplyr)
require(ggplot2)
require(reshape2)
require(plyr)
require(stringr)

## align data sets
#fix binomials
bernor_equid$binomial <- paste(bernor_equid$GENUS, bernor_equid$SPECIES)

#get rid of subspecies in Amelia's dataset
# impala.names <- strsplit(as.character(amelia_impala$Species), " ")
# df <- data.frame(genus = sapply(impala.names, "[", 1), species = sapply(impala.names, "[", 2))
# impala_binomial <- paste(df$genus, df$species)
# impala <- cbind(amelia_impala, impala_binomial)
# impala %>%
#   select(Species, impala_binomial) #check that they are aligned properly

sp.1 <- unique(futres$scientificName)

pan <- panthera[(panthera$MSW05_Binomial %in% sp.1),]

# select out adults only
futres.adult <- dplyr::filter(futres, futres$lifeStage == "Adult" | futres$lifeStage == "Prime Adult" | futres$lifeStage == "young adult" | futres$lifeStage == "adult" | futres$ageValue > 1)
bat.adult <- dplyr::filter(bat_mass, bat_mass$lifestage_cor == "Adult")
mamm.adult <- dplyr::filter(mamm_mass, mamm_mass$lifestage_cor == "Adult")

## need to get rid of subspecies
bat.adult$scientificName <- word(bat.adult$scientificname, 1,2, sep = " ")
mamm.adult$scientificName <- word(mamm.adult$scientificname, 1,2, sep = " ")

## Q1. How do distributions compare to other, recorded species' averages or ranges?
# create loop of histograms and insert pan line

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
clean.masses <- futres.all.mass %>%
  na.omit()

counts.mass <- clean.masses %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(n = length(mass)) 

omit.mass <- counts.mass$scientificName[counts.mass$n < 10]

clean.masses.10 <- clean.masses[!(clean.masses$scientificName %in% omit.mass),]
length(unique(clean.masses$scientificName)) #98 spp

#need to figure out how to get this to go clean
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
clean.length.mass <- futres.all.length.mass %>%
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

# ggplot(clean.length.mass) + 
#   facet_wrap(~ scientificName,  ncol = 2) +
#   geom_point(aes(x = log10(clean.length.mass$mass), y = clean.length.mass$total.length)) + 
#   geom_smooth(aes(x = log10(clean.length.mass$mass), y = clean.length.mass$total.length), method = "lm") + 
#   ggtitle(~ scientificName) +
#   scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
#   scale_y_continuous(name = "Total Length (cm)")








