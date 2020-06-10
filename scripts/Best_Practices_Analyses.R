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
  select(scientificName, mass = body_mass_1.value, lifeStage = lifestage_cor, sex = sex)

data.mass <- rbind(futres.sub.mass, vertnet.sub.mass)
data.mass$mass <- as.numeric(data.mass$mass)
length(unique(data.mass$scientificName)) #876

data.mass_stats <- data.mass %>%
  group_by(scientificName, lifeStage) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE))

#create one specific subset
PEMA <- data.mass[data.mass$scientificName == "Peromyscus maniculatus",]
#have a lot of NAs...

## Method 1: get rid of extreme 5% of adult body masses

# for now adults only
data.mass.adult <- subset(data.mass, data.mass$lifeStage == "Adult")
length(unique(data.mass.adult$scientificName)) #230

# clean up data
#no NAs for mass values
clean.data.mass.adult <- data.mass.adult[!(is.na(data.mass.adult$mass)),]
length(unique(clean.data.mass.adult$scientificName)) #229

#get counts per species
counts.adult.mass <- clean.data.mass.adult %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(n = length(mass)) 

#vector of species with less than ten mass values
omit.adult.mass <- counts.adult.mass$scientificName[counts.adult.mass$n < 10]

#restricting species to only those with at least 10 counts
clean.data.mass.adult.10 <- clean.data.mass.adult[!(clean.data.mass.adult$scientificName %in% omit.adult.mass),]
length(unique(clean.data.mass.adult.10 $scientificName)) #72 sp

# create loop to get distributions and do cutoffs at 3sigma?

# 1. get mean
# 2. get sd
# 3. get std error of mean: (sd / sqrt(n))
# 4. calculate 95% of distribution: mean +/- (1.96)*(std error)

limits <- clean.data.mass.adult.10 %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(count = n(), 
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.mass = sd(mass, na.rm = TRUE),
                   std.err = sd.mass/(sqrt(count)),
                   upper.limit = avg.mass + (1.96)*(std.err),
                   lower.limit = avg.mass - (1.96)*(std.err)) 

# remove species above the upper.limit and below the lower.limit
sp.adult <- unique(clean.data.mass.adult.10$scientificName)
clean.data.mass.adult.95 <- data.frame()
for(i in 1:length(sp.adult)){
  sub <- subset(clean.data.mass.adult.10, clean.data.mass.adult.10$scientificName == sp.adult[i])
  sub2 <- subset(sub, sub$mass > limits$lower.limit[limits$scientificName == sp.adult[i]] & sub$mass < limits$upper.limit[limits$scientificName == sp.adult[i]])
  clean.data.mass.adult.95 <- rbind(clean.data.mass.adult.95, sub2)
}

#recount how many samples there are per species
counts.adult.mass <- clean.data.mass.adult.95 %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(n = length(mass)) 

#keep species with more than 10 counts
keep.n <- counts.adult.mass$scientificName[counts.adult.mass$n > 10]
clean.data.mass.adult.95.10 <- clean.data.mass.adult.95[clean.data.mass.adult.95$scientificName %in% keep.n,]
length(unique(clean.data.mass.adult.95.10)) #4

#get rid of useless species names
futres.adult.95.10 <- subset(futres.adult.95.10, futres.adult.95.10$scientificName != "Sorex sp." | futres.adult.95.10$scientificName != "Peromyscus sp.")

length(unique(futres.adult.95.10$scientificName)) #31 sp

ggplot() +
  geom_density(data = clean.adult.masses.10, aes(x = log10(mass), fill = scientificName), alpha = 0.7) +
  theme(legend.position = "none") +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                     breaks = seq(-1, 7.5, 1),
                     limits = c(-0, 7.5),
                     expand=c(0,0))+
  scale_y_continuous(limits = c(0, 0.65),breaks = c(0,0.2,0.4,0.6),expand=c(0,0), 
                     name = 'Probability')

ggplot() +
  geom_density(data = futres.adult.95.10, aes(x = log10(mass), fill = scientificName), alpha = 0.7) +
  theme(legend.position = "none") +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                     breaks = seq(-1, 7.5, 1),
                     limits = c(-0, 7.5),
                     expand=c(0,0))+
  scale_y_continuous(limits = c(0, 0.65),breaks = c(0,0.2,0.4,0.6),expand=c(0,0), 
                     name = 'Probability')

## Method 2: get rid of extreme top 5% of adult body masses; 
## only allow 5% overlap between largest juveniles & smallest adults

futres.all.mass

#using same routine as above, but only trimming top 5% of adults
sp.adult <- unique(clean.adult.masses.10$scientificName)
futres.adult.bottom.95 <- data.frame()
for(i in 1:length(sp)){
  sub <- subset(clean.adult.masses.10, clean.adult.masses.10$scientificName == sp[i])
  sub2 <- subset(sub, sub$mass < futres.limits$upper.limit[futres.limits$scientificName == sp[i]])
  futres.adult.bottom.95 <- rbind(futres.adult.bottom.95, sub2)
}

#using adult species from above, get matching juvenile 

sp.match <- unique(futres.adult.bottom.95$scientificName)

#get counts by juvenile too
counts.mass <- clean.masses %>%
  dplyr::group_by(scientificName,) %>%
  dplyr::summarise(n = length(mass)) 



futres.all.mass.match <- futres.all.mass[futres.all.mass$scientificName %in% sp.match,]



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








