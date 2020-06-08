# Best Practices Data Clean Up
# Meghan A. Balk
# balkm@email.arizona.edu

## Load packages
require(tidyverse)
require(nlme)x
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

## VertNet data
bat_mass <- read.csv("https://de.cyverse.org/dl/d/C08C5EB3-DE0E-426B-BA83-048902DCBE49/vertnet_bats_body_mass_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)
bat_length <- read.csv("https://de.cyverse.org/dl/d/A54657B7-D167-45C2-9397-5762B3A96707/vertnet_bats_total_len_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)
#mamm has no bats
mamm_mass <- read.csv("https://de.cyverse.org/dl/d/8F3D34AE-A3C8-4F7C-BF57-1C978D5C7825/vertnet_no_bats_body_mass_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)
mamm_length <- read.csv("https://de.cyverse.org/dl/d/5F2F2DDA-8923-4EE0-8A0F-D3C50B07239F/vertnet_no_bats_total_len_2020-04-16a.csv", header = TRUE, stringsAsFactors = FALSE)

#bernor_equid$binomial <- paste(bernor_equid$GENUS, bernor_equid$SPECIES)

#align datasets
sp.1 <- unique(futres$scientificName)

pan <- panthera[(panthera$MSW05_Binomial %in% sp.1),]

## need to get rid of subspecies
bat_mass$scientificName <- word(bat_mass$scientificname, 1,2, sep = " ") #435 unique spp
mamm_mass$scientificName <- word(mamm_mass$scientificname, 1,2, sep = " ") #1190 unique spp

# group lifeStages
futres$lifeStage[futres$lifeStage == "young adult" | futres$lifeStage == "Adult" | futres$lifeStage == "Prime Adult"] <- "Adult"
futres$lifeStage[futres$lifeStage == "Juvenile" | futres$lifeStage == "juvenile"] <- "Juvenile"

futres <- subset(futres, futres$lifeStage == "Juvenile" | futres$lifeStage == "Adult")

bat_mass <- subset(bat_mass, bat_mass$lifestage_cor == "Adult" | bat_mass$lifestage_cor == "Juvenile")
mamm_mass <- subset(mamm_mass, mamm_mass$lifestage_cor == "Adult" | mamm_mass$lifestage == "Juvenile")

bat_mass$body_mass_1.value <- as.numeric(bat_mass$body_mass_1.value)

futres_stats <- futres %>%
  group_by(scientificName, lifeStage) %>%
  dplyr::summarise(sample.size = n(), 
            min.mass = min(Total.Fresh.Weight..g., na.rm = TRUE),
            max.mass  = max(Total.Fresh.Weight..g., na.rm = TRUE))

bat_stats <- bat_mass %>%
  group_by(scientificName, lifestage_cor) %>%
  dplyr::summarise(count = n(),
            min.mass = min(body_mass_1.value, na.rm = TRUE),
            max.mass = max(body_mass_1.value, na.rm = TRUE))

#convert to g for mammal mass
mamm_mass <- subset(mamm_mass, mamm_mass$lifestage_cor == "Adult" | mamm_mass$lifestage_cor == "Juvenile")
mamm_mass$body_mass_1.value <- as.numeric(mamm_mass$body_mass_1.value)

mamm_mass_g <- subset(mamm_mass, mamm_mass$body_mass_1.units == "g" | mamm_mass$body_mass_1.units == "Grams")
mamm_mass_g$body_mass_1.units <- "g"

mamm_mass_kg <- subset(mamm_mass, mamm_mass$body_mass_1.units == "kg")
mamm_mass_kg$body_mass_1.value <- mamm_mass_kg$body_mass_1.value / 1000
mamm_mass_kg$body_mass_1.units <- "g"

mamm_mass_lb <- subset(mamm_mass, mamm_mass$body_mass_1.units == "lb" | mamm_mass$body_mass_1.units == "lbs" | mamm_mass$body_mass_1.units == "pounds")
# 1 lb = 453.592g
mamm_mass_lb$body_mass_1.value <- mamm_mass_lb$body_mass_1.value * 453.592
mamm_mass_lb$body_mass_1.units <- "g"

mamm_mass_oz <- subset(mamm_mass, mamm_mass$body_mass_1.units == "oz")
#1 oz = 28.3495 g
mamm_mass_oz$body_mass_1.value <- mamm_mass_oz$body_mass_1.value * 28.3495
mamm_mass_oz$body_mass_1.units <- "g"

mamm_mass_allg <- rbind(mamm_mass_g, mamm_mass_kg, mamm_mass_lb, mamm_mass_oz)

#mamm_mass_mix <- subset(mamm_mass, mamm_mass$body_mass_1.units == "['lb', 'oz']" | mamm_mass$body_mass_1.units == "['lbs', 'oz']")
#deal with these later

mamm_stats <- mamm_mass_allg %>%
  group_by(scientificName, lifestage_cor) %>%
  dplyr::summarise(count = n(),
                   min.mass = min(body_mass_1.value, na.rm = TRUE),
                   max.mass = max(body_mass_1.value, na.rm = TRUE))

# for now adults only
futres.adult <- subset(futres, futres$lifeStage == "Adult")

# figure out distributions of juveniles and cut them out of adult body sizes
futres.mass <- futres.adult %>%
  select(scientificName, mass = Total.Fresh.Weight..g.)
bat.adult <- bat_mass %>%
  select(scientificName, mass = body_mass_1.value)
mamm.adult <- mamm_mass %>%
  select(scientificName, mass = body_mass_1.value)

futres.adult.mass <- rbind(futres.mass, bat.adult, mamm.adult)
futres.adult.mass$mass <- as.numeric(futres.adult.mass$mass)

# clean up data
clean.masses <- futres.adult.mass %>%
  na.omit()

counts.mass <- clean.masses %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(n = length(mass)) 

omit.mass <- counts.mass$scientificName[counts.mass$n < 10]

clean.masses.10 <- clean.masses[!(clean.masses$scientificName %in% omit.mass),]
length(unique(clean.masses$scientificName)) #99 spp

# create loop to get distributions and do cutoffs at 3sigma?

# 1. get mean
# 2. get sd
# 3. get std error of mean: (sd / sqrt(n))
# 4. calculate 95% of distribution: mean +/- (1.96)*(std error)

futres.limits <- clean.masses.10 %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(count = n(), 
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.mass = sd(mass, na.rm = TRUE),
                   std.err = sd.mass/(sqrt(count)),
                   upper.limit = avg.mass + (1.96)*(std.err),
                   lower.limit = avg.mass - (1.96)*(std.err)) 

sp <- unique(clean.masses.10$scientificName)
futres.95 <- data.frame()
for(i in 1:length(sp)){
  sub <- subset(clean.masses.10, clean.masses.10$scientificName == sp[i])
  sub2 <- subset(sub, sub$mass > futres.limits$lower.limit[futres.limits$scientificName == sp[i]] & sub$mass < futres.limits$upper.limit[futres.limits$scientificName == sp[i]])
  futres.95 <- rbind(futres.95, sub2)
}

counts.mass <- futres.95 %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(n = length(mass)) 
keep.n <- counts.mass$scientificName[counts.mass > 10]
futres.95.10 <- futres.95[futres.95$scientificName %in% keep.n,]
length(unique(futres.95.10$scientificName)) #85 spp

clean.masses.10 <- subset(clean.masses.10, clean.masses.10$scientificName != "Sorex sp.") #98 spp

ggplot() +
  geom_density(data = clean.masses.10, aes(x = log10(mass), fill = scientificName), alpha = 0.7) +
  theme(legend.position = "none") +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                     breaks = seq(-1, 7.5, 1),
                     limits = c(-0, 7.5),
                     expand=c(0,0))+
  scale_y_continuous(limits = c(0, 0.65),breaks = c(0,0.2,0.4,0.6),expand=c(0,0), 
                     name = 'Probability')

ggplot() +
  geom_density(data = futres.95, aes(x = log10(mass), fill = scientificName), alpha = 0.7) +
  theme(legend.position = "none") +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                     breaks = seq(-1, 7.5, 1),
                     limits = c(-0, 7.5),
                     expand=c(0,0))+
  scale_y_continuous(limits = c(0, 0.65),breaks = c(0,0.2,0.4,0.6),expand=c(0,0), 
                     name = 'Probability')
#looks a lot cleaner...

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








