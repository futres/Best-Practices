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

#kitty_deer <- read.csv("EAP Florida Modern Deer Measurements_FORFUTRES_1_23_2020.csv", header = TRUE, stringsAsFactors = FALSE)
#blois_ground.squirrel <- read.csv("J.Biogeo.2008.AllData.Final.csv", header = TRUE, stringsAsFactors = FALSE)
#amelia_impala <- read.csv("Extant Aepyceros database_updated 11_2016.csv", header = TRUE, stringsAsFactors = FALSE)
#bernor_equid <- read.csv("ToFuTRESVER_12_4_16_2020_REV_19.csv", header = TRUE, stringsAsFactors = FALSE)
#cougar_OR <- read.csv("1987-2019 Cougar Weight-Length Public Request.csv", header = TRUE, stringsAsFactors = FALSE)

futres <- read.csv("https://de.cyverse.org/dl/d/42CF8FC4-13FC-4D57-A73B-90668E2AFBCC/futres.csv", header = TRUE, stringsAsFactors = FALSE)
#about futres data:
#has various terms for adult and juvenile
#currently aligned manually; fixed Amelia's weight data to be g, deleted lone mass that did not have units
#6 species


## VertNet data
bat_mass <- read.csv("https://de.cyverse.org/dl/d/2A542CDF-BDED-4486-AB15-445B53F80F08/vertnet_bats_body_mass_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#706 spp; 18530 rows; 10350 occurrenceids

#get rid of dups
bat_mass.clean <- bat_mass[!(duplicated(bat_mass)),]
#706 spp; 10350 rows; 10350 occurrenceids
#8000 diff

bat_length <- read.csv("https://de.cyverse.org/dl/d/896A54B4-1E52-4976-95AB-71449384B3A4/vertnet_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#696 spp; 16313 rows; 9460 occurrence ids

#get rid of dups
bat_length.clean <- bat_length[!(duplicated(bat_length)),]
#696 spp; 9460 rows; 9460 occurrence ids
#6853 diff

#mamm has no bats
mamm_mass <- read.csv("https://de.cyverse.org/dl/d/EF537422-2246-4B25-A9BC-D8259C78BFA2/vertnet_no_bats_body_mass_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#2439 spp; 225237 rows; 89588 occurrenceids

#get rid of dups
mamm_mass.clean <- mamm_mass[!(duplicated(mamm_mass)),]
#2439 spp; 89588 rows; 89588 occurrenceids
#135649 diff

mamm_length <- read.csv("https://de.cyverse.org/dl/d/DA7E36EF-0008-4DED-A49C-C7DCCC71E98C/vertnet_no_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#2728 spp; 245647 rows; 97225 occurrenceids

#get rid of dups
mamm_length.clean <- mamm_length[!(duplicated(mamm_length)),]
#2728 spp; 97225 rows; 97225 occurrenceids
#148422 diff

#about VertNet data:
#has adult, juvenile, and NA for lifestage
#has mass in different units, sometimes inferreed

#bernor_equid$binomial <- paste(bernor_equid$GENUS, bernor_equid$SPECIES)
#deal with this once mapped

## combine mass & length
bat_length.sub <- subset(bat_length.clean, select = c(scientificname, total_length_1.value, total_length_1.units, occurrenceid))
bat_mass.sub <- dplyr::select(bat_mass.clean, -c('total_length_1.value', 'total_length_1.units'))
bats <- full_join(bat_mass.sub, bat_length.sub, by = c("occurrenceid", "scientificname"))
length(unique(bats$scientificname)) #724; added 28 spp
#10634 occurrenceids; 48657 rows; added 284 occ. ids
#why are there dupes???

## combine mass & length
mamm_length.sub <- subset(mamm_length.clean, select = c(scientificname, total_length_1.value, total_length_1.units, occurrenceid))
mamm_mass.sub <- dplyr::select(mamm_mass.clean, -c('total_length_1.value', 'total_length_1.units'))
mamm <- full_join(mamm_mass.sub, mamm_length.sub, by = c("occurrenceid", "scientificname"))
length(unique(mamm$scientificname)) #2766; added 38 spp
#795495 rows; 101440 occ ids; added 11852 occ. ids

#are column names in the same order?
x <- colnames(mamm)
y <- colnames(bats)
cols <- as.data.frame(cbind(x,y))
cols$same <- cols$x[1] == cols$y[1]
#all TRUE!

vertnet <- rbind(bats, mamm) #sum = bats rows + mamm rows

## need to get rid of subspecies
vertnet$scientificName <- word(vertnet$scientificname, 1,2, sep = " ") 
length(unique(vertnet$scientificName)) #1738
# in mamm: 2766 spp; and now 1738 because got rid of trinomials
#some weird spp. names: e.g.,: (new SW
#will deal with later

# group lifeStages
futres$lifeStage[futres$lifeStage == "young adult" | futres$lifeStage == "Adult" | futres$lifeStage == "Prime Adult" | futres$lifeStage == "adult"] <- "Adult"
futres$lifeStage[futres$lifeStage == "Juvenile" | futres$lifeStage == "juvenile"] <- "Juvenile"
# need to convert to g
futres$Total.Fresh.Weight..g. <- as.numeric(futres$Total.Fresh.Weight..g.)
futres$Total.Fresh.Weight..g.[futres$scientificName == "Aepyceros melampus"] <- futres$Total.Fresh.Weight..g.[futres$scientificName == "Aepyceros melampus"] / .0022 
futres$Total.Fresh.Weight..g.[futres$scientificName == "Puma concolor"] <- futres$Total.Fresh.Weight..g.[futres$scientificName == "Puma concolor"] / .0022

#convert to g for mammal & bat mass
vertnet$body_mass_1.value <- as.numeric(vertnet$body_mass_1.value)
vertnet$total_length_1.value <- as.numeric(vertnet$total_length_1.value)
vertnet$hind_foot_length_1.value <- as.numeric(vertnet$hind_foot_length_1.value)
vertnet$tail_length_1.value <- as.numeric(vertnet$tail_length_1.value)
vertnet$ear_length_1.value <- as.numeric(vertnet$ear_length_1.value)
vertnet$forearm_length_1.value <- as.numeric(vertnet$forearm_length_1.value)

#convert to standard units
for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$body_mass_1.units[i] == "GRAMS" | vertnet$body_mass_1.units[i] == "GR" | vertnet$body_mass_1.units[i] == "G" | vertnet$body_mass_1.units[i] == "gm" | vertnet$body_mass_1.units[i] == "grams" | vertnet$body_mass_1.units[i] == "gms" | vertnet$body_mass_1.units[i] == "GMS" | vertnet$body_mass_1.units[i] == "gr" | vertnet$body_mass_1.units[i] == "['Grams', 'gm']")){
    vertnet$body_mass_1.units[i] == "g"
 }

  else if(isTRUE(vertnet$body_mass_1.units[i] == "mg")){
    vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] * 1000
    vertnet$body_mass_1.units[i] <- "g"
  }
  else if(isTRUE(vertnet$body_mass_1.units[i] == "kg")){
    vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] / 1000
    vertnet$body_mass_1.units[i] <- "g"
  }
  else if(isTRUE(vertnet$body_mass_1.units[i] == "lb" | vertnet$body_mass_1.units[i] == "lbs" | vertnet$body_mass_1.units[i] == "pounds")){
    vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] / 0.002204623
    vertnet$body_mass_1.units[i] <- "g"
  }
  else if(isTRUE(vertnet$body_mass_1.units[i] == "oz")){
    vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] / 0.03527396
    vertnet$body_mass_1.units[i] <- "g"
  }
  else{
    next
  }
}

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$total_length_1.units[i] == "mm_shorthand" | vertnet$total_length_1.units[i] == "MM" | vertnet$total_length_1.units[i] == "Millimeters")){
    vertnet$total_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$total_length_1.units[i] == "cm")){
    vertnet$total_length_1.value[i] <- vertnet$total_length_1.value[i] / 10
    vertnet$total_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$total_length_1.units[i] == "in" | vertnet$total_length_1.units[i] == "inches")){
    vertnet$total_length_1.value[i] <- vertnet$total_length_1.value[i] / 0.03937008
    vertnet$total_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$total_length_1.units[i] == "Foot" | vertnet$total_length_1.units[i] == "ft" | vertnet$total_length_1.units[i] == "FT" | vertnet$total_length_1.units[i] == "feet" | vertnet$total_length_1.units[i] == "'")){
    vertnet$total_length_1.value[i] <- vertnet$total_length_1.value[i] / 0.00328084
    vertnet$total_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$hind_foot_length_1.units[i] == "MM" | vertnet$hind_foot_length_1.units[i] == "mm_shorthand" | vertnet$hind_foot_length_1.units[i] == "Millimeters" | vertnet$hind_foot_length_1.units[i] == "['MM', 'mm']")){
    vertnet$hind_foot_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$hind_foot_length_1.units[i] == "cm")){
    vertnet$hind_foot_length_1.value[i] <- vertnet$hind_foot_length_1.value[i] / 10
    vertnet$hind_foot_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$hind_foot_length_1.units[i] == "in" | vertnet$hind_foot_length_1.units[i] == "inches")){
    vertnet$hind_foot_length_1.value[i] <- vertnet$hind_foot_length_1.value[i] / 0.03937008
    vertnet$hind_foot_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$ear_length_1.units[i] == "MM" | vertnet$ear_length_1.units[i] == "mm_shorthand" | vertnet$ear_length_1.units[i] == "Millimeters" | vertnet$ear_length_1.units[i] == "['MM', 'mm']")){
    vertnet$ear_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$ear_length_1.units[i] == "cm")){
    vertnet$ear_length_1.value[i] <- vertnet$ear_length_1.value[i] / 10
    vertnet$ear_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$ear_length_1.units[i] == "in")){
    vertnet$ear_length_1.value[i] <- vertnet$ear_length_1.value[i] / 0.03937008
    vertnet$ear_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$forearm_length_1.units[i] == "MM" | vertnet$forearm_length_1.units[i] == "mm_shorthand" | vertnet$forearm_length_1.units[i] == "Millimeters" | vertnet$forearm_length_1.units[i] == "['MM', 'mm']")){
    vertnet$forearm_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$forearm_length_1.units[i] == "cm")){
    vertnet$forearm_length_1.value[i] <- vertnet$forearm_length_1.value[i] / 10
    vertnet$forearm_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$forearm_length_1.units[i] == "G")){
    vertnet$forearm_length_1.units[i] <- "g"
  }
  else if(isTRUE(vertnet$forearm_length_1.units[i] == "lbs")){
    vertnet$forearm_length_1.value[i] <- vertnet$forearm_length_1.value[i] / 0.002204623
    vertnet$forearm_length_1.units[i] <- "g"
  }
  else{
    next
  }
}

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$tail_length_1.units[i] == "MM" | vertnet$tail_length_1.units[i] == "mm_shorthand" | vertnet$tail_length_1.units[i] == "Millimeters" | vertnet$tail_length_1.units[i] == "['MM', 'mm']")){
    vertnet$tail_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$tail_length_1.units[i] == "cm")){
    vertnet$tail_length_1.value[i] <- vertnet$tail_length_1.value[i] / 10
    vertnet$tail_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$tail_length_1.units[i] == "in" | vertnet$tail_length_1.units[i] == "['inches', 'in']" | vertnet$tail_length_1.units[i] == "inches")){
    vertnet$tail_length_1.value[i] <- vertnet$tail_length_1.value[i] / 0.03937008
    vertnet$tail_length_1.units[i] <- "mm"
  }
  else if(isTRUE(vertnet$tail_1.units[i] == "Foot" | vertnet$tail_1.units[i] == "ft" | vertnet$tail_1.units[i] == "FT" | vertnet$tail_1.units[i] == "feet" | vertnet$tail_1.units[i] == "'")){
    vertnet$tail_1.value[i] <- vertnet$tail_1.value[i] / 0.00328084
    vertnet$tail_1.units[i] <- "mm"
  }
  else{
    next
  }
}

#deal with these later
#vertnet.length_mix

futres.sub <- futres %>%
  select(scientificName, lifeStage = lifeStage, sex = sex,
         mass = Total.Fresh.Weight..g.,
         total.length = TL..mm...Total.Length.,
         hindfoot.length = HF..mm...Hind.Foot.Length.,
         Calcaneus.GL = Calcaneus.GL...greatest.length..von.den.Driesch..1976...mm,
         Calcaneus.GB = Calcaneus.GB...greatest.breadth..von.den.Driesch.1976...mm,
         tooth.row = c.toothrow.1.mm)
futres.sub$mass.units <- "g"
futres.sub$total.length.units <- "mm"
futres.sub$hindfoot.length.units <- "mm"
futres.sub$Calcaneus.GB.units <- "mm"
futres.sub$Calcaneus.GL.units <- "mm"
futres.sub$tooth.row.units <- "mm"
futres.sub$occurrence.id <- c(1:length(futres.sub$scientificName))

futres.sub$mass[futres.sub$scientificName == "Puma concolor"] <- futres.sub$mass[futres.sub$scientificName == "Puma concolor"] / .002204623

vertnet.sub <- vertnet %>%
  select(scientificName = scientificName, lifeStage = lifestage_cor, sex = sex,
         mass = body_mass_1.value,
         mass.units = body_mass_1.units,
         total.length = total_length_1.value, 
         total.length.units = total_length_1.units,
         hindfoot.length = hind_foot_length_1.value,
         hindfoot.length.units = hind_foot_length_1.units,
         occurrence.id = occurrenceid)
vertnet.sub$Calcaneus.GL = "NA"
vertnet.sub$Calcaneus.GL.units = "NA"
vertnet.sub$Calcaneus.GB = "NA"
vertnet.sub$Calcaneus.GB.units = "NA"
vertnet.sub$tooth.row = "NA"
vertnet.sub$tooth.row.units = "NA"

#are column names in the same order?
x <- colnames(futres.sub)
y <- colnames(vertnet.sub)
cols <- as.data.frame(cbind(x,y))
cols$same <- cols$x[1] == cols$y[1]
#all TRUE!

data <- rbind(vertnet.sub, futres.sub)
length(unique(data$scientificName)) #1739 spp

data.binom<- data[!grepl('sp.',data$scientificName),]
length(unique(data.binom$scientificName)) #1631 sp

data.noJuv <- data.binom[data.binom$lifeStage != "Juvenile",]
length(unique(data.noJuv$scientificName)) #1522 spp

data.noJuv$mass <- as.numeric(data.noJuv$mass)
data.noJuv$total.length <- as.numeric(data.noJuv$total.length)
data.noJuv$hindfoot.length <- as.numeric(data.noJuv$hindfoot.length)
data.noJuv$Calcaneus.GB <- as.numeric(data.noJuv$Calcaneus.GB)
data.noJuv$Calcaneus.GL <- as.numeric(data.noJuv$Calcaneus.GL)
data.noJuv$tooth.row <- as.numeric(data.noJuv$tooth.row)

#need 10 samples per species
data.noJuv_stats <- data.noJuv %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   avg.mass = 
                     mean(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   min.length = min(total.length, na.rm = TRUE),
                   max.length = max(total.length, na.rm = TRUE),
                   avg.length = mean(total.length, na.rm = TRUE))

keep.data <- data.noJuv_stats$scientificName[data.noJuv_stats$sample.size >= 10] #566
data.10 <- data.noJuv[data.noJuv$scientificName %in% keep.data,]
data.10 <- data.10[!(is.na(data.10$scientificName)),]
length(unique(data.10$scientificName)) #565



##Find outliers; 2 sigmas
# look at size of sigmas
  # plot sigmas; (COV: std v mean); those with issues would be outliers on the line
# highlight those outside of 2 sigmas and not "g" or "mm"
# change those within 2 sigmas and change to correct units "g" or "mm"
# for analysis use 2 or 3 sigmas
# interquartile ranges (look for 2-3 values outside); creating bins of data

data.sigma <- data.10 %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(),
                   mass.mean = mean(mass, na.rm = TRUE),
                   mass.sigma = sd(mass, na.rm = TRUE),
                   two.sigma.mass = 2*mass.sigma,
                   mass.upper.limit = mass.mean+two.sigma.mass,
                   mass.lower.limit = mass.mean-two.sigma.mass,
                   length.sigma = sd(total.length, na.rm = TRUE),
                   length.mean = mean(total.length),
                   two.sigma.length = 2*length.sigma,
                   length.upper.limit = length.mean + two.sigma.length,
                   length.lower.limit = length.mean - two.sigma.length) %>%
  as.data.frame()

# label outliers
data.outlier <- data.frame()

sp <- unique(data.10$scientificName)
for(i in 1:length(sp)){
  sub.data <- subset(data.10, data.10$scientificName == sp[i])
  sub.sigma <- subset(data.sigma, data.sigma$scientificName == sp[i])
  sub.data$mass.upper.limit <- sub.sigma$mass.upper.limit[1]
  sub.data$mass.lower.limit <- sub.sigma$mass.lower.limit[1]
  sub.data$length.upper.limit <- sub.sigma$length.upper.limit[1]
  sub.data$length.lower.limit <- sub.sigma$length.lower.limit[1]
  data.outlier <- rbind(data.outlier, sub.data)
}

#ask if is true that mass and length fall inbetween upper and lower limit
data.outlier$mass.status <- rep("", length(data.outlier$scientificName))
data.outlier$length.status <- rep("", length(data.outlier$scientificName))

for(i in 1:length(data.outlier$scientificName)){
  if(isTRUE(data.outlier$mass[i] < data.outlier$mass.lower.limit[i])){
    data.outlier$mass.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$mass[i] > data.outlier$mass.upper.limit[i])){
    data.outlier$mass.status[i] <- "outlier"
  }
  else{
    next
  }
}

for(i in 1:length(data.outlier$scientificName)){
  if(isTRUE(data.outlier$total.length[i] < data.outlier$length.lower.limit[i])){
    data.outlier$length.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$total.length[i] > data.outlier$length.upper.limit[i])){
    data.outlier$length.status[i] <- "outlier"
  }
  else{
    next
  }
}

#look at outliers
outliers <- data.outlier %>%
  group_by(scientificName, mass.status) %>%
  dplyr::summarise(sample.size = n()) %>%
  as.data.frame()

sum(outliers$sample.size[outliers$mass.status == "outlier"]) #2303 samples
length(outliers$sample.size[outliers$mass.status == "outlier"]) #395 spp 

# remove outliers
data.clean <- subset(data.outlier, data.outlier$mass.status != "outlier" | data.outlier$length.status != "outlier")
length(unique(data.clean$scientificName))
#reduce sample size = 10 again
data.clean_stats <- data.clean %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n())

keep.clean <- data.clean_stats$scientificName[data.clean_stats$sample.size >= 10]
data.clean.10 <- data.clean[data.clean$scientificName %in% keep.clean,]
length(unique(data.clean.10$scientificName)) #564

#write.csv(data.clean.10, "clean.data.csv")
