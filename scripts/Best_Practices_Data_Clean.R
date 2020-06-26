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

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$body_mass_1.units[i] == "GRAMS" | vertnet$body_mass_1.units[i] == "GR" | vertnet$body_mass_1.units[i] == "G" | vertnet$body_mass_1.units[i] == "gm" | vertnet$body_mass_1.units[i] == "grams" | vertnet$body_mass_1.units[i] == "gms" | vertnet$body_mass_1.units[i] == "GMS" | vertnet$body_mass_1.units[i] == "gr" | vertnet$body_mass_1.units[i] == "['Grams', 'gm']")){
    vertnet$body_mass_1.units[i] == "g"
  }
  else{
    next
  }
}
# }
# 
#   else if(isTRUE(vertnet$body_mass_1.units[i] == "mg")){
#     vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] * 1000
#     vertnet$body_mass_1.units[i] <- "g"
#   }
#   else if(isTRUE(vertnet$body_mass_1.units[i] == "kg")){
#     vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] / 1000
#     vertnet$body_mass_1.units[i] <- "g"
#   }
#   else if(isTRUE(vertnet$body_mass_1.units[i] == "lb" | vertnet$body_mass_1.units[i] == "lbs" | vertnet$body_mass_1.units[i] == "pounds")){
#     vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] * 453.592
#     vertnet$body_mass_1.units[i] <- "g"
#   }
#   else if(isTRUE(vertnet$body_mass_1.units[i] == "oz")){
#     vertnet$body_mass_1.value[i] <- vertnet$body_mass_1.value[i] * 28.3495
#     vertnet$body_mass_1.units[i] <- "g"
#   }
#   else{
#     next
#   }
# }

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$total_length_1.units[i] == "mm_shorthand" | vertnet$total_length_1.units[i] == "MM" | vertnet$total_length_1.units[i] == "Millimeters")){
    vertnet$total_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}
#   else if(isTRUE(vertnet$total_length_1.units[i] == "cm")){
#     vertnet$total_length_1.value[i] <- vertnet$total_length_1.value[i] / 10
#     vertnet$total_length_1.units[i] <- "mm"
#   }
#   else if(isTRUE(vertnet$total_length_1.units[i] == "in" | vertnet$total_length_1.units[i] == "inches")){
#     vertnet$total_length_1.value[i] <- vertnet$total_length_1.value[i] * 25.4
#     vertnet$total_length_1.units[i] <- "mm"
#   }
#   else if(isTRUE(vertnet$total_length_1.units[i] == "Foot" | vertnet$total_length_1.units[i] == "ft" | vertnet$total_length_1.units[i] == "FT" | vertnet$total_length_1.units[i] == "feet" | vertnet$total_length_1.units[i] == "'")){
#     vertnet$total_length_1.value[i] <- vertnet$total_length_1.value[i] * 304.8
#     vertnet$total_length_1.units[i] <- "mm"
#   }
#   else{
#     next
#   }
# }

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$hind_foot_length_1.units[i] == "MM" | vertnet$hind_foot_length_1.units[i] == "mm_shorthand" | vertnet$hind_foot_length_1.units[i] == "Millimeters" | vertnet$hind_foot_length_1.units[i] == "['MM', 'mm']")){
    vertnet$hind_foot_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}
#   else if(isTRUE(vertnet$hind_foot_length_1.units[i] == "cm")){
#     vertnet$hind_foot_length_1.value[i] <- vertnet$hind_foot_length_1.value[i] / 10
#     vertnet$hind_foot_length_1.units[i] <- "mm"
#   }
#   else if(isTRUE(vertnet$hind_foot_length_1.units[i] == "in" | vertnet$hind_foot_length_1.units[i] == "inches")){
#     vertnet$hind_foot_length_1.value[i] <- vertnet$hind_foot_length_1.value[i] * 25.4
#     vertnet$hind_foot_length_1.units[i] <- "mm"
#   }
#   else{
#     next
#   }
# }

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$ear_length_1.units[i] == "MM" | vertnet$ear_length_1.units[i] == "mm_shorthand" | vertnet$ear_length_1.units[i] == "Millimeters" | vertnet$ear_length_1.units[i] == "['MM', 'mm']")){
    vertnet$ear_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}
#   else if(isTRUE(vertnet$ear_length_1.units[i] == "cm")){
#     vertnet$ear_length_1.value[i] <- vertnet$ear_length_1.value[i] / 10
#     vertnet$ear_length_1.units[i] <- "mm"
#   }
#   else if(isTRUE(vertnet$ear_length_1.units[i] == "in")){
#     vertnet$ear_length_1.value[i] <- vertnet$ear_length_1.value[i] * 25.4
#     vertnet$ear_length_1.units[i] <- "mm"
#   }
#   else{
#     next
#   }
# }

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$forearm_length_1.units[i] == "MM" | vertnet$forearm_length_1.units[i] == "mm_shorthand" | vertnet$forearm_length_1.units[i] == "Millimeters" | vertnet$forearm_length_1.units[i] == "['MM', 'mm']")){
    vertnet$forearm_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}
#   else if(isTRUE(vertnet$forearm_length_1.units[i] == "cm")){
#     vertnet$forearm_length_1.value[i] <- vertnet$forearm_length_1.value[i] / 10
#     vertnet$forearm_length_1.units[i] <- "mm"
#   }
#   else if(isTRUE(vertnet$forearm_length_1.units[i] == "G")){
#     vertnet$forearm_length_1.units[i] <- "g"
#   }
#   else if(isTRUE(vertnet$forearm_length_1.units[i] == "lbs")){
#     vertnet$forearm_length_1.value[i] <- vertnet$forearm_length_1.value[i] * 453.592
#     vertnet$forearm_length_1.units[i] <- "g"
#   }
#   else{
#     next
#   }
# }

for(i in 1:length(vertnet$occurrenceid)){
  if(isTRUE(vertnet$tail_length_1.units[i] == "MM" | vertnet$tail_length_1.units[i] == "mm_shorthand" | vertnet$tail_length_1.units[i] == "Millimeters" | vertnet$tail_length_1.units[i] == "['MM', 'mm']")){
    vertnet$tail_length_1.units[i] <- "mm"
  }
  else{
    next
  }
}
#   else if(isTRUE(vertnet$tail_length_1.units[i] == "cm")){
#     vertnet$tail_length_1.value[i] <- vertnet$tail_length_1.value[i] / 10
#     vertnet$tail_length_1.units[i] <- "mm"
#   }
#   else if(isTRUE(vertnet$tail_length_1.units[i] == "in" | vertnet$tail_length_1.units[i] == "['inches', 'in']" | vertnet$tail_length_1.units[i] == "inches")){
#     vertnet$tail_length_1.value[i] <- vertnet$tail_length_1.value[i] * 25.4
#     vertnet$tail_length_1.units[i] <- "mm"
#   }
#   else if(isTRUE(vertnet$tail_1.units[i] == "Foot" | vertnet$tail_1.units[i] == "ft" | vertnet$tail_1.units[i] == "FT" | vertnet$tail_1.units[i] == "feet" | vertnet$tail_1.units[i] == "'")){
#     vertnet$tail_1.value[i] <- vertnet$tail_1.value[i] * 304.8
#     vertnet$tail_1.units[i] <- "mm"
#   }
#   else{
#     next
#   }
# }

#write.csv(vertnet, "vertnet.clean.csv")

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

# data$mass <- as.numeric(data$mass)
# data$length <- as.numeric(data$total.length)
# data$hindfoot.length <- as.numeric(data$hindfoot.length)
# data$Calcaneus.GB <- as.numeric(data$Calcaneus.GB)
# data$Calcaneus.GL <- as.numeric(data$Calcaneus.GL)
# data$tooth.row <- as.numeric(data$tooth.row)

# data.clean <- data
# data.clean$mass.use <- rep(NA, length(data.clean$mass))
# data.clean$total.length.use <- rep(NA, length(data.clean$total.length))
# for(i in 1:length(data$scientificName)){
#   if(isTRUE(data.clean$mass.units[i] == "g")){
#     data.clean$mass.use[i] <- "yes"
#   }
#   else if(isTRUE(data.clean$total.length.units[i] == "mm")){
#     data.clean$total.length.use[i] <- "yes"
#   }
#   else {
#     next
#   }
# }

#write.csv(data.clean, "clean.data.csv")

data.adult <- data[data$lifeStage == "Adult",]
length(unique(data.adult$scientificName)) #295 spp
#still some animals w/ "sp."

data.adult.clean <- data.adult[!grepl('sp.',data.adult$scientificName),] #274 spp


data.adult.clean$mass <- as.numeric(data.adult.clean$mass)
data.adult.clean$total.length <- as.numeric(data.adult.clean$total.length)
data.adult.clean$hindfoot.length <- as.numeric(data.adult.clean$hindfoot.length)
data.adult.clean$Calcaneus.GB <- as.numeric(data.adult.clean$Calcaneus.GB)
data.adult.clean$Calcaneus.GL <- as.numeric(data.adult.clean$Calcaneus.GL)
data.adult.clean$tooth.row <- as.numeric(data.adult.clean$tooth.row)

data.adult_stats <- data.adult.clean %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   avg.mass = 
                     mean(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   min.length = min(total.length, na.rm = TRUE),
                   max.length = max(total.length, na.rm = TRUE),
                   avg.length = mean(total.length, na.rm = TRUE))

keep.adult <- data.adult_stats$scientificName[data.adult_stats$sample.size >= 10] #59
data.adult.10 <- data.adult.clean[data.adult.clean$scientificName %in% keep.adult,]
length(unique(data.adult.10$scientificName)) #59

#write.csv(data.adult.10, "data.adult.10.csv")

data.stand <- as.data.frame(subset(data.adult.10, data.adult.10$mass.units == "g" & data.adult.10$total.length.units == "mm")) #67 spp

data.stand_stats <- data.stand %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   avg.mass = 
                     mean(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   min.length = min(total.length, na.rm = TRUE),
                   max.length = max(total.length, na.rm = TRUE),
                   avg.length = mean(total.length, na.rm = TRUE))

keep.stand <- data.stand_stats$scientificName[data.stand_stats$sample.size >= 10] #25
data.stand.10 <- data.stand[data.stand$scientificName %in% keep.stand,]
length(unique(data.stand.10$scientificName)) #43

# remove samples that are 3 sigmas outside of distribution
# find 3xsigma
data.stand.sigma <- data.stand.10 %>%
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
# but include full dataset? or ignore? [go back to data.adult.10 %iN%]
# for now just the dataset with appropriate datasets

sp.stand <- unique(data.stand.10$scientificName)
data.stand.trim <- data.frame()
for(i in 1:length(sp.stand)){
  data.sub <- subset(data.stand.10, data.stand.10$scientificName == sp.stand[i])
  sigma.sub <- subset(data.stand.sigma, data.stand.sigma$scientificName == sp.stand[i])
  for(j in 1:length(data.sub$scientificName)){
    if(isTRUE(data.sub$mass[j] < sigma.sub$mass.lower.limit)){
      data.sub$mass[j] <- "outlier"
    }
    else if(isTRUE(data.sub$mass[j] > sigma.sub$mass.upper.limit)){
      data.sub$mass[j] <- "outlier"
    }
    else if(isTRUE(data.sub$total.length[j] < sigma.sub$length.lower.limit)){
      data.sub$total.length[j] <- "outlier"
    }
    else if(isTRUE(data.sub$total.length[j] > sigma.sub$mass.upper.limit)){
      data.sub$total.length[j] <- "outlier"
    }
    else{
      next
    }
  }
  data.stand.trim <- rbind(data.stand.trim, data.sub)
}

length(data.stand.trim$scientificName[data.stand.trim$mass == "outlier"]) #878 outliers (~50%)
length(data.stand.trim$scientificName) #1260 total

data.stand.trim$mass <- as.numeric(data.stand.trim$mass)
data.stand.trim$total.length <- as.numeric(data.stand.trim$total.length)

data.stand.trim_stats <- data.stand.trim %>%
  group_by(scientificName) %>%
  dplyr::summarise(counts = n()) %>%
  as.data.frame()

keep.trim <- data.stand.trim_stats$scientificName[data.stand.trim_stats$counts >= 10] #25
data.stand.trim.10 <- data.stand.trim[data.stand.trim$scientificName %in% keep.trim,]
length(unique(data.stand.trim.10$scientificName)) #25

#write.csv(data.stand.trim.10, "data.mass.length.csv")


