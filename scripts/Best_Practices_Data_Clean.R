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
require(OutlierDetection)

##Load data----

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
bat_mass.clean <- bat_mass[!(duplicated(bat_mass$occurrenceid)),]
#706 spp; 10350 rows; 10350 occurrenceids
#8000 diff

bat_length <- read.csv("https://de.cyverse.org/dl/d/896A54B4-1E52-4976-95AB-71449384B3A4/vertnet_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#696 spp; 16313 rows; 9460 occurrence ids

#get rid of dups
bat_length.clean <- bat_length[!(duplicated(bat_length$occurrenceid)),]
#696 spp; 9460 rows; 9460 occurrence ids
#6853 diff

#mamm has no bats
mamm_mass <- read.csv("https://de.cyverse.org/dl/d/EF537422-2246-4B25-A9BC-D8259C78BFA2/vertnet_no_bats_body_mass_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#2439 spp; 225237 rows; 89588 occurrenceids

#get rid of dups
mamm_mass.clean <- mamm_mass[!(duplicated(mamm_mass$occurrenceid)),]
#2439 spp; 89588 rows; 89588 occurrenceids
#135649 diff

mamm_length <- read.csv("https://de.cyverse.org/dl/d/DA7E36EF-0008-4DED-A49C-C7DCCC71E98C/vertnet_no_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#2728 spp; 245647 rows; 97225 occurrenceids

#get rid of dups
mamm_length.clean <- mamm_length[!(duplicated(mamm_length$occurrenceid)),]
#2728 spp; 97225 rows; 97225 occurrenceids
#148422 diff

#about VertNet data:
#has adult, juvenile, and NA for lifestage
#has mass in different units, sometimes inferreed

#bernor_equid$binomial <- paste(bernor_equid$GENUS, bernor_equid$SPECIES)
#deal with this once mapped

##VERTNET CLEANING----

## combine mass & length
#bat data
bat_length.sub <- subset(bat_length.clean, select = c(scientificname, total_length_1.value, total_length_1.units, occurrenceid))
bat_mass.sub <- dplyr::select(bat_mass.clean, -c('total_length_1.value', 'total_length_1.units'))
bats <- full_join(bat_mass.sub, bat_length.sub, by = c("occurrenceid", "scientificname"))
length(unique(bats$scientificname)) #724; added 28 spp
#10634 occurrenceids; 48657 rows; added 284 occ. ids
#why are there dupes???

#mammal data
mamm_length.sub <- subset(mamm_length.clean, select = c(scientificname, total_length_1.value, total_length_1.units, occurrenceid))
mamm_mass.sub <- dplyr::select(mamm_mass.clean, -c('total_length_1.value', 'total_length_1.units'))
mamm <- full_join(mamm_mass.sub, mamm_length.sub, by = c("occurrenceid", "scientificname"))
length(unique(mamm$scientificname)) #2766; added 38 spp
#795495 rows; 101440 occ ids; added 11852 occ. ids

##combine
vertnet <- rbind(bats, mamm) #sum = bats rows + mamm rows

##FUTRES CLEANING----

##group lifeStages
futres$lifeStage[futres$lifeStage == "young adult" | futres$lifeStage == "Adult" | futres$lifeStage == "Prime Adult" | futres$lifeStage == "adult"] <- "Adult"
futres$lifeStage[futres$lifeStage == "Juvenile" | futres$lifeStage == "juvenile"] <- "Juvenile"

##need to convert to g
futres$Total.Fresh.Weight..g. <- as.numeric(futres$Total.Fresh.Weight..g.)
futres$Skin.weight..g. <- as.numeric(futres$Skin.weight..g.)
futres$Weight.field.dressed <- as.numeric(futres$Weight.field.dressed)
futres$Total.Fresh.Weight..g.[futres$scientificName == "Puma concolor"] <- futres$Total.Fresh.Weight..g.[futres$scientificName == "Puma concolor"] / .0022
futres$Skin.weight..g.[futres$scientificName == "Puma concolor"] <- futres$Skin.weight..g.[futres$scientificName == "Puma concolor"] / .0022
futres$Weight.field.dressed[futres$scientificName == "Puma concolor"] <- futres$Weight.field.dressed[futres$scientificName == "Puma concolor"] / .0022

##Combine Data----

##select out columns
order <- c("occurrenceID", "scientificName", "lifeStage", "sex",
           "catalogNumber", "materialSampleID", "locality", "stateProvince",
           "decimalLatitude", "decimalLongitude", "elevation",
           "mass", "mass.units", "mass.units.inferred", 
           "gutted", "gutted.units", "gutted.units.inferred",
           "skinned", "skinned.units", "skinned.units.inferred",
           "total.length", "total.length.units", "total.length.units.inferred",
           "hindfoot.length", "hindfoot.length.units", "hindfoot.length.units.inferred",
           "ear.length", "ear.length.units", "ear.length.units.inferred",
           "tail.length", "tail.length.units", "tail.length.units.inferred",
           "astragalus.length", "astragalus.length.units", "astragalus.length.units.inferred",
           "astragalus.width", "astragalus.width.units", "astragalus.width.units.inferred",
           "calcaneus.GB", "calcaneus.GB.units", "calcaneus.GB.units.inferred",
           "calcaneus.GL", "calcaneus.GL.units", "calcaneus.GL.units.inferred",
           "femur.length", "femur.length.units", "femur.length.units.inferred",
           "humerus.length", "humerus.length.units", "humerus.length.units.inferred",
           "forearm.length", "forearm.length.units", "forearm.length.units.inferred",
           "tooth.row", "tooth.row.units", "tooth.row.units.inferred")

##futres
futres.sub <- futres %>%
  select(scientificName, 
         lifeStage, 
         sex,
         catalogNumber,
         materialSampleID,
         locality,
         stateProvince,
         decimalLatitude,
         decimalLongitude,
         elevation,
         mass = Total.Fresh.Weight..g.,
         total.length = TL..mm...Total.Length.,
         tail.length = TA..mm...Tail.Length.,
         hindfoot.length = HF..mm...Hind.Foot.Length.,
         ear.length = En..mm...Ear.Notch...Ear.Length.,
         calcaneus.GL = Calcaneus.GL...greatest.length..von.den.Driesch..1976...mm,
         calcaneus.GB = Calcaneus.GB...greatest.breadth..von.den.Driesch.1976...mm,
         tooth.row = c.toothrow.1.mm,
         gutted = Weight.field.dressed,
         skinned = Weight.Skinned,
         astragalus.length = Astragalus.Length,
         astragalus.width = Astragalus.Width,
         humerus.length = Humerus.Length,
         femur.length = Femur.Length)

##add in missing columns
futres.sub$occurrenceID <- c(1:length(futres.sub$scientificName))
futres.sub$mass.units[!is.na(futres.sub$mass)] <- "g"
futres.sub$mass.units.inferred[!is.na(futres.sub$mass.units)] <- "FALSE"
futres.sub$total.length.units[!is.na(futres.sub$total.length)] <- "mm"
futres.sub$total.length.units.inferred[!is.na(futres.sub$total.length.units)] <- "FALSE"
futres.sub$hindfoot.length.units[!is.na(futres.sub$hindfoot.length)] <- "mm"
futres.sub$hindfoot.length.units.inferred[!is.na(futres.sub$hindfoot.length.units)] <- "FALSE"
futres.sub$calcaneus.GB.units[!is.na(futres.sub$calcaneus.GB)] <- "mm"
futres.sub$calcaneus.GB.units.inferred[!is.na(futres.sub$calcaneus.GB.units)] <- "FALSE"
futres.sub$calcaneus.GL.units[!is.na(futres.sub$calcaneus.GL)] <- "mm"
futres.sub$calcaneus.GL.units.inferred[!is.na(futres.sub$calcaneus.GL.units)] <- "FALSE"
futres.sub$tail.length.units[!is.na(futres.sub$tail.length)] <- "mm"
futres.sub$tail.length.units.inferred[!is.na(futres.sub$tail.length.units)] <- "FALSE"
futres.sub$ear.length.units[!is.na(futres.sub$ear.length)] <- "mm"
futres.sub$ear.length.units.inferred[!is.na(futres.sub$ear.length.units)] <- "FALSE"
futres.sub$gutted.units[!is.na(futres.sub$gutted)] <- "g" 
futres.sub$gutted.units.inferred[!is.na(futres.sub$gutted.units)] <- "FALSE"
futres.sub$skinned.units[!is.na(futres.sub$skinned)] <- "g" 
futres.sub$skinned.units.inferred[!is.na(futres.sub$skinned.units)] <- "FALSE"
futres.sub$astragalus.length.units[!is.na(futres.sub$astragalus.length)] <- "mm"
futres.sub$astragalus.length.units.inferred[!is.na(futres.sub$astragalus.length.units)] <- "FALSE"
futres.sub$astragalus.width.units[!is.na(futres.sub$astragalus.width)] <- "mm"
futres.sub$astragalus.width.units.inferred[!is.na(futres.sub$astragalus.width.units)] <- "FALSE"
futres.sub$femur.length.units[!is.na(futres.sub$femur.length)] <- "mm"
futres.sub$femur.length.units.inferred[!is.na(futres.sub$femur.length.units)] <- "FALSE"
futres.sub$humerus.length.units[!is.na(futres.sub$humerus.length)] <- "mm"
futres.sub$humerus.length.units.inferred[!is.na(futres.sub$humerus.length.units)] <- "FALSE"
futres.sub$forearm.length <- NA
futres.sub$forearm.length.units <- NA
futres.sub$forearm.length.units.inferred <- NA
futres.sub$tooth.row.units[!is.na(futres.sub$tooth.row)] <- "mm"
futres.sub$tooth.row.units.inferred[!is.na(futres.sub$tooth.row.units)] <- "FALSE"

##vertnet
vertnet.sub <- vertnet %>%
  select(scientificName = scientificname, 
         occurrenceID = occurrenceid,
         lifeStage = lifestage_cor, 
         sex,
         locality,
         catalogNumber = catalognumber,
         decimalLatitude = decimallatitude,
         decimalLongitude = decimallongitude,
         mass = body_mass_1.value,
         mass.units = body_mass_1.units,
         mass.units.inferred = body_mass_1.units_inferred,
         total.length = total_length_1.value, 
         total.length.units = total_length_1.units,
         total.length.units.inferred = total_length_1.units_inferred,
         hindfoot.length = hind_foot_length_1.value,
         hindfoot.length.units = hind_foot_length_1.units,
         hindfoot.length.units.inferred = hind_foot_length_1.units_inferred,
         ear.length = ear_length_1.value,
         ear.length.units = ear_length_1.units,
         ear.length.units.inferred = ear_length_1.units_inferred,
         tail.length = tail_length_1.value,
         tail.length.units = tail_length_1.units,
         tail.length.units.inferred = tail_length_1.units_inferred,
         forearm.length = forearm_length_1.value,
         forearm.length.units = forearm_length_1.units,
         forearm.length.units.inferred = forearm_length_1.units_inferred)

vertnet.sub$materialSampleID <- vertnet.sub$occurrenceID
vertnet.sub$elevation <- NA
vertnet.sub$stateProvince <- NA
vertnet.sub$calcaneus.GL <- NA
vertnet.sub$calcaneus.GL.units <- NA
vertnet.sub$calcaneus.GL.units.inferred <- NA
vertnet.sub$calcaneus.GB <- NA
vertnet.sub$calcaneus.GB.units <- NA
vertnet.sub$calcaneus.GB.units.inferred <- NA
vertnet.sub$tooth.row <- NA
vertnet.sub$tooth.row.units <- NA
vertnet.sub$tooth.row.units.inferred <- NA
vertnet.sub$astragalus.length <- NA
vertnet.sub$astragalus.length.units <- NA
vertnet.sub$astragalus.length.units.inferred <- NA
vertnet.sub$astragalus.width <- NA
vertnet.sub$astragalus.width.units <- NA
vertnet.sub$astragalus.width.units.inferred <- NA
vertnet.sub$gutted <- NA
vertnet.sub$gutted.units <- NA
vertnet.sub$gutted.units.inferred <- NA
vertnet.sub$skinned <- NA
vertnet.sub$skinned.units <- NA
vertnet.sub$skinned.units.inferred <- NA
vertnet.sub$femur.length <- NA
vertnet.sub$femur.length.units <- NA
vertnet.sub$femur.length.units.inferred <- NA
vertnet.sub$humerus.length <- NA
vertnet.sub$humerus.length.units <- NA
vertnet.sub$humerus.length.units.inferred <- NA

#reorder columns
futres.sub <- futres.sub[,order]
vertnet.sub <- vertnet.sub[,order]

##comnbine datasets
data <- rbind(vertnet.sub, futres.sub)
length(unique(data$scientificName)) #3491 spp

##create binomials----
data$scientificName <- word(data$scientificName, 1,2, sep = " ") 
data.binom<- data[!grepl('sp.',data$scientificName),]
length(unique(data.binom$scientificName)) #1630
# in mamm: 2766 spp; and now 1738 because got rid of trinomials
#some weird spp. names: e.g.,: (new SW
#will deal with later

##write data file with futres and vertnet combined----
#write.csv(data.binom, "dirty.data.csv")
#data <- read.csv("https://de.cyverse.org/dl/d/73ED75F0-30D2-43FB-8B9E-1F7BC2DFA3F8/dirty.data.csv")

##functions to standardize unit type----

correct.units <- function(data, column.unit, column.infer, values, unit){
  for(i in 1:nrow(data)){
    if(isTRUE(data[,column.unit][i] %in% values)){
      data[,column.unit][i] <- unit 
    }
    else {
      next
    }
  }
  return(data)
}

#apply function for mass, total.length, tail.length, ear.length, forearm.length
grams = c("GRAMS", "GR", "grams", "G", "gm", "gms", "gr", "Grams", "GMS", "['Grams', 'gm']")
millimeters = c("mm_shorthand", "MM", "Millimeters", "['MM', 'mm']")

data.mass <- correct.units(data = data.binom, column.unit = "mass.units", column.infer = "mass.units.inferred", values = grams, unit = "g")
data.total.length <- correct.units(data = data.mass, column.unit = "total.length.units", column.infer = "total.length.units.inferred", values = millimeters, unit = "mm")
data.tail.length <- correct.units(data = data.total.length, column.unit = "tail.length.units", column.infer = "tail.length.units.inferred", values = millimeters, unit = "mm")
data.ear.length <- correct.units(data = data.tail.length, column.unit = "ear.length.units", column.infer = "ear.length.units.inferred", values = millimeters, unit = "mm")
data.forearm.length <- correct.units(data = data.ear.length, column.unit = "forearm.length.units", column.infer = "forearm.length.units.inferred", values = millimeters, unit = "mm")
data.hindfoot.length <- correct.units(data = data.forearm.length, column.unit = "hindfoot.length.units", column.infer = "hindfoot.length.units.inferred", values = millimeters, unit = "mm")

##more data cleaning----
#make measurementValue numeric
data <- data.forearm.length
cols <- c("mass", "skinned", "total.length", "hindfoot.length", "ear.length", "tail.length", "calcaneus.GB", "calcaneus.GL")
data[,cols] <- sapply(data[,cols], as.numeric)


##write datafile with standardization of units----
#write.csv(data, "less.dirty.data.csv")
#data <- read.csv("https://de.cyverse.org/dl/d/293487CC-5E12-4BB3-9EC0-80AA8C2B0562/less.dirty.data.csv")

#make sure every sp. has 10 samples
data_stats <- data %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n())

keep <- data_stats$scientificName[data_stats$sample.size >= 10] #609 sp
data.10 <- data[data$scientificName %in% keep,] #609 spp

##Label OUTLIERS----

#add rownames for indexing
rownames(data.10) <- seq(1, nrow(data.10),1)

#create status columns
data.10$mass.status <- rep("", nrow(data.10))
data.10$total.length.status <- rep("", nrow(data.10))
data.10$tail.length.status <- rep("", nrow(data.10))
data.10$hindfoot.length.status <- rep("", nrow(data.10))
data.10$forearm.length.status <- rep("", nrow(data.10))
data.10$ear.length.status <- rep("", nrow(data.10))

#need to select out trait
#remove NAs from trait
#apply maha() function
#select out indices
#label those rows as outliers

outlier.function <- function(data, threshold, column, vector, trait, units, unit, unit.infer, values, status){
  #data = dataframe of trait values
  #threshold = cutoff criteria in maha()
  #column = how the data should be subsetted (e.g., species, locality); not in quotes
  #vector = a vector of the unique values in the column (e.g., unique(column))
  #trait = trait of interest (e.g., mass, total body length); in quotes
  #unit = unit type to restric it to; not in quotes
  #unit.infer = column for if values were inferred; not in quotes
  #value = True or False or a vector
  #status = outlier status column for the trait; in quotes
  for(i in 1:length(vector)){
    sub <- subset(data, subset = data[,column] == vector[i] & data[,units] == unit & data[,unit.infer] %in% values, select = trait) %>%
      drop_na()
    if(isTRUE(nrow(sub) >= 3)){
      outlier <- maha(sub, cutoff = threshold, rnames = FALSE)
      index <- names(outlier[[2]])
      if(isTRUE(length(index) != 0)){
        data[index,status] <- "outlier"
      }
      else{
        next
      }
    }
    else{
      next
    }
  }
  return(data)
}

sp <- unique(data$scientificName)
values <- c("False", "FALSE")
df.mass <- outlier.function(data = data.10, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "mass", units = "mass.units", unit = "g", unit.infer = "mass.units.inferred", values = values, status = "mass.status")
df.total.length <- outlier.function(data = df.mass, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "total.length", units = "total.length.units", unit = "mm", unit.infer = "total.length.units.inferred", values = values, status = "total.length.status")
#df.hindfoot.length <- outlier.function(data = df.total.length, threshold = 0.95, column = "scientificName", vector = sp, 
#                            trait = "hindfoot.length", units = "hindfoot.length.units", unit = "mm", unit.infer = "hindfoot.length.units.inferred", values = values, status = "hindfoot.length.status")
#hindfoot not working, moving on... 
#NOTE: only doing three traits for now

# Function for upper and lower limits ----
##create function to find upper and lower limit for each measurementType based on non-juveniles, non-inferred units, and correct units ("g" or "mm")

##mean, standard deviation, and upper and lower limit
data <- df.total.length
noJuv <- data[data$lifeStage != "Juvenile",]
length(unique(noJuv$scientificName)) #606

##TO DO
# limits.specific <- function(data, column, trait, units.infer, values, status, unit, amt){
#   data %>%
#     dplyr::group_by(column) %>%
#     dplyr::summarise(sample.size = n(),
#       avg = mean(trait[units.infer %in% values & status != "outlier" & units == unit], na.rm = TRUE),
#       sigma = sd(trait[units.infer %in% values & status != "outlier" & units == unit], na.rm = TRUE),
#       upper.limit = avg + amt*sigma,
#       lower.limit = avg - amt*sigma) %>%
#     as.data.frame()
# }

# values <- c("TRUE", "True")
# data.noJuv.noinfer.mass_stats <-  limits.specific(data = noJuv, column = scientificName, 
#                                                   trait = mass, units.infer = mass.units.inferred,
#                                                   values = values, status = mass.status, unit = "g",
#                                                   amt = 3)

values <- c("FALSE", "False")
data.noJuv.noInfer_stats <- noJuv %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(),
                   avg.mass = mean(mass[mass.units.inferred %in% values & mass.status != "outlier" & mass.units == "g"], na.rm = TRUE),
                   sigma.mass = sd(mass[mass.units.inferred %in% values & mass.status != "outlier" & mass.units == "g"], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (3*sigma.mass),
                   lower.limit.mass = avg.mass - (3*sigma.mass),
                   avg.length = mean(total.length[total.length.units.inferred %in% values & total.length.units == "mm"], na.rm = TRUE),
                   sigma.length = sd(total.length[total.length.units.inferred %in% values & total.length.units == "mm"], na.rm = TRUE),
                   upper.limit.length = avg.length + (3*sigma.length),
                   lower.limit.length = avg.length - (3*sigma.length),
                   avg.hindfoot = mean(hindfoot.length[hindfoot.length.units.inferred %in% values & hindfoot.length.units == "mm"], na.rm = TRUE),
                   sigma.hindfoot = sd(hindfoot.length[hindfoot.length.units.inferred %in% values & hindfoot.length.units == "mm"], na.rm = TRUE),
                   upper.limit.hindfoot = avg.hindfoot + (3*sigma.hindfoot),
                   lower.limit.hindfoot = avg.hindfoot - (3*sigma.hindfoot)) %>%
  as.data.frame()
nrow(data.noJuv.noInfer_stats) #606

##add stats to dataframe
data.check <- data.frame()
for(i in 1:nrow(data.noJuv.noInfer_stats)){
  sub.data <- subset(data, data$scientificName == data.noJuv.noInfer_stats$scientificName[i])
  sub.stats <- subset(data.noJuv.noInfer_stats, data.noJuv.noInfer_stats$scientificName == data.noJuv.noInfer_stats$scientificName[i])
  sub.data$mass.upper.limit <- sub.stats$upper.limit.mass
  sub.data$mass.lower.limit <- sub.stats$lower.limit.mass
  sub.data$total.length.upper.limit <- sub.stats$upper.limit.length
  sub.data$total.length.lower.limit <- sub.stats$lower.limit.length
  sub.data$hindfoot.upper.limit<- sub.stats$upper.limit.hindfoot
  sub.data$hindfoot.lower.limit <- sub.stats$lower.limit.hindfoot
  # sub.data$ear.length.upper.limit <- sub.stats$ear.length.upper.limit
  # sub.data$ear.length.lower.limit <- sub.stats$ear.length.lower.limit
  # sub.data$forearm.length.upper.limit <- sub.stats$forearm.length.upper.limit
  # sub.data$forearm.length.lower.limit <- sub.stats$forearm.length.lower.limit
  # sub.data$tail.length.upper.limit <- sub.stats$tail.length.upper.limit
  # sub.data$tail.length.lower.limit <- sub.stats$tail.length.lower.limit
  data.check <- rbind(data.check, sub.data)
}
length(unique(data.check$scientificName)) #605 - one less species because one species is "NA"

##label outliers
#label samples that are outside of limits with outlier, and label those within limits as "g" and inferred = TRUE
check <- function(data, trait, units, units.infer, status, upper, lower, unit){
  for(i in 1:nrow(data)){
    if(isTRUE(data[i,trait][units != unit] >= data[i,lower] & data[i,trait][units != unit] <= data[i,upper])){
      data[i,units.infer] <- "TRUE"
      data[i,units] <- "g"
      data[i,status] <- "GOOD"
    }
    else if(isTRUE(data[i,trait][units == unit] >= data[i,lower] & data[i,trait][units == unit] <= data[i,upper])){
      data[i,status] <- "GOOD"
    }
    else{
      data[i,status] <- "outlier"
    }
  }
  return(data)
}


data.mass.check <- check(data = data.check, trait = "mass", units = "mass.units", units.infer = "mass.units.inferred", status = "mass.status", upper = "mass.upper.limit", lower = "mass.lower.limit", unit = "g")
data.total.length.check <- check(data = data.mass.check, trait = "total.length", units = "total.length.units", units.infer = "total.length.units.inferred", status = "total.length.status", upper = "total.length.upper.limit", lower = "total.length.lower.limit", unit = "mm")
#data.hindfoot.length.check <- check(data = data.total.length.check, column.value = "hindfoot.length", column.units = "hindfoot.length.units", column.infer = "hindfoot.length.units.inferred", upper = "hindfoot.upper.limit", lower = "hindfoot.lower.limit")
#data.ear.length.check <- check.mm(data = data.hindfoot.length.check, column.value = "ear.length", column.units = "ear.length.units", column.infer = "ear.length.units.inferred", upper = "ear.length.upper.limit", lower = "ear.length.lower.limit")
#data.tail.length.check <- check.mm(data = data.ear.length.check, column.value = "tail.length", column.units = "tail.length.units", column.infer = "tail.length.units.inferred", upper = "tail.length.upper.limit", lower = "tail.length.lower.limit")
#data.forearm.length.check <- check.mm(data = data.tail.length.check, column.value = "forearm.length", column.units = "forearm.length.units", column.infer = "forearm.length.units.inferred", upper = "forearm.length.upper.limit", lower = "forearm.length.lower.limit")

##unit conversion----
#convert units that are wrong (i.e., not "g" or "mm") to proper units

#convert units to "g" or "mm"
convert.g <- function(data, trait, units, units.infer){
  for(i in 1:nrow(data)){
    if(isTRUE(data[i, units] == "mg")){
      data[i, trait] <- data[i, trait] * 1000
      data[i, units] <- "g"
      data[i, units.infer] <- "CONVERTED"
    }
    else if(isTRUE(data[i, units] == "kg")){
      data[i, trait] <- data[i, trait] / 1000
      data[i, units] <- "g"
      data[i, units.infer] <- "CONVERTED"
    }
    else if(isTRUE(data[i, units] == "lb" | data[i, units][i] == "lbs" | data[i, units][i] == "pounds")){
      data[i, trait] <- data[i, trait] / 0.002204623
      data[i, units] <- "g"
      data[i, units.infer] <- "CONVERTED"
    }
    else if(isTRUE(data[i, units] == "oz")){
      data[i, trait] <- data[i, trait] / 0.03527396
      data[i, units] <- "g"
      data[i, units.infer] <- "CONVERTED"
    }
    else{
      next
    }
  }
  return(data)
}

convert.mm <- function(data, trait, units, units.infer){
  for(i in 1:nrow(data)){
    if(isTRUE(data[i, units]== "cm")){
      data[i, trait] <- data[i, trait] / 10
      data[i, units] <- "mm"
      data[i, units.infer] <- "CONVERTED"
    }
    else if(isTRUE(data[i, units] == "in" | data[i, units] == "inches" | data[i, units] == "\"" | data[i, units] == "[\"'\", 'in']")){
      data[i, trait] <- data[i, trait] / 0.03937008
      data[i, units] <- "mm"
      data[i, units.infer] <- "CONVERTED"
    }
    else if(isTRUE(data[i, units] == "Foot" | data[i, units] == "ft" | data[i, units] == "FT" | data[i, units] == "feet" | data[i, units] == "'")){
      data[i, trait] <- data[i, trait] / 0.00328084
      data[i, units] <- "mm"
      data[i, units.infer] <- "CONVERTED"
    }
    else{
      next
    }
  }
  return(data)
}

data.check <- data.total.length.check

data.convert.mass <- convert.g(data = data.check, trait = "mass", units = "mass.units", units.infer = "mass.units.inferred")
data.convert.length <- convert.mm(data = data.convert.mass, trait = "total.length", units = "total.length.units", units.infer = "total.length.units.inferred")
#data.convert.hindfoot <- convert.mm(data = data.convert.length, column.value = "hindfoot.length", column.units = "hindfoot.length.units", column.infer = "hindfoot.length.units.inferred")
#data.convert.ear <- convert.mm(data = data.convert.hindfoot, column.value = "ear.length", column.units = "ear.length.units", column.infer = "ear.length.units.inferred")
#data.convert.tail <- convert.mm(data = data.convert.ear, column.value = "tail.length", column.units = "tail.length.units", column.infer = "tail.length.units.inferred")
#data.convert.forearm <- convert.mm(data = data.convert.tail, column.value = "forearm.length", column.units = "forearm.length.units", column.infer = "forearm.length.units.inferred")

##recalculate upper and lower limits----

data.convert <- data.convert.length

##create new sigma, this time only without juveniles, but allow for inferred units

data.stats <- data.convert[data.convert$lifeStage != "Juvenile",] %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(),
                   avg.mass = mean(mass[mass.status != "outlier"], na.rm = TRUE),
                   sigma.mass = sd(mass[mass.status != "outlier"], na.rm = TRUE),
                   mass.upper.limit = avg.mass + (3*sigma.mass),
                   mass.lower.limit = avg.mass - (3*sigma.mass),
                   avg.length = mean(total.length[total.length.status != "outlier"], na.rm = TRUE),
                   sigma.length = sd(total.length[total.length.status != "outlier"], na.rm = TRUE),
                   total.length.upper.limit = avg.length + (2.5*sigma.length),
                   total.length.lower.limit = avg.length - (2.5*sigma.length)) %>%
  as.data.frame()

#add stats to dataframe
data.recheck <- data.frame()
for(i in 1:nrow(data.stats)){
  sub.data <- subset(data.convert, data.convert$scientificName == data.stats$scientificName[i])
  sub.stats <- subset(data.stats, data.stats$scientificName == data.stats$scientificName[i])
  sub.data$mass.upper.limit <- sub.stats$mass.upper.limit
  sub.data$mass.lower.limit <- sub.stats$mass.lower.limit
  sub.data$total.length.upper.limit <- sub.stats$total.length.upper.limit
  sub.data$total.length.lower.limit <- sub.stats$total.length.lower.limit
  sub.data$hindfoot.length.upper.limit<- sub.stats$hindfoot.length.upper.limit
  sub.data$hindfoot.length.lower.limit <- sub.stats$hindfoot.length.lower.limit
  sub.data$ear.length.upper.limit <- sub.stats$ear.length.upper.limit
  sub.data$ear.length.lower.limit <- sub.stats$ear.length.lower.limit
  sub.data$forearm.length.upper.limit <- sub.stats$forearm.length.upper.limit
  sub.data$forearm.length.lower.limit <- sub.stats$forearm.length.lower.limit
  sub.data$tail.length.upper.limit <- sub.stats$tail.length.upper.limit
  sub.data$tail.length.lower.limit <- sub.stats$tail.length.lower.limit
  data.recheck <- rbind(data.recheck, sub.data)
}

##RE-label outliers----

#ask if is true that mass and length fall in-between upper and lower limit
data.outlier <- data.recheck

# outlier.check <- function(data, trait, lower, upper, status){
#   for(i in nrow(data)){
#     if(isTRUE(data[i, trait] < data[i, lower])){
#       data[i, status] <- "outlier"
#     }
#     else if(isTRUE(data[i, trait] > data[i, upper])){
#       data[i, status] <- "outlier"
#     }
#     else{
#       next
#     }
#   }
# }


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

# for(i in 1:length(data.outlier$scientificName)){
#   if(isTRUE(data.outlier$tail.length[i] < data.outlier$tail.length.lower.limit[i])){
#     data.outlier$tail.length.status[i] <- "outlier"
#   }
#   else if(isTRUE(data.outlier$tail.length[i] > data.outlier$tail.length.upper.limit[i])){
#     data.outlier$tail.length.status[i] <- "outlier"
#   }
#   else{
#     next
#   }
# }
# 
# for(i in 1:length(data.outlier$scientificName)){
#   if(isTRUE(data.outlier$ear.length[i] < data.outlier$ear.length.lower.limit[i])){
#     data.outlier$ear.length.status[i] <- "outlier"
#   }
#   else if(isTRUE(data.outlier$ear.length[i] > data.outlier$ear.length.upper.limit[i])){
#     data.outlier$ear.length.status[i] <- "outlier"
#   }
#   else{
#     next
#   }
# }
# 
# for(i in 1:length(data.outlier$scientificName)){
#   if(isTRUE(data.outlier$hindfoot.length[i] < data.outlier$hindfoot.length.lower.limit[i])){
#     data.outlier$hindfoot.length.status[i] <- "outlier"
#   }
#   else if(isTRUE(data.outlier$hindfoot.length[i] > data.outlier$hindfoot.length.upper.limit[i])){
#     data.outlier$hindfoot.length.status[i] <- "outlier"
#   }
#   else{
#     next
#   }
# }
# 
# for(i in 1:length(data.outlier$scientificName)){
#   if(isTRUE(data.outlier$forearm.length[i] < data.outlier$forearm.length.lower.limit[i])){
#     data.outlier$forearm.length.status[i] <- "outlier"
#   }
#   else if(isTRUE(data.outlier$forearm.length[i] > data.outlier$forearm.length.upper.limit[i])){
#     data.outlier$forearm.length.status[i] <- "outlier"
#   }
#   else{
#     next
#   }
# }

##info about outliers----
outlier_stats <- data.outlier %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.outlier.mass = length(mass[mass.status == "outlier" & mass >= 0]),
                   sample.mass = length(mass[mass.status != "outlier" & mass >= 0]),
                   sample.outlier.total.length = length(total.length[total.length.status == "outlier" & total.length >= 0]),
                   sample.total.length = length(total.length[total.length.status != "outlier" & total.length >= 0])) %>%
                   # sample.outlier.forearm.length = length(forearm.length[forearm.length.status == "outlier" & forearm.length >= 0]),
                   # sample.forearm.length = length(forearm.length[forearm.length.status != "outlier" & forearm.length >= 0]),
                   # sample.outlier.hindfoot.length = length(hindfoot.length[hindfoot.length.status == "outlier" & hindfoot.length >= 0]),
                   # sample.hindfoot.length = length(hindfoot.length[hindfoot.length.status != "outlier" & hindfoot.length >= 0]),
                   # sample.outlier.ear.length = length(ear.length[ear.length.status == "outlier" & ear.length >= 0]),
                   # sample.ear.length = length(ear.length[ear.length.status != "outlier" & ear.length >= 0]),
                   # sample.outlier.tail.length = length(tail.length[tail.length.status == "outlier" & tail.length >= 0]),
                   # sample.tail.length = length(tail.length[tail.length.status != "outlier" & tail.length >= 0])
  as.data.frame()

#write.csv(outlier_stats, "outliers.csv")

##write out clean, labeled data----

data.labeled <- data.outlier
#write.csv(data.labeled, "labeled.clean.data.csv")


##Case studies----
##test out juvenile and adult distributions to narrow down an actual adult distribution

PEMA <- subset(data.clean.10, data.clean.10$scientificName == "Peromyscus maniculatus")

PEMA.clean <- PEMA[!is.na(PEMA$mass),]
PEMA.clean$lifeStage[PEMA.clean$lifeStage != "Juvenile"] <- "Adult"
PEMA.clean$lifeStage[is.na(PEMA.clean$lifeStage)] <- "Adult"

plot_theme <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .75, #adjust as needed
                    axis.text = element_text(size = 21, color = "black"), 
                    axis.ticks.length=unit(0.2,"cm"),
                    axis.title = element_text(size = 21),
                    axis.title.y = element_text(margin = margin(r = 10)),
                    axis.title.x = element_text(margin = margin(t = 10)),
                    axis.title.x.top = element_text(margin = margin(b = 5)),
                    plot.title = element_text(size = 21, face = "plain", hjust = 10),
                    panel.border = element_rect(colour = "black", fill=NA, size=1),
                    panel.background = element_blank(),
                    legend.position = "none",
                    text = element_text(family = 'Helvetica'))
col = c("mediumpurple4", "mediumpurple1")
ggplot() +
  geom_density(data = PEMA.clean, aes(x = log10(mass), fill = PEMA.clean$lifeStage), alpha = 0.7) +
  scale_fill_manual(values = col, 
                    name="Life Stage") +
  plot_theme + theme(legend.position = c(0.85, 0.82))+
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))+
  scale_y_continuous(name = 'Probability')

# PEMA.adult <- subset(PEMA.clean, PEMA.clean$lifeStage != "Juvenile")
# PEMA.juvenile <- subset(PEMA.clean, PEMA.clean$lifeStage == "Juvenile")
# 
# ggplot() + geom_density(data = PEMA.juvenile, aes(x = log10(mass), alpha = 0.7))
# 
# ggplot() + geom_density(data = PEMA.adult, aes(x = log10(mass), alpha = 0.7))
