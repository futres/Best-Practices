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

options(stringsAsFactors = FALSE)

#kitty_deer <- read.csv("EAP Florida Modern Deer Measurements_FORFUTRES_1_23_2020.csv", header = TRUE, stringsAsFactors = FALSE)
#blois_ground.squirrel <- read.csv("J.Biogeo.2008.AllData.Final.csv", header = TRUE, stringsAsFactors = FALSE)
#amelia_impala <- read.csv("Extant Aepyceros database_updated 11_2016.csv", header = TRUE, stringsAsFactors = FALSE)
#bernor_equid <- read.csv("ToFuTRESVER_12_4_16_2020_REV_19.csv", header = TRUE, stringsAsFactors = FALSE)
#cougar_OR <- read.csv("1987-2019 Cougar Weight-Length Public Request.csv", header = TRUE, stringsAsFactors = FALSE)

futres <- read.csv("https://de.cyverse.org/dl/d/B296C95F-56A1-4AB6-9EAA-532CD7B1285B/futres.csv", header = TRUE, stringsAsFactors = FALSE)
#about futres data:
#has various terms for adult and juvenile
#currently aligned manually; fixed Amelia's weight data to be g, deleted lone mass that did not have units
#6 species

## Vertnet data round 2

#bats
bats <- read.csv("https://de.cyverse.org/dl/d/8BE15938-0A21-4712-9FD9-D22B3DF31101/bats_2020-08-11b.csv", header = TRUE)
length(unique(bats$binomial)) #1074
length(unique(bats$occurrenceid)) #74678
nrow(bats) #74678
ncol(bats) #300

#mamm
mamm <- read.csv("https://de.cyverse.org/dl/d/C46CED4D-B974-47CD-8BCF-5A04D6DD642B/no_bats_2020-08-12b.csv", header = TRUE)
length(unique(mamm$binomial)) #4007
length(unique(mamm$occurrenceid)) #584865
nrow(mamm) #584865
ncol(mamm) #368

## Combine VertNet Bat & Mammals data----

setdiff(colnames(mamm), colnames(bats)) #85 different
setdiff(colnames(bats), colnames(mamm)) #17 different

#important column names
imp.cols <- c("binomial", "scientificname", "occurrenceid", "institutioncode", "collectioncode", "catalognumber", "eventdate", 
          "sex", "lifestage_cor", "reproductivecondition", 
          "locality", "highergeography", "continent", "country", "county",
          "decimallatitude", "decimallongitude", "verbatimelevation",
          "waterbody", "island", "islandgroup")

# select out rows that have .1 only, don't need the other values for now
col.pattern <- "1"
V.cols.mamm <- grep(col.pattern, colnames(mamm), value = TRUE)
V.cols.bats <- grep(col.pattern, colnames(bats), value = TRUE)

cols.mamm <- c(imp.cols, V.cols.mamm)
cols.bats <- c(imp.cols, V.cols.bats)

mamm.1 <- mamm[, colnames(mamm) %in% cols.mamm]
bats.1 <- bats[, colnames(bats) %in% cols.bats]

#we don't care about these values related to the measurement/specimen for now
remove.pattern <- "\bhigh|low|ambiguous"
#create pattern
remove.mamm <- grep(remove.pattern, colnames(mamm.1), value = TRUE)
remove.bats <- grep(remove.pattern, colnames(bats.1), value = TRUE)

mamm.2 <- mamm.1[, !(colnames(mamm.1) %in% remove.mamm)]
bats.2 <- bats.1[, !(colnames(bats.1) %in% remove.bats)]

setdiff(colnames(mamm.2), colnames(bats.2))
setdiff(colnames(bats.2), colnames(mamm.2))

mamm.2$forearm_length.1.estimated_value <- NA
mamm.2$tragus_length.1.estimated_value <- NA

extra.cols <- c("placental_scar_count.2.side1", "placental_scar_count.3.side1")
mamm.3 <- mamm.2[, !(colnames(mamm.2) %in% extra.cols)]

bats.2$nipple_count.1.notation <- NA
bats.2$placental_scar_count.1.side1 <- NA
bats.2$placental_scar_count.1.side2 <- NA

setdiff(colnames(mamm.3), colnames(bats.2))
setdiff(colnames(bats.2), colnames(mamm.3))

vertnet <- rbind(mamm.3, bats.2) #5075

write.csv(x = vertnet, file = "vertnet.combined.first.meas.csv")

## VertNet data - OLD----
#bat_mass <- read.csv("https://de.cyverse.org/dl/d/2A542CDF-BDED-4486-AB15-445B53F80F08/vertnet_bats_body_mass_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#706 spp; 18530 rows; 10350 occurrenceids

#get rid of dups
#bat_mass.clean <- bat_mass[!(duplicated(bat_mass$occurrenceid)),]
#706 spp; 10350 rows; 10350 occurrenceids
#8000 diff

#bat_length <- read.csv("https://de.cyverse.org/dl/d/896A54B4-1E52-4976-95AB-71449384B3A4/vertnet_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#696 spp; 16313 rows; 9460 occurrence ids

#get rid of dups
#bat_length.clean <- bat_length[!(duplicated(bat_length$occurrenceid)),]
#696 spp; 9460 rows; 9460 occurrence ids
#6853 diff

#mamm has no bats
#mamm_mass <- read.csv("https://de.cyverse.org/dl/d/EF537422-2246-4B25-A9BC-D8259C78BFA2/vertnet_no_bats_body_mass_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#2439 spp; 225237 rows; 89588 occurrenceids

#get rid of dups
#mamm_mass.clean <- mamm_mass[!(duplicated(mamm_mass$occurrenceid)),]
#2439 spp; 89588 rows; 89588 occurrenceids
#135649 diff

#mamm_length <- read.csv("https://de.cyverse.org/dl/d/DA7E36EF-0008-4DED-A49C-C7DCCC71E98C/vertnet_no_bats_total_len_2020-04-16a_juvAd.csv", header = TRUE, stringsAsFactors = FALSE)
#2728 spp; 245647 rows; 97225 occurrenceids

#get rid of dups
#mamm_length.clean <- mamm_length[!(duplicated(mamm_length$occurrenceid)),]
#2728 spp; 97225 rows; 97225 occurrenceids
#148422 diff

#about VertNet data:
#has adult, juvenile, and NA for lifestage
#has mass in different units, sometimes inferreed

#bernor_equid$binomial <- paste(bernor_equid$GENUS, bernor_equid$SPECIES)
#deal with this once mapped

##VERTNET CLEANING - OLD----

## combine mass & length
#bat data
#bat_length.sub <- subset(bat_length.clean, select = c(scientificname, total_length_1.value, total_length_1.units, occurrenceid))
#bat_mass.sub <- dplyr::select(bat_mass.clean, -c('total_length_1.value', 'total_length_1.units'))
#bats <- full_join(bat_mass.sub, bat_length.sub, by = c("occurrenceid", "scientificname"))
#length(unique(bats$scientificname)) #724; added 28 spp
#10634 occurrenceids; 48657 rows; added 284 occ. ids
#why are there dupes???

#mammal data
#mamm_length.sub <- subset(mamm_length.clean, select = c(scientificname, total_length_1.value, total_length_1.units, occurrenceid))
#mamm_mass.sub <- dplyr::select(mamm_mass.clean, -c('total_length_1.value', 'total_length_1.units'))
#mamm <- full_join(mamm_mass.sub, mamm_length.sub, by = c("occurrenceid", "scientificname"))
#length(unique(mamm$scientificname)) #2766; added 38 spp
#795495 rows; 101440 occ ids; added 11852 occ. ids

##combine
#different number of columns
#300 col in bats, 368 in mamm
#bat.col <- colnames(bats)
#mamm.col <- colnames(mamm)
#diff.col <- !(mamm.col %in% bat.col)
#vertnet <- rbind(bats, mamm) #sum = bats rows + mamm rows

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

col.order <- c("occurrenceID", "scientificName", "lifeStage", "sex", "reproductiveCondition",
           "catalogNumber", "materialSampleID", "institutionCode", "collectionCode", "eventDate",
           "locality", "higherGeography", "continent", "country", "county",
           "waterBody", "island", "islandGroup",
           "decimalLatitude", "decimalLongitude", "elevation",
           "mass", "mass.units", "mass.units.inferred", "mass.estimated.value",
           "gutted", "gutted.units", "gutted.units.inferred",
           "skinned", "skinned.units", "skinned.units.inferred",
           "total.length", "total.length.units", "total.length.units.inferred", "total.length.estimated.value",
           "hindfoot.length", "hindfoot.length.units", "hindfoot.length.units.inferred", "hindfoot.length.estimated.value",
           "ear.length", "ear.length.units", "ear.length.units.inferred", "ear.length.estimated.value",
           "tail.length", "tail.length.units", "tail.length.units.inferred", "tail.length.estimated.value",
           "astragalus.length", "astragalus.length.units", "astragalus.length.units.inferred", "astragalus.length.estimated.value",
           "astragalus.width", "astragalus.width.units", "astragalus.width.units.inferred", "astragalus.width.estimated.value",
           "calcaneus.GB", "calcaneus.GB.units", "calcaneus.GB.units.inferred", "calcaneus.GB.estimated.value",
           "calcaneus.GL", "calcaneus.GL.units", "calcaneus.GL.units.inferred", "calcaneus.GL.estimated.value",
           "femur.length", "femur.length.units", "femur.length.units.inferred", "femur.length.estimated.value",
           "humerus.length", "humerus.length.units", "humerus.length.units.inferred", "humerus.length.estimated.value",
           "forearm.length", "forearm.length.units", "forearm.length.units.inferred", "forearm.length.estimated.value",
           "tooth.row", "tooth.row.units", "tooth.row.units.inferred", "tooth.row.estimated.value")

##futres
futres.sub <- futres %>%
  dplyr::select(scientificName, 
         lifeStage, 
         sex,
         reproductiveCondition,
         catalogNumber,
         materialSampleID,
         eventDate,
         locality,
         country, 
         county,
         decimalLatitude,
         decimalLongitude,
         elevation,
         mass = Total.Fresh.Weight..g.,
         total.length = TL..mm...Total.Length.,
         tail.length = TA..mm...Tail.Length.,
         hindfoot.length = HF..mm...Hind.Foot.Length.,
         ear.length = En..mm...Ear.Notch...Ear.Length.,
         calcaneus.GL = Calcaneus.GL...greatest.length..von.den.Driesch..1976...mm,
         calcaneus.GB =Calcaneus.GB...greatest.breadth..von.den.Driesch.1976...mm,
         tooth.row = c.toothrow.1.mm,
         gutted = Weight.field.dressed,
         skinned = Weight.Skinned,
         astragalus.length = Astragalus.Length,
         astragalus.width = Astragalus.Width,
         humerus.length = Humerus.Length,
         femur.length = Femur.Length) %>%
  mutate_at(c("elevation", "mass", 
               "total.length", "tail.length", "hindfoot.length", "ear.length",
               "calcaneus.GB", "calcaneus.GL", "tooth.row",
               "gutted", "skinned", "astragalus.length", "astragalus.width",
               "humerus.length", "femur.length"), as.numeric)

##add in missing columns
emptyvector <- c("institutionCode", "collectionCode", "higherGeography",
                "waterBody", "island", "islandGroup", "continent", 
                "forearm.length", "forearm.length.units", "forearm.length.units.inferred",
                "forearm.length.estimated.value", "tooth.row.estimated.value",
                "humerus.length.estimated.value")
futres.sub[ , emptyvector] <- ""

futres.sub <- futres.sub %>% mutate_at(c("forearm.length"), as.numeric)

falsevector <- c("mass.estimated.value", "total.length.estimated.value", "hindfoot.length.estimated.value",
                 "calcaneus.GB.estimated.value", "calcaneus.GL.estimated.value",
                 "tail.length.estimated.value", "ear.length.estimated.value",
                 "astragalus.length.estimated.value", "astragalus.width.estimated.value",
                 "femur.length.estimated.value", "tooth.row.estimated.value")
futres.sub[ , falsevector] <- "FALSE"

futres.sub$occurrenceID <- as.character(c(1:length(futres.sub$scientificName)))
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
futres.sub$tooth.row.units[!is.na(futres.sub$tooth.row)] <- "mm"
futres.sub$tooth.row.units.inferred[!is.na(futres.sub$tooth.row.units)] <- "FALSE"

##vertnet
vertnet.sub <- vertnet %>%
  dplyr::select(scientificName = scientificname, 
         occurrenceID = occurrenceid,
         lifeStage = lifestage_cor, 
         sex,
         locality,
         elevation = verbatimelevation,
         catalogNumber = catalognumber,
         reproductiveCondition = reproductivecondition,
         decimalLatitude = decimallatitude,
         decimalLongitude = decimallongitude,
         continent,
         country,
         county,
         eventDate = eventdate,
         institutionCode = institutioncode,
         collectionCode = collectioncode,
         higherGeography = highergeography,
         waterBody = waterbody,
         island,
         islandGroup = islandgroup,
         mass = body_mass.1.value,
         mass.units = body_mass.1.units,
         mass.units.inferred = body_mass.1.units_inferred,
         mass.estimated.value = body_mass.1.estimated_value,
         total.length = total_length.1.value, 
         total.length.units = total_length.1.units,
         total.length.units.inferred = total_length.1.units_inferred,
         total.length.estimated.value = total_length.1.estimated_value,
         hindfoot.length = hind_foot_length.1.value,
         hindfoot.length.units = hind_foot_length.1.units,
         hindfoot.length.units.inferred = hind_foot_length.1.units_inferred,
         hindfoot.length.estimated.value = hind_foot_length.1.estimated_value,
         ear.length = ear_length.1.value,
         ear.length.units = ear_length.1.units,
         ear.length.units.inferred = ear_length.1.units_inferred,
         ear.length.estimated.value = ear_length.1.estimated_value,
         tail.length = tail_length.1.value,
         tail.length.units = tail_length.1.units,
         tail.length.units.inferred = tail_length.1.units_inferred,
         tail.length.estimated.value = tail_length.1.estimated_value,
         forearm.length = forearm_length.1.value,
         forearm.length.units = forearm_length.1.units,
         forearm.length.units.inferred = forearm_length.1.units_inferred,
         forearm.length.estimated.value = forearm_length.1.estimated_value) %>%
  mutate_at(c("elevation", "mass", "total.length", "hindfoot.length", "ear.length",
              "tail.length", "forearm.length"), as.numeric)

empty.vector <- c("calcaneus.GL", "calcaneus.GL.units", "calcaneus.GL.units.inferred", "calcaneus.GL.estimated.value",
                "calcaneus.GB", "calcaneus.GB.units", "calcaneus.GB.units.inferred", "calcaneus.GB.estimated.value",
                "tooth.row", "tooth.row.units", "tooth.row.units.inferred", "tooth.row.estimated.value",
                "astragalus.length", "astragalus.length.units", "astragalus.length.units.inferred", "astragalus.length.estimated.value",
                "astragalus.width", "astragalus.width.units", "astragalus.width.units.inferred", "astragalus.width.estimated.value",
                "gutted", "gutted.units", "gutted.units.inferred",
                "skinned", "skinned.units", "skinned.units.inferred",
                "femur.length", "femur.length.units", "femur.length.units.inferred", "femur.length.estimated.value",
                "humerus.length", "humerus.length.units", "humerus.length.units.inferred", "humerus.length.estimated.value")
vertnet.sub[ , empty.vector] <- ""

vertnet.sub <- vertnet.sub %>% mutate_at(c("calcaneus.GL", "calcaneus.GB", "tooth.row", 
                          "astragalus.length", "astragalus.width",
                          "gutted", "skinned", "femur.length", "humerus.length"),
                          as.numeric)

vertnet.sub$materialSampleID <- vertnet.sub$occurrenceID

#check that columns are the same
setdiff(colnames(futres.sub), colnames(vertnet.sub))
setdiff(colnames(vertnet.sub), colnames(futres.sub))
setdiff(col.order, colnames(vertnet.sub))
setdiff(col.order, colnames(futres.sub))

##comnbine datasets
futres.order <- futres.sub[, col.order]
vertnet.order <- vertnet.sub[, col.order]

data <- rbind(futres.order, vertnet.order)
length(unique(data$scientificName)) #10329 spp; must be a lot of trinomials...

##create binomials----
data$scientificName <- word(data$scientificName, 1,2, sep = " ") 
data.binom<- data[!grepl('sp.', data$scientificName),]
length(unique(data.binom$scientificName)) #4348
# in mamm: 2766 spp; and now 1738 because got rid of trinomials
data.binom <- data.binom[!is.na(data.binom$scientificName),]
data.binom <- data.binom[data.binom$scientificName != "(new SW",]
length(unique(data.binom$scientificName)) #4346

##write data file with futres and vertnet combined----
write.csv(data.binom, "dirty.data.csv")

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
millimeters = c("mm_shorthand", "MM", "Millimeters", "['MM', 'mm']", "('MM', 'mm')", "('Millimeters', 'mm')")

data.mass <- correct.units(data = data.binom, column.unit = "mass.units", column.infer = "mass.units.inferred", values = grams, unit = "g")
data.total.length <- correct.units(data = data.mass, column.unit = "total.length.units", column.infer = "total.length.units.inferred", values = millimeters, unit = "mm")
data.tail.length <- correct.units(data = data.total.length, column.unit = "tail.length.units", column.infer = "tail.length.units.inferred", values = millimeters, unit = "mm")
data.ear.length <- correct.units(data = data.tail.length, column.unit = "ear.length.units", column.infer = "ear.length.units.inferred", values = millimeters, unit = "mm")
data.forearm.length <- correct.units(data = data.ear.length, column.unit = "forearm.length.units", column.infer = "forearm.length.units.inferred", values = millimeters, unit = "mm")
data.hindfoot.length <- correct.units(data = data.forearm.length, column.unit = "hindfoot.length.units", column.infer = "hindfoot.length.units.inferred", values = millimeters, unit = "mm")

##more data cleaning----
#make measurementValue numeric
data <- data.hindfoot.length
cols <- c("mass", "skinned", "total.length", "hindfoot.length", "ear.length", "tail.length", "calcaneus.GB", "calcaneus.GL")
data[,cols] <- sapply(data[,cols], as.numeric)

##write datafile with standardization of units----
write.csv(data, "less.dirty.data.csv")

##Figure
df <- subset(data.hindfoot.length, scientificName == "Artibeus jamaicensis")
length(df$mass[!is.na(df$mass)]) #1406
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = lifeStage), alpha = 0.7) +
  ggtitle("Artibeus jamaicensis N=1406") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("orig.dist.lifeStage.bat",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.hindfoot.length, scientificName == "Peromyscus maniculatus")
length(df$mass[!is.na(df$mass)])
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = lifeStage), alpha = 0.7) +
  ggtitle("Peromyscus maniculatus N=31669") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("orig.dist.lifeStage.mouse",".png"), width = 14, height = 10, units = "cm")

##Label OUTLIERS----

#add rownames for indexing
rownames(data) <- seq(1, nrow(data),1)

#create status columns
data$mass.status <- rep("", nrow(data))
data$total.length.status <- rep("", nrow(data))
data$tail.length.status <- rep("", nrow(data))
data$hindfoot.length.status <- rep("", nrow(data))
data$forearm.length.status <- rep("", nrow(data))
data$ear.length.status <- rep("", nrow(data))

#need to select out trait
#remove NAs from trait
#apply maha() function
#select out indices
#label those rows as outliers

data.noJuv <- subset(data, data$lifeStage != "Juvenile")
data.Juv <- subset(data, data$lifeStage == "Juvenile")

##NOTE: this function on works on spp. with more than ten samples.
outlier.function <- function(data, threshold, column, vector, trait, units, unit, unit.infer, values, status){
  #data = dataframe of trait values
  #threshold = cutoff criteria in maha()
  #column = how the data should be subsetted (e.g., species, locality); not in quotes
  #vector = a vector of the unique values in the column (e.g., unique(column))
  #trait = trait of interest (e.g., mass, total body length); in quotes
  #unit = unit type to restric it to; not in quotes
  #unit.infer = column for if values were inferred; not in quotes
  #value = True or False or a vector for if units inferred
  #status = outlier status column for the trait; in quotes
  for(i in 1:length(vector)){
    sub <- subset(data, subset = data[,column] == vector[i] & data[,units] == unit & data[,unit.infer] %in% values, select = trait) %>%
      drop_na()
    if(isTRUE(nrow(sub) >= 10)){
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

sp <- unique(data.noJuv$scientificName)
values <- c("False", "FALSE")
df.mass <- outlier.function(data = data.noJuv, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "mass", units = "mass.units", unit = "g", unit.infer = "mass.units.inferred", values = values, status = "mass.status")
df.total.length <- outlier.function(data = df.mass, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "total.length", units = "total.length.units", unit = "mm", unit.infer = "total.length.units.inferred", values = values, status = "total.length.status")
df.hindfoot.length <- outlier.function(data = df.total.length, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "hindfoot.length", units = "hindfoot.length.units", unit = "mm", unit.infer = "hindfoot.length.units.inferred", values = values, status = "hindfoot.length.status")
df.ear.length <- outlier.function(data = df.hindfoot.length, threshold = 0.95, column = "scientificName", vector = sp,
                                  trait = "ear.length", units = "ear.length.units", unit = "mm", unit.infer = "ear.length.units.inferred", values = values, status = "ear.length.status")
#df.tail.length <- outlier.function(data = df.ear.length, threshold = 0.95, column = "scientificName", vector = sp,
#                                   trait = "tail.length", units = "tail.length.units", unit = "mm", unit.infer = "tail.length.units.inferred", values = values, status = "tail.length.status")

#NOTE: tail length not working

mean(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus"], na.rm = TRUE) #102.73
length(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus" & !is.na(data.noJuv$mass)]) #30717
mean(df.ear.length$mass[df.ear.length$scientificName == "Peromyscus maniculatus" & df.ear.length$mass.status != "outlier"], na.rm = TRUE) #26.02
length(df.ear.length$mass[df.ear.length$scientificName == "Peromyscus maniculatus" & df.ear.length$mass.status != "outlier" & !is.na(df.ear.length$mass)]) #30716
mean(df.ear.length$mass[df.ear.length$scientificName == "Peromyscus maniculatus" & df.ear.length$mass.status == "outlier"], na.rm = TRUE) #26.02
length(df.ear.length$mass[df.ear.length$scientificName == "Peromyscus maniculatus" & df.ear.length$mass.status == "outlier" & !is.na(df.ear.length$mass)]) #30716

mean(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis"], na.rm = TRUE) #41.08755
length(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis" & !is.na(data.noJuv$mass)]) #1394
mean(df.ear.length$mass[df.ear.length$scientificName == "Artibeus jamaicensis" & df.ear.length$mass.status != "outlier"], na.rm = TRUE) #40.50593
length(df.ear.length$mass[df.ear.length$scientificName == "Artibeus jamaicensis" & df.ear.length$mass.status != "outlier" & !is.na(df.ear.length$mass)]) #1324

# Function for upper and lower limits ----
##create function to find upper and lower limit for each measurementType based on non-juveniles, non-inferred units, and correct units ("g" or "mm")

##mean, standard deviation, and upper and lower limit
data <- rbind(df.ear.length, data.Juv)
length(unique(data$scientificName)) #4346

df <- subset(data, data$scientificName == "Artibeus jamaicensis" & data$lifeStage != "Juvenile" & !is.na(data$mass))
length(df$mass)
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  ggtitle("Artibeus jamaicensis N=1394") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("outlier.test.bat",".png"), width = 14, height = 10, units = "cm")

df <- subset(data, data$scientificName == "Peromyscus maniculatus" & data$lifeStage != "Juvenile" & !is.na(data$mass))
length(df$mass)
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  ggtitle("Peromyscus maniculatus N=30717") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("outlier.test.mouse",".png"), width = 14, height = 10, units = "cm")

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
data.noJuv.noInfer_stats <- data %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(avg.mass = mean(mass[mass.units.inferred %in% values & mass.status != "outlier" & mass.units == "g" & lifeStage != "Juvenile" & mass.status != "not checked"], na.rm = TRUE),
                   sigma.mass = sd(mass[mass.units.inferred %in% values & mass.status != "outlier" & mass.units == "g" & lifeStage != "Juvenile" & mass.status != "not checked"], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (3*sigma.mass),
                   lower.limit.mass = avg.mass - (3*sigma.mass),
                   avg.length = mean(total.length[total.length.units.inferred %in% values & total.length.units == "mm" & lifeStage != "Juvenile" & total.length.status != "not checked"], na.rm = TRUE),
                   sigma.length = sd(total.length[total.length.units.inferred %in% values & total.length.units == "mm" & lifeStage != "Juvenile" & total.length.status != "not checked"], na.rm = TRUE),
                   upper.limit.length = avg.length + (3*sigma.length),
                   lower.limit.length = avg.length - (3*sigma.length),
                   avg.hindfoot = mean(hindfoot.length[hindfoot.length.units.inferred %in% values & hindfoot.length.units == "mm" & lifeStage != "Juvenile" & hindfoot.length.status != "not checked"], na.rm = TRUE),
                   sigma.hindfoot = sd(hindfoot.length[hindfoot.length.units.inferred %in% values & hindfoot.length.units == "mm" & lifeStage != "Juvenile" & hindfoot.length.status != "not checked"], na.rm = TRUE),
                   upper.limit.hindfoot = avg.hindfoot + (3*sigma.hindfoot),
                   lower.limit.hindfoot = avg.hindfoot - (3*sigma.hindfoot),
                   avg.ear.length = mean(ear.length[ear.length.units.inferred %in% values & ear.length.units == "mm" & lifeStage != "Juvenile" & ear.length.status != "not checked"], na.rm = TRUE),
                   sigma.ear = sd(ear.length[ear.length.units.inferred %in% values & ear.length.units == "mm" & lifeStage != "Juvenile" & ear.length.status != "not checked"], na.rm = TRUE),
                   upper.limit.ear = avg.ear.length + (3*sigma.ear),
                   lower.limit.ear = avg.ear.length - (3*sigma.ear),
                   avg.forearm.length = mean(forearm.length[forearm.length.units.inferred %in% values & forearm.length.units == "mm" & lifeStage != "Juvenile" & forearm.length.status != "not checked"], na.rm = TRUE),
                   sigma.forearm = sd(forearm.length[forearm.length.units.inferred %in% values & forearm.length.units == "mm" & lifeStage != "Juvenile" & forearm.length.status != "not checked"], na.rm = TRUE),
                   upper.limit.forearm = avg.forearm.length + (3*sigma.forearm),
                   lower.limit.forearm = avg.forearm.length - (3*sigma.forearm),
                   avg.tail.length = mean(tail.length[tail.length.units.inferred %in% values & tail.length.units == "mm" & lifeStage != "Juvenile" & tail.length.status != "not checked"], na.rm = TRUE),
                   sigma.tail = sd(tail.length[tail.length.units.inferred %in% values & tail.length.units == "mm" & lifeStage != "Juvenile" & tail.length.status != "not checked"], na.rm = TRUE),
                   upper.limit.tail = avg.tail.length + (3*sigma.tail),
                   lower.limit.tail = avg.tail.length - (3*sigma.tail)) %>%
  as.data.frame()
nrow(data.noJuv.noInfer_stats) #4346

##add stats to dataframe
data.limit <- merge(data, data.noJuv.noInfer_stats, by = "scientificName", all.x = TRUE, all.y = FALSE)
length(unique(data.limit$scientificName)) #4346

data.noJuv <- data.limit[data.limit$lifeStage != "Juvenile",]
data.juv <- data.limit[data.limit$lifeStage == "Juvenile",]

##label outliers
#label samples that are outside of limits with outlier, and label those within limits as "g" and inferred = TRUE
check <- function(data, trait, units, units.infer, status, upper, lower, unit){
  for(i in 1:nrow(data)){
    if(isTRUE(data[i,trait][units != unit] >= data[i,lower] & data[i,trait][units != unit] <= data[i,upper])){
      data[i,units.infer] <- "TRUE"
      data[i,units] <- unit
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

data.check <- data.noJuv
data.mass.check <- check(data = data.check, trait = "mass", units = "mass.units", units.infer = "mass.units.inferred", status = "mass.status", upper = "mass.upper.limit", lower = "mass.lower.limit", unit = "g")
data.total.length.check <- check(data = data.mass.check, trait = "total.length", units = "total.length.units", units.infer = "total.length.units.inferred", status = "total.length.status", upper = "total.length.upper.limit", lower = "total.length.lower.limit", unit = "mm")
data.hindfoot.length.check <- check(data  = data.total.length.check, trait = "hindfoot.length", units = "hindfoot.length.units", units.infer = "hindfoot.length.units.inferred", status = "hindfoot.length.status", upper = "hindfoot.upper.limit", lower = "hindfoot.lower.limit", unit = "mm")
data.ear.length.check <- check(data = data.hindfoot.length.check, trait = "ear.length", units = "ear.length.units", units.infer = "ear.length.units.inferred", status = "ear.length.status", upper = "ear.length.upper.limit", lower = "ear.length.lower.limit", unit = "mm")
data.tail.length.check <- check(data = data.ear.length.check, trait = "tail.length", units = "tail.length.units", units.infer = "tail.length.units.inferred", status = "tail.length.status", upper = "tail.length.upper.limit", lower = "tail.length.lower.limit", unit = "mm")
data.forearm.length.check <- check(data = data.tail.length.check, trait = "forearm.length", units = "forearm.length.units", units.infer = "forearm.length.units.inferred", status = "forearm.length.status", upper = "forearm.length.upper.limit", lower = "forearm.length.lower.limit", unit = "mm")

data <- rbind(data.forearm.length.check, data.juv)

df <- subset(data, data$scientificName  == "Artibeus jamaicensis" & data$lifeStage != "Juvenile" & !is.na(data$mass))
length(df$mass)
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(df$mass), fill = df$mass.status), alpha = 0.7) +
  ggtitle("Artibeus jamaicensis N=1394") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("second.outlier.test.bat",".png"), width = 14, height = 10, units = "cm")

df <- subset(data, data$scientificName  == "Peromyscus maniculatus" & data$lifeStage != "Juvenile" & !is.na(data$mass))
length(df$mass)
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(df$mass), fill = df$mass.status), alpha = 0.7) +
  ggtitle("Peromyscus maniculatus N=30717") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("second.outlier.test",".png"), width = 14, height = 10, units = "cm")


mean(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus" & data.noJuv$mass.status != "outlier"], na.rm = TRUE) #26.02
length(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus" & data.noJuv$mass.status != "outlier" & !is.na(data.noJuv$mass)])
mean(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Peromyscus maniculatus" & data.forearm.length.check$mass.status == "GOOD"], na.rm = TRUE)
length(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Peromyscus maniculatus" & data.forearm.length.check$mass.status == "GOOD"])
mean(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Peromyscus maniculatus" & data.forearm.length.check$mass.status != "outlier"], na.rm = TRUE)
length(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Peromyscus maniculatus" & data.forearm.length.check$mass.status != "outlier"])

mean(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis" & data.noJuv$mass.status != "outlier"], na.rm = TRUE)
length(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis" & data.noJuv$mass.status != "outlier" & !is.na(data.noJuv$mass)])
mean(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Artibeus jamaicensis" & data.forearm.length.check$mass.status == "GOOD"], na.rm = TRUE)
length(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Artibeus jamaicensis" & data.forearm.length.check$mass.status == "GOOD" & !is.na(data.forearm.length$mass)])
mean(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Artibeus jamaicensis" & data.forearm.length.check$mass.status != "outlier"], na.rm = TRUE)
length(data.forearm.length.check$mass[data.forearm.length.check$scientificName == "Artibeus jamaicensis" & data.forearm.length.check$mass.status != "outlier" & !is.na(data.forearm.length$mass)])

##unit conversion----
#convert units that are wrong (i.e., not "g" or "mm") to proper units

data.noJuv <- subset(data, data$lifeStage != "Juvenile")
data.Juv <- subset(data, data$lifeStage == "Juvenile")

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
    if(isTRUE(data[i, units] == "cm" | data[i, units] == "CM")){
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

data.check <- data.noJuv
data.convert.mass <- convert.g(data = data.check, trait = "mass", units = "mass.units", units.infer = "mass.units.inferred")
data.convert.length <- convert.mm(data = data.convert.mass, trait = "total.length", units = "total.length.units", units.infer = "total.length.units.inferred")
data.convert.hindfoot <- convert.mm(data = data.convert.length, trait = "hindfoot.length", units = "hindfoot.length.units", units.infer = "hindfoot.length.units.inferred")
data.convert.ear <- convert.mm(data = data.convert.hindfoot, trait = "ear.length", units = "ear.length.units", units.infer = "ear.length.units.inferred")
data.convert.tail <- convert.mm(data = data.convert.ear, trait = "tail.length", units = "tail.length.units", units.infer = "tail.length.units.inferred")
data.convert.forearm <- convert.mm(data = data.convert.tail, trait = "forearm.length", units = "forearm.length.units",units.infer = "forearm.length.units.inferred")

##recalculate upper and lower limits----

data.convert <- rbind(data.convert.forearm, data.Juv)

df <- subset(data.convert, data.convert$scientificName  == "Artibeus jamaicensis" & lifeStage != "Juvenile" & !is.na(data.convert$mass))
length(df$mass) #1394
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  ggtitle("Artibeus jamaicensis N=1394") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("convert.units.bat",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.convert, data.convert$scientificName  == "Peromyscus maniculatus" & lifeStage != "Juvenile" & !is.na(data.convert$mass))
length(df$mass) #30717
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  ggtitle("Peromyscus maniculatus") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("convert.units.mouse",".png"), width = 14, height = 10, units = "cm")

mean(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus" & data.noJuv$mass.status != "outlier"], na.rm = TRUE)
length(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus" & data.noJuv$mass.status != "outlier" & !is.na(data.noJuv$mass.status)])
mean(data.convert.forearm$mass[data.convert.forearm$scientificName == "Peromyscus maniculatus" & data.convert.forearm$mass.status != "outlier"], na.rm = TRUE)
length(data.convert.forearm$mass[data.convert.forearm$scientificName == "Peromyscus maniculatus" & data.convert.forearm$mass.status != "outlier" & !is.na(data.covert.forearm$mass)])

mean(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis" & data.noJuv$mass.status != "outlier"], na.rm = TRUE)
length(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis" & data.noJuv$mass.status != "outlier" & !is.na(data.noJuv$mass)])
mean(data.convert.forearm$mass[data.convert.forearm$scientificName == "Artibeus jamaicensis" & data.convert.forearm$mass.status != "outlier"], na.rm = TRUE)
length(data.convert.forearm$mass[data.convert.forearm$scientificName == "Artibeus jamaicensis" & data.convert.forearm$mass.status != "outlier" & !is.na(data.convert.forearm$mass)])

##create new sigma, this time only without juveniles, but allow for inferred units

data.stats <- data.convert %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n(),
                   avg.mass = mean(mass[mass.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.mass = sd(mass[mass.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   mass.upper.limit = avg.mass + (3*sigma.mass),
                   mass.lower.limit = avg.mass - (3*sigma.mass),
                   avg.length = mean(total.length[total.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.length = sd(total.length[total.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   total.length.upper.limit = avg.length + (2.5*sigma.length),
                   total.length.lower.limit = avg.length - (2.5*sigma.length),
                   avg.ear.length = mean(ear.length[ear.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.ear = sd(ear.length[ear.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.ear = avg.ear.length + (2.5*sigma.ear),
                   lower.limit.ear = avg.ear.length - (2.5*sigma.ear),
                   avg.hindfoot = mean(hindfoot.length[hindfoot.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.hindfoot = sd(hindfoot.length[hindfoot.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.hindfoot = avg.hindfoot + (2.5*sigma.length),
                   lower.limit.hindfoot = avg.hindfoot - (2.5*sigma.length),
                   avg.tail = mean(tail.length[tail.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.tail = sd(tail.length[tail.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.tail = avg.length + (2.5*sigma.length),
                   lower.limit.tail = avg.length - (2.5*sigma.length)) %>%
  as.data.frame()

#add stats to dataframe
data.recheck <- merge(data.convert, data.stats, by = "scientificName", all.x = TRUE, all.y = FALSE)

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
  if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$mass[i] < data.outlier$mass.lower.limit[i])){
    data.outlier$mass.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$mass[i] > data.outlier$mass.upper.limit[i])){
    data.outlier$mass.status[i] <- "outlier"
  }
  else{
    next
  }
}

for(i in 1:length(data.outlier$scientificName)){
  if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$total.length[i] < data.outlier$length.lower.limit[i])){
    data.outlier$length.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$total.length[i] > data.outlier$length.upper.limit[i])){
    data.outlier$length.status[i] <- "outlier"
  }
  else{
    next
  }
}

for(i in 1:length(data.outlier$scientificName)){
  if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$tail.length[i] < data.outlier$tail.length.lower.limit[i])){
    data.outlier$tail.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$tail.length[i] > data.outlier$tail.length.upper.limit[i])){
    data.outlier$tail.length.status[i] <- "outlier"
  }
  else{
    next
  }
}

for(i in 1:length(data.outlier$scientificName)){
  if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$ear.length[i] < data.outlier$ear.length.lower.limit[i])){
    data.outlier$ear.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$ear.length[i] > data.outlier$ear.length.upper.limit[i])){
    data.outlier$ear.length.status[i] <- "outlier"
  }
  else{
    next
  }
}

for(i in 1:length(data.outlier$scientificName)){
  if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$hindfoot.length[i] < data.outlier$hindfoot.length.lower.limit[i])){
    data.outlier$hindfoot.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$hindfoot.length[i] > data.outlier$hindfoot.length.upper.limit[i])){
    data.outlier$hindfoot.length.status[i] <- "outlier"
  }
  else{
    next
  }
}

for(i in 1:length(data.outlier$scientificName)){
  if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$forearm.length[i] < data.outlier$forearm.length.lower.limit[i])){
    data.outlier$forearm.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$forearm.length[i] > data.outlier$forearm.length.upper.limit[i])){
    data.outlier$forearm.length.status[i] <- "outlier"
  }
  else{
    next
  }
}

df < subset(data.outlier, scientificName == "Artibeus jamaicensis" & lifeStage != "Juvenile" & !is.na(data.outlier$mass))
length(df$mass)
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  ggtitle("Artibeus jamaicensis") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("third.outlier.test.bat",".png"), width = 14, height = 10, units = "cm")

df < subset(data.outlier, scientificName == "Peromyscus maniculatus" & lifeStage != "Juvenile" & !is.na(data.outlier$mass))
length(df$mass)
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  ggtitle("Peromyscus maniculatus") +
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("third.outlier.test.mouse",".png"), width = 14, height = 10, units = "cm")

mean(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus" & data.noJuv$mass.status != "outlier"], na.rm = TRUE)
length(data.noJuv$mass[data.noJuv$scientificName == "Peromyscus maniculatus" & data.noJuv$mass.status != "outlier" & !is.na(data.noJuv$mass)])
mean(data.outlier$mass[data.outlier$scientificName == "Peromyscus maniculatus" & data.outlier$mass.status != "outlier"], na.rm = TRUE)
length(data.outlier$mass[data.outlier$scientificName == "Peromyscus maniculatus" & data.outlier$mass.status != "outlier" & !is.na(data.outlier$mass)])

mean(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis" & data.noJuv$mass.status != "outlier"], na.rm = TRUE)
length(data.noJuv$mass[data.noJuv$scientificName == "Artibeus jamaicensis" & data.noJuv$mass.status != "outlier" & !is.na(data.noJuv$mass)])
mean(data.outlier$mass[data.outlier$scientificName == "Artibeus jamaicensis" & data.outlier$mass.status != "outlier"], na.rm = TRUE)
length(data.outlier$mass[data.outlier$scientificName == "Artibeus jamaicensis" & data.outlier$mass.status != "outlier" & !is.na(data.outlier$mass)])

##info about outliers----
outlier_stats <- data.outlier %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.outlier.mass = length(mass[mass.status == "outlier" & mass >= 0 & lifeStage != "Juvenile"]),
                   sample.mass = length(mass[mass.status != "outlier" & mass >= 0 & lifeStage != "Juvenile"]),
                   sample.outlier.total.length = length(total.length[total.length.status == "outlier" & total.length >= 0 & lifeStage != "Juvenile"]),
                   sample.total.length = length(total.length[total.length.status != "outlier" & total.length >= 0 & lifeStage != "Juvenile"]),
                   sample.outlier.forearm.length = length(forearm.length[forearm.length.status == "outlier" & forearm.length >= 0 & lifeStage != "Juvenile"]),
                   sample.forearm.length = length(forearm.length[forearm.length.status != "outlier" & forearm.length >= 0 & lifeStage != "Juvenile"]),
                   sample.outlier.hindfoot.length = length(hindfoot.length[hindfoot.length.status == "outlier" & hindfoot.length >= 0 & lifeStage != "Juvenile"]),
                   sample.hindfoot.length = length(hindfoot.length[hindfoot.length.status != "outlier" & hindfoot.length >= 0 & lifeStage != "Juvenile"]),
                   sample.outlier.ear.length = length(ear.length[ear.length.status == "outlier" & ear.length >= 0 & lifeStage != "Juvenile"]),
                   sample.ear.length = length(ear.length[ear.length.status != "outlier" & ear.length >= 0 & lifeStage != "Juvenile"]),
                   sample.outlier.tail.length = length(tail.length[tail.length.status == "outlier" & tail.length >= 0 & lifeStage != "Juvenile"]),
                   sample.tail.length = length(tail.length[tail.length.status != "outlier" & tail.length >= 0 & lifeStage != "Juvenile"])) %>%
  as.data.frame()

write.csv(outlier_stats, "outliers.csv")

##write out clean, labeled data----

data.labeled <- data.outlier
write.csv(data.labeled, "labeled.clean.data.csv")

##Juvenile test with PEMA----
PEMA <- data.labeled[data.labeled$scientificName == "Peromyscus maniculatus",]
PEMA.clean <- PEMA[PEMA$mass.status != "outlier",]

length(PEMA.clean[PEMA.clean$lifeStage == "Juvenile",]) #91
min(PEMA.clean$mass[PEMA.clean$lifeStage == "Juvenile"], na.rm = TRUE) #4.02
max(PEMA.clean$mass[PEMA.clean$lifeStage == "Juvenile"], na.rm = TRUE) #31
#unclean min & max = 0, 141

min(PEMA.clean$mass[PEMA.clean$lifeStage != "Juvenile"], na.rm = TRUE) #3.9
max(PEMA.clean$mass[PEMA.clean$lifeStage != "Juvenile"], na.rm = TRUE) #32.6
#unclean min & max = 0, 2356119

#2sigma for juveniles
Juv.2sigma <- PEMA.clean %>%
  dplyr::summarise(sample.size = n(),
                   avg.mass = mean(mass[lifeStage == "Juvenile"], na.rm = TRUE),
                   sigma.mass = sd(mass[lifeStage == "Juvenile"], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (2*sigma.mass),
                   lower.limit.mass = avg.mass - (2*sigma.mass)) %>%
  as.data.frame()

PEMA.clean$mass.status.juv.limit <- rep("NA", nrow(PEMA.clean))

for(i in 1:nrow(PEMA.clean)){
  if(isTRUE(PEMA.clean$mass[i] <= Juv.2sigma$upper.limit.mass)){
    PEMA.clean$mass.status.juv.limit[i] <- "JUV"
  }
  else if(isTRUE(PEMA.clean$mass[i] >= Juv.2sigma$upper.limit.mass)){
    PEMA.clean$mass.status.juv.limit[i] <- "NOT JUV"
  }
}

min(PEMA.clean$mass[PEMA.clean$lifeStage != "Juvenile" & PEMA.clean$mass.status.juv.limit == "NOT JUV"], na.rm = TRUE) #18.77
#old min = 3.9

## generalize to entire dataset
#2sigma for juveniles
Juv.2sigma <- data.labeled %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(sample.size = length(lifeStage == "Juvenile"),
                   avg.mass = mean(mass[lifeStage == "Juvenile" & mass.status != "outlier"], na.rm = TRUE),
                   sigma.mass = sd(mass[lifeStage == "Juvenile" & mass.status != "outlier"], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (2*sigma.mass)) %>%
  as.data.frame()


data.labeled$mass.status.juv.limit <- rep("NA", nrow(data.labeled))

data.juv.limit <- merge(data.labeled, Juv.2sigma, by = "scientificName", all.x = TRUE, all.y = FALSE)

for(i in 1:nrow(data.juv.limit)){
  if(isTRUE(data.juv.limit$mass[i] <= data.juv.limit$juv.mass.upper.limit[i])){
    data.juv.limit$mass.status.juv.limit[i] <- "JUV"
  }
  else if(isTRUE(data.juv.limit$mass[i] >= data.juv.limit$juv.mass.upper.limit[i])){
    data.juv.limit$mass.status.juv.limit[i] <- "NOT JUV"
  }
}

df <- subset(data.juv.limit, lifeStage != "Juvenile" & scientificName  == "Peromyscus maniculatus")
p <- ggplot(data = df) + 
  geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  ggtitle("Peromyscus maniculatus")
  scale_x_log10(name = expression(log[10]~Body~Mass~(g)))
ggsave(p, file=paste0("juv.outlier.test",".png"), width = 14, height = 10, units = "cm")

write.csv(data.juv.limit, "clean.data.csv")

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


##Lagomorphs - work well
##Carnivores
##Chiroptera
##split up data in other ways? Family (phylo); function but not related; shape; guilds
