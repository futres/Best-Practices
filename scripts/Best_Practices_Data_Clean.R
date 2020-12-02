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

## FuTRES data

futres <- read.csv("https://de.cyverse.org/dl/d/B296C95F-56A1-4AB6-9EAA-532CD7B1285B/futres.csv", header = TRUE, stringsAsFactors = FALSE)
#about futres data:
#has various terms for adult and juvenile
#currently aligned manually; fixed Amelia's weight data to be g, deleted lone mass that did not have units
#6 species

## Vertnet data

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

## Combine VertNet Bat & Mammal data----

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

##clean FuTRES data----

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

##Combine VertNet and FuTRES Data----

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

##clean scientificCNames
data$scientificName <- word(data$scientificName, 1,2, sep = " ") 
data.binom<- data[!grepl('sp.', data$scientificName),]
length(unique(data.binom$scientificName)) #4348
# in mamm: 2766 spp; and now 1738 because got rid of trinomials
data.binom <- data.binom[!is.na(data.binom$scientificName),]
data.binom <- data.binom[data.binom$scientificName != "(new SW",]
length(unique(data.binom$scientificName)) #4346

##write data file with futres and vertnet combined----
write.csv(data.binom, "dirty.data.csv")

##remove Juv. and less than 10 samples---
data.adult <- data.binom[data.binom$lifeStage != "Juvenile",]
length(unique(data.adult$scientificName))

data.adult.stats <- data.adult %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size = n())

keep.10 <- data.adult.stats$scientificName[data.adult.stats$sample.size >= 10]
data.adult <- data.adult[data.adult$scientificName %in% keep.10,]

##write data file without juveniles and species with less than 10 records
write.csv(data.adult, "data.adult.noJuv.csv")

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

data.mass <- correct.units(data = data.adult, column.unit = "mass.units", column.infer = "mass.units.inferred", values = grams, unit = "g")
data.total.length <- correct.units(data = data.mass, column.unit = "total.length.units", column.infer = "total.length.units.inferred", values = millimeters, unit = "mm")
data.tail.length <- correct.units(data = data.total.length, column.unit = "tail.length.units", column.infer = "tail.length.units.inferred", values = millimeters, unit = "mm")
data.ear.length <- correct.units(data = data.tail.length, column.unit = "ear.length.units", column.infer = "ear.length.units.inferred", values = millimeters, unit = "mm")
data.hindfoot.length <- correct.units(data = data.ear.length, column.unit = "hindfoot.length.units", column.infer = "hindfoot.length.units.inferred", values = millimeters, unit = "mm")
#data.forearm.length <- correct.units(data = data.hindfoot.length, column.unit = "forearm.length.units", column.infer = "forearm.length.units.inferred", values = millimeters, unit = "mm")

#make measurementValue numeric
data.meas <- data.hindfoot.length
cols <- c("mass", "total.length", "hindfoot.length", "ear.length", "tail.length", "calcaneus.GB", "calcaneus.GL", "astragalus.length", "astragalus.width")
data.meas[,cols] <- sapply(data.meas[,cols], as.numeric)
length(unique(data.meas$scientificName)) #1747

##write datafile with standardization of units----
write.csv(data.meas, "less.dirty.data.csv")
#data.meas <- read.csv("less.dirty.data.csv", header = TRUE)

## TO DO:----
#make all traits with values of "0" <- NA
#only like 80 records for mass, so likely not too big of an issue

##Figure 1 panel 1: lifeStage----
length(data.meas$mass[data.meas$scientificName == "Artibeus jamaicensis" & !is.na(data.meas$mass)]) #1394
length(data.meas$mass[data.meas$scientificName == "Peromyscus maniculatus" & !is.na(data.meas$mass)]) #30717
length(data.meas$mass[data.meas$scientificName == "Spermophilus beecheyi" & !is.na(data.meas$mass)]) #222
length(data.meas$mass[data.meas$scientificName == "Odocoileus virginianus" & !is.na(data.meas$mass)]) #930
data.meas2 <- data.meas

ccQuality <- c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue", "lightsteelblue1")
data.meas2$lifeStage[data.meas2$lifeStage == "--" | data.meas2$lifeStage == ""] <- "NS"

data.meas2$mass.units.inferred[data.meas2$mass.units.inferred == "False" | data.meas2$mass.units.inferred == "FALSE"] <- "F"
data.meas2$mass.units.inferred[data.meas2$mass.units.inferred == "True" | data.meas2$mass.units.inferred == "TRUE"] <- "T"
data.meas2$mass.units.inferred[data.meas2$mass.units.inferred == ""] <- NA

data.meas2$cat <- paste(data.meas2$lifeStage, data.meas2$mass.units.inferred)
data.meas2$cat[data.meas2$cat == "NS F"] <- "No stage; units known" #lightsteelblue4
data.meas2$cat[data.meas2$cat == "Adult F"] <- "Adult; units known" #darkslateblue
data.meas2$cat[data.meas2$cat == "Adult NA"] <- "Adult; units unknown" #lightslateblue
data.meas2$cat[data.meas2$cat == "NS NA"] <- "No stage; units unknown" #lightsteelblue1
data.meas2$cat[data.meas2$cat == "NS T"] <- "No stage; units inferred" #lightsteelblue
data.meas2$cat[data.meas2$cat == "Adult T"] <- "Adult; units inferred" #mediumslateblue

data.meas2$cat <- as.factor(data.meas2$cat)
data.meas2$cat = relevel(data.meas2$cat, "Adult; units known")
data.meas2$cat <- factor(data.meas2$cat, levels = c("Adult; units known", "Adult; units inferred", "Adult; units unknown",
                                                      "No stage; units known", "No stage; units inferred", "No stage; units unknown"))
df <- subset(data.meas2, data.meas2$scientificName == "Artibeus jamaicensis" & !is.na(data.meas2$mass))
length(df$mass) 
unique(df$cat)
length(df$cat[df$cat == "No stage; units known"]) #996
length(df$cat[df$cat == "No stage; units inferred"]) #151
length(df$cat[df$cat == "Adult; units known"]) #239
length(df$cat[df$cat == "Adult; units inferred"]) #8
p <- ggplot(data = df) + 
  geom_density(aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Artibeus jamaicensis N = 1394") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 250)) +
  scale_y_continuous(name = "Density", limits = c(0, .15)) #.7
ggsave(p, file=paste0("orig.dist.lifeStage.bat2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.meas2, data.meas2$scientificName == "Peromyscus maniculatus" & !is.na(data.meas2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "No stage; units known"]) #21700
length(df$cat[df$cat == "No stage; units inferred"]) #5992
length(df$cat[df$cat == "Adult; units known"]) #2955
length(df$cat[df$cat == "Adult; units inferred"]) #70
p <- ggplot(data = df) + 
  geom_density(aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightsteelblue4", "lightsteelblue"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Peromyscus maniculatus N = 30717") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .2))
ggsave(p, file=paste0("orig.dist.lifeStage.mouse2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.meas2, data.meas2$scientificName == "Spermophilus beecheyi" & !is.na(data.meas2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "No stage; units known"]) #52
length(df$cat[df$cat == "No stage; units inferred"]) #56
length(df$cat[df$cat == "Adult; units known"]) #113
length(df$cat[df$cat == "Adult; units inferred"]) #1
p <- ggplot(data = df) + 
  geom_density(aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightsteelblue4", "lightsteelblue"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Spermophilus beecheyi N = 222") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .02)) #.2?
ggsave(p, file=paste0("orig.dist.lifeStage.squirrel2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.meas2, data.meas2$scientificName == "Odocoileus virginianus" & !is.na(data.meas2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "No stage; units known"]) #904
length(df$cat[df$cat == "No stage; units inferred"]) #22
length(df$cat[df$cat == "Adult; units known"]) #4
p <- ggplot(data = df) + 
  geom_density(aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "lightsteelblue4", "lightsteelblue"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Odocoileus virginianus N = 930") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 80000)) +
  scale_y_continuous(name = "Density", limits = c(0, .001))
ggsave(p, file=paste0("orig.dist.lifeStage.deer2",".png"), width = 14, height = 10, units = "cm")

##Mahalanobis Outlier test----
#add rownames for indexing
rownames(data.meas) <- seq(1, nrow(data.meas),1)

#create status columns
data.meas$mass.status <- rep("", nrow(data.meas))
data.meas$total.length.status <- rep("", nrow(data.meas))
data.meas$tail.length.status <- rep("", nrow(data.meas))
data.meas$hindfoot.length.status <- rep("", nrow(data.meas))
data.meas$ear.length.status <- rep("", nrow(data.meas))
#data.meas$forearm.length.status <- rep("", nrow(data.meas))

#need to select out trait
#remove NAs from trait
#apply maha() function
#select out indices
#label those rows as outliers

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

sp <- unique(data.meas$scientificName)
values <- c("False", "FALSE")
df.mass <- outlier.function(data = data.meas, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "mass", units = "mass.units", unit = "g", unit.infer = "mass.units.inferred", values = values, status = "mass.status")
df.total.length <- outlier.function(data = df.mass, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "total.length", units = "total.length.units", unit = "mm", unit.infer = "total.length.units.inferred", values = values, status = "total.length.status")
df.hindfoot.length <- outlier.function(data = df.total.length, threshold = 0.95, column = "scientificName", vector = sp, 
                            trait = "hindfoot.length", units = "hindfoot.length.units", unit = "mm", unit.infer = "hindfoot.length.units.inferred", values = values, status = "hindfoot.length.status")
df.ear.length <- outlier.function(data = df.hindfoot.length, threshold = 0.95, column = "scientificName", vector = sp,
                                  trait = "ear.length", units = "ear.length.units", unit = "mm", unit.infer = "ear.length.units.inferred", values = values, status = "ear.length.status")
#df.ear.length[is.nan(df.ear.length$tail.length)] <- NA
#df.tail.length <- outlier.function(data = df.ear.length, threshold = 0.95, column = "scientificName", vector = sp,
  #                                 trait = "tail.length", units = "tail.length.units", unit = "mm", unit.infer = "tail.length.units.inferred", values = values, status = "tail.length.status")

#NOTE: tail length not working

##write out Mahalanobis outlier test data----
data.mh <- df.ear.length
length(unique(data.mh$scientificName)) #1747
write.csv(data.mh, "mh.outlier.checked.data.csv")
#data.mh <- read.csv("mh.outlier.checked.csv", header = TRUE)

##Figure 1, panel 2: outliers----
#cQuality <- c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue", "lightsteelblue1")
data.mh2$lifeStage[data.mh2$lifeStage == "--" | data.mh2$lifeStage == ""] <- "NS"

data.mh2$mass.units.inferred[data.mh2$mass.units.inferred == "False" | data.mh2$mass.units.inferred == "FALSE"] <- "F"
data.mh2$mass.units.inferred[data.mh2$mass.units.inferred == "True" | data.mh2$mass.units.inferred == "TRUE"] <- "T"
data.mh2$mass.units.inferred[data.mh2$mass.units.inferred == ""] <- NA

data.mh2$cat <- paste(data.mh2$mass.units.inferred, data.mh2$mass.status)
data.mh2$cat[data.mh2$cat == "F "] <- "possibly good; units known" #darkslateblue
data.mh2$cat[data.mh2$cat == "F outlier"] <- "outlier; units known" #lightsteelblue4
data.mh2$cat[data.mh2$cat == "NA "] <- "untested; units unknown" #lightslateblue
data.mh2$cat[data.mh2$cat == "T "] <- "untested; units inferred" #lightsteelblue1
#lightsteelblue
#mediumslateblue

data.mh2$cat <- as.factor(data.mh2$cat)
data.mh2$cat = relevel(data.mh2$cat, "possibly good; units known")
data.mh2$cat <- factor(data.mh2$cat, levels = c("possibly good; units known", "outlier; units known", "untested; units unknown", "untested; units inferred"))
                                                    
df <- subset(data.mh2, data.mh2$scientificName == "Artibeus jamaicensis" & !is.na(data.mh2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units known"]) #1165
length(df$cat[df$cat == "untested; units inferred"]) #159
length(df$cat[df$cat == "outlier; units known"]) #70
length(df$mass[df$mass.status == "outlier"]) #70
p <- ggplot() + 
  #geom_histogram(data = filter(df, mass.status == "outlier"), aes(x = log10(mass)), color = "darkgray", alpha = 0.3, binwidth = .005, boundary = TRUE) +
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "lightsteelblue4", "lightsteelblue1"),
                    name = "Data Quality Category") +
  #geom_density(data = df, aes(x = mass), color = "darkgray", fill = cat, alpha = 0.9) +
  ggtitle("Artibeus jamaicensis N = 1394, Noutliers = 70") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 250)) +
  scale_y_continuous(name = "Density", limits = c(0, .15)) #.7
ggsave(p, file=paste0("outlier.test.bat2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.mh2, data.mh2$scientificName == "Peromyscus maniculatus" & !is.na(data.mh2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units known"]) #24654
length(df$cat[df$cat == "untested; units inferred"]) #6062
length(df$cat[df$cat == "outlier; units known"]) #1
length(df$mass[df$mass.status == "outlier"])
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "lightsteelblue4", "lightsteelblue1"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30717, Noutlier = 1") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .2))
ggsave(p, file=paste0("outlier.test.mouse2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.mh2, data.mh2$scientificName == "Spermophilus beecheyi" & !is.na(data.mh2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units known"]) #155
length(df$cat[df$cat == "untested; units inferred"]) #57
length(df$cat[df$cat == "outlier; units known"]) #10
length(df$mass[df$mass.status == "outlier"])
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "lightsteelblue4", "lightsteelblue1"),
                    name = "Data Quality Category") +
  ggtitle("Spermophilus beecheyi N = 222, Noutlier = 10") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .02))
ggsave(p, file=paste0("outlier.test.squirrel2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.mh2, data.mh2$scientificName == "Odocoileus virginianus" & !is.na(data.mh2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units known"]) #902
length(df$cat[df$cat == "untested; units inferred"]) #22
length(df$cat[df$cat == "outlier; units known"]) #6
length(df$mass[df$mass.status == "outlier"])
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, adjust = 1/10, alpha = 0.4) +
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9, adjust = 1/10) +
  #geom_density(data = df, aes(x = log10(mass), fill = mass.status), alpha = 0.9) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4, adjust = 1/10) +
  scale_fill_manual(values = c("darkslateblue", "lightsteelblue4", "lightsteelblue1"),
                    name = "Data Quality Category") +
  ggtitle("Odocoileus virginianus N = 930, Noutlier = 6") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 80000)) +
  scale_y_continuous(name = "Density", limits = c(0, .001))#.0001
ggsave(p, file=paste0("outlier.test.deer2",".png"), width = 14, height = 10, units = "cm")

##TO DO: make into function
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

## Function for upper and lower limits (round 1) ----
##create function to find upper and lower limit for each measurementType based on non-juveniles, non-inferred units, and correct units ("g" or "mm")

##mean, standard deviation, and upper and lower limit

values <- c("FALSE", "False")
data.noInfer_stats <- data.mh %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(sample.size.mass = length(mass[mass.units.inferred %in% values & mass.status != "outlier" & mass.units == "g" & !is.na(mass)]),
                   avg.mass = mean(mass[mass.units.inferred %in% values & mass.status != "outlier" & mass.units == "g"], na.rm = TRUE),
                   sigma.mass = sd(mass[mass.units.inferred %in% values & mass.status != "outlier" & mass.units == "g"], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (3*sigma.mass),
                   lower.limit.mass = avg.mass - (3*sigma.mass),
                   
                   sample.size.length = length(total.length[total.length.units.inferred %in% values & total.length.units == "mm" & !is.na(total.length)]),
                   avg.length = mean(total.length[total.length.units.inferred %in% values & total.length.units == "mm"], na.rm = TRUE),
                   sigma.length = sd(total.length[total.length.units.inferred %in% values & total.length.units == "mm"], na.rm = TRUE),
                   upper.limit.length = avg.length + (3*sigma.length),
                   lower.limit.length = avg.length - (3*sigma.length),
                   
                   sample.size.hindfoot = length(hindfoot.length[hindfoot.length.units.inferred %in% values & hindfoot.length.units == "mm" & !is.na(hindfoot.length)]),
                   avg.hindfoot = mean(hindfoot.length[hindfoot.length.units.inferred %in% values & hindfoot.length.units == "mm"], na.rm = TRUE),
                   sigma.hindfoot = sd(hindfoot.length[hindfoot.length.units.inferred %in% values & hindfoot.length.units == "mm"], na.rm = TRUE),
                   upper.limit.hindfoot = avg.hindfoot + (3*sigma.hindfoot),
                   lower.limit.hindfoot = avg.hindfoot - (3*sigma.hindfoot),
                   
                   sample.size.ear = length(ear.length[ear.length.units.inferred %in% values & ear.length.units == "mm" & !is.na(ear.length)]),
                   avg.ear.length = mean(ear.length[ear.length.units.inferred %in% values & ear.length.units == "mm"], na.rm = TRUE),
                   sigma.ear = sd(ear.length[ear.length.units.inferred %in% values & ear.length.units == "mm"], na.rm = TRUE),
                   upper.limit.ear = avg.ear.length + (3*sigma.ear),
                   lower.limit.ear = avg.ear.length - (3*sigma.ear),
                   
                   #sample.size.forearm = length(forearm.length[forearm.length.units.inferred %in% values & forearm.length.units == "mm" & !is.na(forearm.length)])
                   #avg.forearm.length = mean(forearm.length[forearm.length.units.inferred %in% values & forearm.length.units == "mm"], na.rm = TRUE),
                   #sigma.forearm = sd(forearm.length[forearm.length.units.inferred %in% values & forearm.length.units == "mm"], na.rm = TRUE),
                   #upper.limit.forearm = avg.forearm.length + (3*sigma.forearm),
                   #lower.limit.forearm = avg.forearm.length - (3*sigma.forearm),
                   
                   sample.size.tail = length(tail.length[tail.length.units.inferred %in% values & tail.length.units == "mm" & !is.na(tail.length)]),
                   avg.tail.length = mean(tail.length[tail.length.units.inferred %in% values & tail.length.units == "mm"], na.rm = TRUE),
                   sigma.tail = sd(tail.length[tail.length.units.inferred %in% values & tail.length.units == "mm"], na.rm = TRUE),
                   upper.limit.tail = avg.tail.length + (3*sigma.tail),
                   lower.limit.tail = avg.tail.length - (3*sigma.tail)) %>%
  as.data.frame()
nrow(data.noInfer_stats) #1747

##add stats to dataframe
data.limit <- merge(data.mh, data.noInfer_stats, by = "scientificName", all.x = TRUE, all.y = FALSE)
length(unique(data.limit$scientificName)) #1747

##label outliers
#label samples that are outside of limits with outlier, and label those within limits as "g" and inferred = TRUE

check.1 <- function(data, trait, status, upper, lower, sample.size){
  for(i in 1:nrow(data)){
    if(isTRUE(data[i,sample.size] < 10)){
      data[i,status] <- "too few records"
    }
    else if(isTRUE(data[i,trait] <= data[i,lower])){
      data[i,status] <- "outlier"
    }
    else if(isTRUE(data[i,trait] >= data[i,upper])){
      data[i,status] <- "outlier"
    }
    else{
      data[i,status] <- "GOOD"
    }
  }
  return(data)
}

check.2 <- function(data, units, units.infer, status, unit){
  for(i in 1:nrow(data)){
    if(isTRUE(data[i,status][units != unit] == "GOOD")){
      data[i,units.infer] <- "TRUE"
      data[i,units] <- unit
    }
    else{
      next
    }
  }
  return(data)
}

#test
#which(data.limit$sample.size.mass < 10)
#data.limit$scientificName[6977]
#df <- subset(data.limit, data.limit$scientificName == "Ametrida centurio")
#df2 <- subset(df, select = c("mass", "mass.status", "upper.limit.mass", "lower.limit.mass", "sample.size.mass", "mass.units", "mass.units.inferred"))
#df3 <- check.1(data = df2, trait = "mass", status = "mass.status", upper = "upper.limit.mass", lower = "lower.limit.mass", sample.size = "sample.size.mass")
#df3$mass.status

data.mass.check1 <- check.1(data = data.limit, trait = "mass", status = "mass.status", upper = "upper.limit.mass", lower = "lower.limit.mass", sample.size = "sample.size.mass")
data.mass.check2 <- check.2(data = data.mass.check1, status = "mass.status", units = "mass.units", units.infer = "mass.units.inferred", unit = "g")

data.length.check1 <- check.1(data = data.mass.check2, trait = "total.length", status = "total.length.status", upper = "upper.limit.length", lower = "lower.limit.length", sample.size = "sample.size.length")
data.length.check2 <- check.2(data = data.length.check1, status = "total.length.status", units = "total.length.units", units.infer = "total.length.units.inferred", unit = "mm")

data.ear.check1 <- check.1(data = data.length.check2, trait = "ear.length", status = "ear.length.status", upper = "upper.limit.ear", lower = "lower.limit.ear", sample.size = "sample.size.ear")
data.ear.check2 <- check.2(data = data.ear.check1, status = "ear.length.status", units = "ear.length.units", units.infer = "ear.length.units.inferred", unit = "mm")

data.hindfoot.check1 <- check.1(data = data.ear.check2, trait = "hindfoot.length", status = "hindfoot.length.status", upper = "upper.limit.hindfoot", lower = "lower.limit.hindfoot", sample.size = "sample.size.hindfoot")
data.hindfoot.check2 <- check.2(data = data.hindfoot.check1, status = "hindfoot.length.status", units = "hindfoot.length.units", units.infer = "hindfoot.length.units.inferred", unit = "mm")

data.tail.check1 <- check.1(data = data.hindfoot.check2, trait = "tail.length", status = "tail.length.status", upper = "upper.limit.tail", lower = "lower.limit.tail", sample.size = "sample.size.hindfoot")
data.tail.check2 <- check.2(data = data.tail.check1, status = "tail.length.status", units = "tail.length.units", units.infer = "tail.length.units.inferred", unit = "mm")

##write out first round of upper and lower limits checking----
data.check <- data.tail.check2
write.csv(data.check, "data.check1.csv")
#data.check <- read.csv("data.check1.csv", header = TRUE)

##Figure 1, Panel 3----
data.check2 <- data.check
#ccQuality <- c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue", "lightsteelblue1")
data.check2$lifeStage[data.check2$lifeStage == "--" | data.check2$lifeStage == ""] <- "NS"

data.check2$mass.units.inferred[data.check2$mass.units.inferred == "False" | data.check2$mass.units.inferred == "FALSE"] <- "F"
data.check2$mass.units.inferred[data.check2$mass.units.inferred == "True" | data.check2$mass.units.inferred == "TRUE"] <- "T"
data.check2$mass.units.inferred[data.check2$mass.units.inferred == ""] <- NA

data.check2$cat <- paste(data.check2$mass.units.inferred, data.check2$mass.status) #weird, no units aren't inferred and GOOD
data.check2$cat[data.check2$cat == "T GOOD"] <- "possibly good; units inferred" #mediumslateblue
data.check2$cat[data.check2$cat == "F outlier"] <- "outlier; units known" #lightsteelblue4
data.check2$cat[data.check2$cat == "T too few records"] <- "too few records; units inferred" #gray
data.check2$cat[data.check2$cat == "NA too few records"] <- "too few records; units unknown" #gray
data.check2$cat[data.check2$cat == "F too few records"] <- "too few records; units known" #gray
data.check2$cat[data.check2$cat == "T outlier"] <- "outlier; units inferred" #lightsteelblue
#darkslateblue
#lightslateblue
#lightsteelblue4

data.check2$cat <- as.factor(data.check2$cat)
data.check2$cat = relevel(data.check2$cat, "possibly good; units inferred")
data.check2$cat <- factor(data.check2$cat, levels = c("possibly good; units inferred", "outlier; units known", "outlier; units inferred", 
                                                      "too few records; units known", "too few records; units inferred", "too few records; units unknown"))

df <- subset(data.check2, data.check2$scientificName == "Artibeus jamaicensis" & !is.na(data.check2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #1302
length(df$cat[df$cat == "outlier; units known"]) #62
length(df$cat[df$cat == "outlier; units inferred"]) #30
length(df$mass[df$mass.status == "outlier"]) #92
p <- ggplot() + 
  #geom_histogram(data = filter(df, mass.status == "outlier"), aes(x = log10(mass)), color = "darkgray", alpha = 0.3, binwidth = .005, boundary = TRUE) +
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  #geom_density(data = df, aes(x = mass), color = "darkgray", fill = cat, alpha = 0.9) +
  ggtitle("Artibeus jamaicensis N = 1394, Noutliers = 92") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 250)) +
  scale_y_continuous(name = "Density", limits = c(0, .15)) #.7
ggsave(p, file=paste0("check1.test.bat2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.check2, data.check2$scientificName == "Peromyscus maniculatus" & !is.na(data.check2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #30694
length(df$cat[df$cat == "outlier; units known"]) #20
length(df$cat[df$cat == "outlier; units inferred"]) #3
length(df$mass[df$mass.status == "outlier"]) #23
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30717, Noutlier = 23") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .2))
ggsave(p, file=paste0("check1.test.mouse2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.check2, data.check2$scientificName == "Spermophilus beecheyi" & !is.na(data.check2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #213
length(df$cat[df$cat == "outlier; units known"]) #2
length(df$cat[df$cat == "outlier; units inferred"]) #7
length(df$mass[df$mass.status == "outlier"]) #9
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) +
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  ggtitle("Spermophilus beecheyi N = 222, Noutlier = 9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .02))
ggsave(p, file=paste0("check1.test.squirrel2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.check2, data.check2$scientificName == "Odocoileus virginianus" & !is.na(data.check2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #915
length(df$cat[df$cat == "outlier; units known"]) #15
length(df$mass[df$mass.status == "outlier"]) #125
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, adjust = 1/10, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9, adjust = 1/10) +
  #geom_density(data = df, aes(x = log10(mass), fill = mass.status), alpha = 0.9) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4, adjust = 1/10) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue4"),
                    name = "Data Quality Category") +
  ggtitle("Odocoileus virginianus N = 930, Noutlier = 15") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 80000)) +
  scale_y_continuous(name = "Density", limits = c(0, .001))#.0001
ggsave(p, file=paste0("check1.test.deer2",".png"), width = 14, height = 10, units = "cm")

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

data.convert.mass <- convert.g(data = data.check, trait = "mass", units = "mass.units", units.infer = "mass.units.inferred")
data.convert.length <- convert.mm(data = data.convert.mass, trait = "total.length", units = "total.length.units", units.infer = "total.length.units.inferred")
data.convert.hindfoot <- convert.mm(data = data.convert.length, trait = "hindfoot.length", units = "hindfoot.length.units", units.infer = "hindfoot.length.units.inferred")
data.convert.ear <- convert.mm(data = data.convert.hindfoot, trait = "ear.length", units = "ear.length.units", units.infer = "ear.length.units.inferred")
data.convert.tail <- convert.mm(data = data.convert.ear, trait = "tail.length", units = "tail.length.units", units.infer = "tail.length.units.inferred")
#data.convert.forearm <- convert.mm(data = data.convert.tail, trait = "forearm.length", units = "forearm.length.units",units.infer = "forearm.length.units.inferred")

##write out data with converted units----
data.convert <- data.convert.tail
write.csv(data.convert, "data.convert.csv")
#data.convert <- read.csv("data.convert.csv", header = TRUE)

##Figure 1, panel 4----
data.convert2 <- data.convert
#ccQuality <- c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue", "lightsteelblue1")
data.convert2$lifeStage[data.convert2$lifeStage == "--" | data.convert2$lifeStage == ""] <- "NS"

data.convert2$mass.units.inferred[data.convert2$mass.units.inferred == "False" | data.convert2$mass.units.inferred == "FALSE"] <- "F"
data.convert2$mass.units.inferred[data.convert2$mass.units.inferred == "True" | data.convert2$mass.units.inferred == "TRUE"] <- "T"
data.convert2$mass.units.inferred[data.convert2$mass.units.inferred == ""] <- NA

data.convert2$cat <- paste(data.convert2$mass.units.inferred, data.convert2$mass.status) #weird, no units aren't inferred and GOOD
data.convert2$cat[data.convert2$cat == "T GOOD"] <- "possibly good; units inferred" #darkslateblue
data.convert2$cat[data.convert2$cat == "F outlier"] <- "outlier; units known" #lightsteelblue4
data.convert2$cat[data.convert2$cat == "T too few records"] <- "too few records; units inferred" #gray
data.convert2$cat[data.convert2$cat == "NA too few records"] <- "too few records; units unknown" #gray
data.convert2$cat[data.convert2$cat == "F too few records"] <- "too few records; units known" #gray
data.convert2$cat[data.convert2$cat == "T outlier"] <- "outlier; units inferred" #lightsteelblue
data.convert2$cat[data.convert2$cat == "CONVERTED too few records"] <- "too few records; units converted" #gray
data.convert2$cat[data.convert2$cat == "CONVERTED outlier"] <- "outlier; units converted" #lightsteelblue1
#mediumslateblue
#lightslateblue


data.convert2$cat <- as.factor(data.convert2$cat)
data.convert2$cat = relevel(data.convert2$cat, "possibly good; units inferred")
data.convert2$cat <- factor(data.convert2$cat, levels = c("possibly good; units inferred", "outlier; units known", "outlier; units inferred", "outlier; units converted",
                                                      "too few records; units known", "too few records; units inferred", "too few records; units unknown", "too few records; units converted"))

df <- subset(data.convert2, data.convert2$scientificName == "Artibeus jamaicensis" & !is.na(data.convert2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #1302
length(df$cat[df$cat == "outlier; units known"]) #62
length(df$cat[df$cat == "outlier; units inferred"]) #30
length(df$mass[df$mass.status == "outlier"]) #92
p <- ggplot() + 
  #geom_histogram(data = filter(df, mass.status == "outlier"), aes(x = log10(mass)), color = "darkgray", alpha = 0.3, binwidth = .005, boundary = TRUE) +
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  #geom_density(data = df, aes(x = mass), color = "darkgray", fill = cat, alpha = 0.9) +
  ggtitle("Artibeus jamaicensis N = 1394, Noutliers = 92") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 250)) +
  scale_y_continuous(name = "Density", limits = c(0, .15)) #.7
ggsave(p, file=paste0("convert.test.bat2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.convert2, data.convert2$scientificName == "Peromyscus maniculatus" & !is.na(data.convert2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #30694
length(df$cat[df$cat == "outlier; units known"]) #19
length(df$cat[df$cat == "outlier; units inferred"]) #3
length(df$cat[df$cat == "outlier; units converted"]) #1
length(df$mass[df$mass.status == "outlier"]) #23
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue4", "lightsteelblue", "lightsteelblue1"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30717, Noutlier = 23") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .2))
ggsave(p, file=paste0("check1.test.mouse2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.convert2, data.convert2$scientificName == "Spermophilus beecheyi" & !is.na(data.convert2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #213
length(df$cat[df$cat == "outlier; units known"]) #2
length(df$cat[df$cat == "outlier; units inferred"]) #7
length(df$mass[df$mass.status == "outlier"]) #9
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) +
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  ggtitle("Spermophilus beecheyi N = 222, Noutlier = 9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .02))
ggsave(p, file=paste0("check1.test.squirrel2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.convert2, data.convert2$scientificName == "Odocoileus virginianus" & !is.na(data.convert2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #915
length(df$cat[df$cat == "outlier; units known"]) #12
length(df$cat[df$cat == "outlier; units converted"])  #3
length(df$mass[df$mass.status == "outlier"]) #15
p <- ggplot() + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, adjust = 1/10, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9, adjust = 1/10) +
  #geom_density(data = df, aes(x = log10(mass), fill = mass.status), alpha = 0.9) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4, adjust = 1/10) +
  scale_fill_manual(values = c("mediumslateblue", "lightsteelblue", "lightsteelblue1"),
                    name = "Data Quality Category") +
  ggtitle("Odocoileus virginianus N = 930, Noutlier = 15") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 80000)) +
  scale_y_continuous(name = "Density", limits = c(0, .001))
ggsave(p, file=paste0("check1.test.deer2",".png"), width = 14, height = 10, units = "cm")


##recalculate upper and lower limits (round 2)----

##create new sigma, this time allow for inferred units
##next time, don't allow for inferred units...
data.stats <- data.convert %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.size.mass.2 = length(mass[mass.status != "outlier" & lifeStage != "Juvenile" & !is.na(mass)]),
                   avg.mass.2 = mean(mass[mass.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.mass.2 = sd(mass[mass.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.mass.2 = avg.mass.2 + (3*sigma.mass.2),
                   lower.limit.mass.2 = avg.mass.2 - (3*sigma.mass.2),
                   
                   sample.size.length.2 = length(total.length[total.length.status != "outlier" & lifeStage != "Juvenile" & !is.na(total.length)]),
                   avg.length.2 = mean(total.length[total.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.length.2 = sd(total.length[total.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.length.2 = avg.length.2 + (2.5*sigma.length.2),
                   lower.limit.length.2 = avg.length.2 - (2.5*sigma.length.2),
                   
                   sample.size.ear.2 = length(ear.length[ear.length.status != "outlier" & lifeStage != "Juvenile" & !is.na(ear.length)]),
                   avg.ear.length.2 = mean(ear.length[ear.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.ear.2 = sd(ear.length[ear.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.ear.2 = avg.ear.length.2 + (2.5*sigma.ear.2),
                   lower.limit.ear.2 = avg.ear.length.2 - (2.5*sigma.ear.2),
                   
                   sample.size.hindfoot.2 = length(hindfoot.length[hindfoot.length.status != "outlier" & lifeStage != "Juvenile" & !is.na(hindfoot.length)]),
                   avg.hindfoot.2 = mean(hindfoot.length[hindfoot.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.hindfoot.2 = sd(hindfoot.length[hindfoot.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.hindfoot.2 = avg.hindfoot.2 + (2.5*sigma.length.2),
                   lower.limit.hindfoot.2 = avg.hindfoot.2 - (2.5*sigma.length.2),
                   
                   sample.size.tail.2 = length(tail.length[tail.length.status != "outlier" & lifeStage != "Juvenile" & !is.na(tail.length)]),
                   avg.tail.2 = mean(tail.length[tail.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   sigma.tail.2 = sd(tail.length[tail.length.status != "outlier" & lifeStage != "Juvenile"], na.rm = TRUE),
                   upper.limit.tail.2 = avg.length.2 + (2.5*sigma.length.2),
                   lower.limit.tail.2 = avg.length.2 - (2.5*sigma.length.2)) %>%
  as.data.frame()

#add stats to dataframe
data.recheck <- merge(data.convert, data.stats, by = "scientificName", all.x = TRUE, all.y = FALSE)

##RE-label outliers & check to see if old outliers are still outliers----

for(i in 1:nrow(data.recheck)){
  if(isTRUE(data.recheck$sample.size.mass.2[i] < 10)){
    data.recheck$mass.status[i] <- "too few records"
  }
  else if(isTRUE(data.recheck$mass[i] < data.recheck$lower.limit.mass.2[i])){
    data.recheck$mass.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$mass[i] > data.recheck$upper.limit.mass.2[i])){
    data.recheck$mass.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$mass[i] <= data.recheck$upper.limit.mass.2[i])){
    data.recheck$mass.status[i] <- "GOOD"
  }
  else if(isTRUE(data.recheck$mass[i] >= data.recheck$lower.limit.mass.2[i])){
    data.recheck$mass.status[i] <- "GOOD"
  }
  else{
    next
  }
}

for(i in 1:nrow(data.recheck)){
  if(isTRUE(data.recheck$sample.size.length.2[i] < 10)){
    data.recheck$total.length.status[i] <- "too few samples"
  }
  else if(isTRUE(data.recheck$total.length[i] < data.recheck$lower.limit.length.2[i])){
    data.recheck$total.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$total.length[i] > data.recheck$upper.limit.length.2[i])){
    data.recheck$total.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$total.length[i] <= data.recheck$upper.limit.length.2[i])){
    data.recheck$total.length.status[i] <- "GOOD"
  }
  else if(isTRUE(data.recheck$total.length[i] >= data.recheck$lower.limit.length.2[i])){
    data.recheck$total.length.status[i] <- "GOOD"
  }
  else{
    next
  }
}

for(i in 1:nrow(data.recheck)){
  if(isTRUE(data.recheck$sample.size.tail.2[i] < 10)){
    data.recheck$tail.length.status[i] <- "too few records"
  }
  else if(isTRUE(data.recheck$tail.length[i] < data.recheck$lower.limit.tail.2[i])){
    data.recheck$tail.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$tail.length[i] > data.recheck$upper.limit.tail.2[i])){
    data.recheck$tail.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$tail.length[i] <= data.recheck$upper.limit.tail.2[i])){
    data.recheck$tail.length.status[i] <- "GOOD"
  }
  else if(isTRUE(data.recheck$tail.length[i] >= data.recheck$lower.limit.tail.2[i])){
    data.recheck$tail.length.status[i] <- "GOOD"
  }
  else{
    next
  }
}

for(i in 1:nrow(data.recheck)){
  if(isTRUE(data.recheck$sample.size.ear.2[i] < 10)){
    data.recheck$ear.length.status[i] <- "too few records"
  }
  else if(isTRUE(data.recheck$ear.length[i] < data.recheck$lower.limit.ear.2[i])){
    data.recheck$ear.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$ear.length[i] > data.recheck$upper.limit.ear.2[i])){
    data.recheck$ear.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$ear.length[i] <= data.recheck$upper.limit.ear.2[i])){
    data.recheck$ear.length[i] <- "GOOD"
  }
  else if(isTRUE(data.recheck$ear.length[i] >= data.recheck$lower.limit.ear.2[i])){
    data.recheck$ear.length.status[i] <- "GOOD"
  }
  else{
    next
  }
}

for(i in 1:nrow(data.recheck)){
  if(isTRUE(data.recheck$sample.size.hindfoot.2[i] < 10)){
    data.recheck$hindfoot.length.status[i] <- "too few records"
  }
  else if(isTRUE(data.recheck$hindfoot.length[i] < data.recheck$lower.limit.hindfoot.2[i])){
    data.recheck$hindfoot.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$hindfoot.length[i] > data.recheck$upper.limit.hindfoot.2[i])){
    data.recheck$hindfoot.length.status[i] <- "outlier"
  }
  else if(isTRUE(data.recheck$hindfoot.length[i] <= data.recheck$upper.limit.hindfoot.2[i])){
    data.recheck$hindfoot.length.status[i] <- "GOOD"
  }
  else if(isTRUE(data.recheck$hindfoot.length[i] >= data.recheck$lower.limit.hindfoot.2[i])){
    data.recheck$hindfoot.length.status[i] <- "GOOD"
  }
  else{
    next
  }
}

# for(i in 1:length(data.outlier$scientificName)){
#   if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$forearm.length[i] < data.outlier$forearm.length.lower.limit[i])){
#     data.outlier$forearm.length.status[i] <- "outlier"
#   }
#   else if(isTRUE(data.outlier$lifeStage[i] != "Juvenile" & data.outlier$forearm.length[i] > data.outlier$forearm.length.upper.limit[i])){
#     data.outlier$forearm.length.status[i] <- "outlier"
#   }
#   else if(isTRUE(data.recheck$forearm.length[i] <= data.recheck$upper.limit.forearm.2[i])){
#     data.recheck$forearm.length.status[i] <- "GOOD"
#   }
#   else if(isTRUE(data.recheck$forearm.length[i] >= data.recheck$lower.limit.forearm.2[i])){
#     data.recheck$forearm.length.status[i] <- "GOOD"
#   }
#   else{
#     next
#   }
# }

##write out clean, labeled data----
data.outlier <- data.recheck
write.csv(data.outlier, "labeled.clean.data.csv")
#data.outlier <- read.csv("labeled.clean.data.csv", header = TRUE)

##Figure 1, panel 5: final results----
data.outlier2 <- data.outlier
data.outlier2$lifeStage[data.outlier2$lifeStage == "--" | data.outlier2$lifeStage == ""] <- "NS"

data.outlier2$mass.units.inferred[data.outlier2$mass.units.inferred == "False" | data.outlier2$mass.units.inferred == "FALSE"] <- "F"
data.outlier2$mass.units.inferred[data.outlier2$mass.units.inferred == "True" | data.outlier2$mass.units.inferred == "TRUE"] <- "T"
data.outlier2$mass.units.inferred[data.outlier2$mass.units.inferred == ""] <- NA

data.outlier2$cat <- paste(data.outlier2$mass.units.inferred, data.outlier2$mass.status) #weird, no units aren't inferred and GOOD
data.outlier2$cat[data.outlier2$cat == "T GOOD"] <- "possibly good; units inferred"#mediumslateblue
data.outlier2$cat[data.outlier2$cat == "F outlier"] <- "outlier; units known" #lightsteelblue4
data.outlier2$cat[data.outlier2$cat == "T too few records"] <- "too few records; units inferred" #gray
data.outlier2$cat[data.outlier2$cat == "NA too few records"] <- "too few records; units unknown" #gray
data.outlier2$cat[data.outlier2$cat == "F too few records"] <- "too few records; units known" #gray
data.outlier2$cat[data.outlier2$cat == "T outlier"] <- "outlier; units inferred" #lightsteelblue
data.outlier2$cat[data.outlier2$cat == "CONVERTED too few records"] <- "too few records; units converted" #gray
data.outlier2$cat[data.outlier2$cat == "CONVERTED outlier"] <- "outlier; units converted" #lightsteelblue1
data.outlier2$cat[data.outlier2$cat == "CONVERTED GOOD"] <- "possibly good; units converted" #lightslateblue
data.outlier2$cat[data.outlier2$cat == "F GOOD"] <- "possibly good; units known" #darkslateblue

data.outlier2$cat <- as.factor(data.outlier2$cat)
data.outlier2$cat = relevel(data.outlier2$cat, "possibly good; units inferred")
data.outlier2$cat <- factor(data.outlier2$cat, levels = c("possibly good; units inferred", "possibly good; units known", "possibly good; units converted",
                                                          "outlier; units known", "outlier; units inferred", "outlier; units converted",
                                                          "too few records; units known", "too few records; units inferred", "too few records; units unknown", "too few records; units converted"))


df <- subset(data.outlier2, data.outlier2$scientificName == "Artibeus jamaicensis" & !is.na(data.outlier2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #1306
length(df$cat[df$cat == "possibly good; units known"]) #9
length(df$cat[df$cat == "outlier; units known"]) #53
length(df$cat[df$cat == "outlier; units inferred"]) #26
length(df$mass[df$mass.status == "outlier"]) #79

p <- ggplot(data = df) + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  geom_density(data = filter(df, mass.status == "GOOD"), aes(x = mass), fill = "darkslateblue", alpha = 0.4) +
  #geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  #geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  #scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightsteelblue4", "lightsteelblue"),
  #                  name = "Data Quality Category") +
  ggtitle("Artibeus jamaicensis N = 1394, Noutlier = 79") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 100)) +
  scale_y_continuous(name = "Density", limits = c(0, .15))
ggsave(p, file=paste0("check2.bat2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.outlier2, data.outlier2$scientificName == "Peromyscus maniculatus" & !is.na(data.outlier2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #30469
length(df$cat[df$cat == "outlier; units known"]) #19
length(df$cat[df$cat == "outlier; units inferred"]) #218
length(df$cat[df$cat == "possibly good; units converted"]) #1
length(df$mass[df$mass.status == "outlier"]) #237

p <- ggplot(data = df) + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status == "GOOD"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  #geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Peromyscus maniculatus N = 30717, Noutlier = 237") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 250)) +
  scale_y_continuous(name = "Density", limits = c(0, .2))
ggsave(p, file=paste0("check2.mouse2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.outlier2, data.outlier2$scientificName == "Spermophilus beecheyi" & !is.na(data.outlier2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #213
length(df$cat[df$cat == "outlier; units known"]) #1
length(df$cat[df$cat == "outlier; units inferred"]) #7
length(df$cat[df$cat == "possibly good; units known"]) #1
length(df$mass[df$mass.status == "outlier"]) #8

p <- ggplot(data = df) + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status == "GOOD"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  #geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightsteelblue4", "lightsteelblue"),
                    name = "Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Spermophilus beecheyi N = 222, Noutlier = 8") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .02))
ggsave(p, file=paste0("check2.test.squirrel2",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.outlier2, data.outlier2$scientificName == "Odocoileus virginianus" & !is.na(data.outlier2$mass))
length(df$mass)
unique(df$cat)
length(df$cat[df$cat == "possibly good; units inferred"]) #915
length(df$cat[df$cat == "outlier; units known"]) #5
length(df$cat[df$cat == "outlier; units converted"]) #2
length(df$cat[df$cat == "possibly good; units known"]) #7
length(df$cat[df$cat == "possibly good; units converted"]) #1
length(df$mass[df$mass.status == "outlier"]) #7

p <- ggplot(data = df) + 
  geom_density(data = filter(df, mass.status == "outlier"), aes(x = mass), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, mass.status == "outlier"), aes(x = mass), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status == "GOOD"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  #geom_density(aes(x = log10(mass), fill = mass.status), alpha = 0.7) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = mass, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue1"),
                    name = "Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Odocoileus virginianus N = 930, Noutlier = 7") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 80000)) +
  scale_y_continuous(name = "Density", limits = c(0, .001))
ggsave(p, file=paste0("check2.deer2",".png"), width = 14, height = 10, units = "cm")

##info about outliers----
outlier_stats <- data.outlier %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.outlier.mass = length(mass[mass.status == "outlier" & mass >= 0]),
                   sample.mass = length(mass[mass.status != "outlier" & mass >= 0]),
                   sample.outlier.total.length = length(total.length[total.length.status == "outlier" & total.length >= 0]),
                   sample.total.length = length(total.length[total.length.status != "outlier" & total.length >= 0]),
                   #sample.outlier.forearm.length = length(forearm.length[forearm.length.status == "outlier" & forearm.length >= 0 & lifeStage != "Juvenile"]),
                   #sample.forearm.length = length(forearm.length[forearm.length.status != "outlier" & forearm.length >= 0 & lifeStage != "Juvenile"]),
                   sample.outlier.hindfoot.length = length(hindfoot.length[hindfoot.length.status == "outlier" & hindfoot.length >= 0]),
                   sample.hindfoot.length = length(hindfoot.length[hindfoot.length.status != "outlier" & hindfoot.length >= 0]),
                   sample.outlier.ear.length = length(ear.length[ear.length.status == "outlier" & ear.length >= 0]),
                   sample.ear.length = length(ear.length[ear.length.status != "outlier" & ear.length >= 0]),
                   sample.outlier.tail.length = length(tail.length[tail.length.status == "outlier" & tail.length >= 0]),
                   sample.tail.length = length(tail.length[tail.length.status != "outlier" & tail.length >= 0])) %>%
  as.data.frame()

##write out csv of outlier stats
write.csv(outlier_stats, "outliers.csv")
