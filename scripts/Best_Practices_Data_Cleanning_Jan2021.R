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

futres <- read.csv("https://de.cyverse.org/dl/d/53BFE69F-EDD2-47FF-B508-723050451FC3/futres.csv", header = TRUE, stringsAsFactors = FALSE)
#about futres data:
#has various terms for adult and juvenile
#currently aligned manually; fixed Amelia's weight data to be g, deleted lone mass that did not have units
#need to fix OR units (currently in lb and in)
#note: astragalus width is actually astragalus breadth
#6 species
nrow(futres[futres$origin == "EAP",]) #34
nrow(futres[futres$origin == "Amelia",]) #94
nrow(futres[futres$origin == "Blois 2008",]) #288
nrow(futres[futres$origin == "OR",]) #7841

## Vertnet data
#extracted from R. LaFrance using Traiter to extract traits and trait values in fields like "dynamicProperties"
#inferred means that units were converted to g or mm
#estimated means that trait values were in non-standard ways (e.g., [5])
#lots of lifeStage info missing
#for traits we're interested in (e.g., not sided traits like testes and ovaries), use first measurement

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
#mammals and bats have different ear length measurements; bats additionally have forearm length

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

write.csv(vertnet, "vertnetcombinded.csv")

#vertnet data has a column header "units" which were the original units. All units have been changed to either "g" or "mm"
#change column names to reflect this

vertnet.sub <- vertnet %>%
  dplyr::select(scientificName = scientificname, 
                materialSampleID = occurrenceid,
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
                mass.verbatim.units = body_mass.1.units,
                mass.units.inferred = body_mass.1.units_inferred,
                mass.estimated.value = body_mass.1.estimated_value,
                
                total.length = total_length.1.value, 
                total.length.verbatim.units = total_length.1.units,
                total.length.units.inferred = total_length.1.units_inferred,
                total.length.estimated.value = total_length.1.estimated_value,
                
                hindfoot.length = hind_foot_length.1.value,
                hindfoot.length.verbatim.units = hind_foot_length.1.units,
                hindfoot.length.units.inferred = hind_foot_length.1.units_inferred,
                hindfoot.length.estimated.value = hind_foot_length.1.estimated_value,
                
                ear.length = ear_length.1.value,
                ear.length.verbatim.units = ear_length.1.units,
                ear.length.units.inferred = ear_length.1.units_inferred,
                ear.length.estimated.value = ear_length.1.estimated_value,
                
                tail.length = tail_length.1.value,
                tail.length.verbatim.units = tail_length.1.units,
                tail.length.units.inferred = tail_length.1.units_inferred,
                tail.length.estimated.value = tail_length.1.estimated_value,
                
                forearm.length = forearm_length.1.value,
                forearm.length.verbatim.units = forearm_length.1.units,
                forearm.length.units.inferred = forearm_length.1.units_inferred,
                forearm.length.estimated.value = forearm_length.1.estimated_value) %>%
  mutate_at(c("elevation", "mass", "total.length", "hindfoot.length", "ear.length",
              "tail.length", "forearm.length"), as.numeric)

write.csv(x = vertnet.sub, file = "vertnet.first.meas.csv")

#create long version
vertnet_mass <- subset(vertnet.sub, select = 1:24)
vertnet_tail.length <- subset(vertnet.sub, select = c(1:20, 37:40))
vertnet_total.length <- subset(vertnet.sub, select = c(1:20, 25:28))
vertnet_hindfoot.length <- subset(vertnet.sub, select = c(1:20, 29:32))
vertnet_forearm.length <- subset(vertnet.sub, select = c(1:20, 41:44))
vertnet_ear.length <- subset(vertnet.sub, select = c(1:20, 33:36))

#change column names
colnames(vertnet_mass)[colnames(vertnet_mass) %in% 
                 c("mass", "mass.verbatim.units",
                   "mass.units.inferred", "mass.estimated.value")] <- c("measurementValue", 
                                                                        "verbatimMeasurementUnit",
                                                                        "measurementUnitInferred",
                                                                        "measurementValueEstimated")
vertnet_mass$measurementType <- "mass"
vertnet_mass$measurementUnit <- "g"

colnames(vertnet_tail.length)[colnames(vertnet_tail.length) %in% 
                         c("tail.length", "tail.length.verbatim.units",
                           "tail.length.units.inferred", "tail.length.estimated.value")] <- c("measurementValue", 
                                                                                "verbatimMeasurementUnit",
                                                                                "measurementUnitInferred",
                                                                                "measurementValueEstimated")
vertnet_tail.length$measurementType <- "tail.length"
vertnet_tail.length$measurementUnit <- "mm"

colnames(vertnet_total.length)[colnames(vertnet_total.length) %in% 
                         c("total.length", "total.length.verbatim.units",
                           "total.length.units.inferred", "total.length.estimated.value")] <- c("measurementValue", 
                                                                                "verbatimMeasurementUnit",
                                                                                "measurementUnitInferred",
                                                                                "measurementValueEstimated")
vertnet_total.length$measurementType <- "total.length"
vertnet_total.length$measurementUnit <- "mm"

colnames(vertnet_hindfoot.length)[colnames(vertnet_hindfoot.length) %in% 
                         c("hindfoot.length", "hindfoot.length.verbatim.units",
                           "hindfoot.length.units.inferred", "hindfoot.length.estimated.value")] <- c("measurementValue", 
                                                                                "verbatimMeasurementUnit",
                                                                                "measurementUnitInferred",
                                                                                "measurementValueEstimated")
vertnet_hindfoot.length$measurementType <- "hindfoot.length"
vertnet_hindfoot.length$measurementUnit <- "mm"

colnames(vertnet_forearm.length)[colnames(vertnet_forearm.length) %in% 
                         c("forearm.length", "forearm.length.verbatim.units",
                           "forearm.length.units.inferred", "forearm.length.estimated.value")] <- c("measurementValue", 
                                                                                "verbatimMeasurementUnit",
                                                                                "measurementUnitInferred",
                                                                                "measurementValueEstimated")
vertnet_forearm.length$measurementType <- "forearm.length"
vertnet_forearm.length$measurementUnit <- "mm"

colnames(vertnet_ear.length)[colnames(vertnet_ear.length) %in% 
                         c("ear.length", "ear.length.verbatim.units",
                           "ear.length.units.inferred", "ear.length.estimated.value")] <- c("measurementValue", 
                                                                                "verbatimMeasurementUnit",
                                                                                "measurementUnitInferred",
                                                                                "measurementValueEstimated")
vertnet_ear.length$measurementType <- "ear.length"
vertnet_ear.length$measurementUnit <- "mm"

vertnet_long <- rbind(vertnet_mass, vertnet_tail.length, vertnet_total.length,
                      vertnet_forearm.length, vertnet_ear.length, vertnet_hindfoot.length)

vertnet_long$measurementStatus <- ""
vertnet_long$verbatimMeasurementValue <- ""

vertnet_long$origin <- "vertnet"

write.csv(vertnet_long, "vertnet.long.csv")

##clean FuTRES data----

##group lifeStages
futres$lifeStage[futres$lifeStage == "young adult" | futres$lifeStage == "Adult" | futres$lifeStage == "Prime Adult" | futres$lifeStage == "adult"] <- "Adult"
futres$lifeStage[futres$lifeStage == "Juvenile" | futres$lifeStage == "juvenile"] <- "Juvenile"

#trim dataset
futres.sub <- futres %>%
  dplyr::select(origin,
                scientificName, 
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

#create long version
futres_long <- melt(data = futres.sub, id.vars = 1:14, variable.name = "measurementType")
colnames(futres_long)[colnames(futres_long) == "value"] <- "measurementValue"
futres_long$verbatimMeasurementUnit <- ""
futres_long$measurementUnit <- ""
futres_long$measurementUnitInferred <- ""
futres_long$measurementStatus <- ""
futres_long$verbatimMeasurementValue <- ""
futres_long$measurementValueEstimated <- ""

futres_long$verbatimMeasurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "skinned"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "skinned"] 
futres_long$verbatimMeasurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "gutted"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "gutted"]
futres_long$verbatimMeasurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "mass"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "mass"]
futres_long$verbatimMeasurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "total.length"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "total.length"]

futres_long$verbatimMeasurementUnit[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "skinned"] <- "lb"
futres_long$verbatimMeasurementUnit[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "gutted"] <- "lb"
futres_long$verbatimMeasurementUnit[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "mass"] <- "lb"
futres_long$verbatimMeasurementUnit[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "total.length"] <- "in"

futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "skinned"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "skinned"] / .0022
futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "gutted"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "gutted"] / .0022
futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "mass"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "mass"] / .0022
futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "total.length"] <- futres_long$measurementValue[futres_long$scientificName == "Puma concolor" & futres_long$measurementType == "total.length"] * 25.4

futres_long$measurementUnit[futres_long$measurementType == "mass" | futres_long$measurementType == "gutted" | futres_long$measurementType == "skinned"] <- "g"
futres_long$measurementUnit[futres_long$measurementType != "mass" | futres_long$measurementType != "gutted" | futres_long$measurementType != "skinned"] <- "g"

write.csv(futres_long, "futres.long.csv")

##Combine VertNet and FuTRES Data----

##select out columns

col.order <- c("origin", "scientificName", "lifeStage", "sex", "reproductiveCondition",
           "catalogNumber", "materialSampleID", "institutionCode", "collectionCode", "eventDate",
           "locality", "higherGeography", "continent", "country", "county",
           "waterBody", "island", "islandGroup",
           "decimalLatitude", "decimalLongitude", "elevation",
           "measurementType", "measurementValue", "measurementUnit", 
           "verbatimMeasurementValue", "verbatimMeasurementUnit", 
           "measurementUnitInferred", "measurementValueEstimated", "measurementStatus")

##futres
#add in missing columns
emptyvector <- c("institutionCode", "collectionCode", "higherGeography",
                "waterBody", "island", "islandGroup", "continent") 

futres_long[ , emptyvector] <- ""

#check that columns are the same
setdiff(colnames(futres_long), colnames(vertnet_long))
setdiff(colnames(vertnet_long), colnames(futres_long))
setdiff(col.order, colnames(vertnet_long))
setdiff(col.order, colnames(futres_long))

##comnbine datasets
futres.order <- futres_long[, col.order]
vertnet.order <- vertnet_long[, col.order]

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

data.binom$lifeStage[data.binom$lifeStage == "--" | data.binom$lifeStage == ""] <- "NS"
data.binom$measurementUnitInferred[data.binom$measurementUnitInferred == "False" | data.binom$measurementUnitInferred == "FALSE"] <- ""
data.binom$measurementUnitInferred[data.binom$measurementUnitInferred == "True" | data.binom$measurementUnitInferred == "TRUE"] <- "T"

##write data file with futres and vertnet combined----
write.csv(data.binom, "dirty.data.csv")

##Figure 1 panel 1: lifeStage----
data.fig1 <- data.binom
length(data.fig1$measurementValue[data.fig1$scientificName == "Artibeus jamaicensis" & 
                                    data.fig1$measurementType == "mass" & 
                                    !is.na(data.fig1$measurementValue)]) #1406
length(data.fig1$measurementValue[data.fig1$scientificName == "Peromyscus maniculatus" & 
                                    data.fig1$measurementType == "mass" & 
                                    !is.na(data.fig1$measurementValue)]) #31669
length(data.fig1$measurementValue[data.fig1$scientificName == "Spermophilus beecheyi" & 
                                    data.fig1$measurementType == "mass" & 
                                    !is.na(data.fig1$measurementValue)]) #233
length(data.fig1$measurementValue[data.fig1$scientificName == "Odocoileus virginianus" & 
                                    data.fig1$measurementType == "mass" & 
                                    !is.na(data.fig1$measurementValue)]) #932

ccQuality <- c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue", "lightsteelblue1")

#care about estimated and lifeStage
data.fig1$cat <- paste(data.fig1$lifeStage, data.fig1$measurementValueEstimated)
unique(data.fig1$cat)
data.fig1$cat[data.fig1$cat == "NS NA" |
                data.fig1$cat == "NS "] <- "No stage; data possibly good" #lightslateblue
data.fig1$cat[data.fig1$cat == "Adult NA" |
                data.fig1$cat == "Adult "] <- "Adult; data possibly good" #darkslateblue
data.fig1$cat[data.fig1$cat == "Juvenile NA" |
                data.fig1$cat == "Juvenile "] <- "Juvenile; data possibly good" #mediumslateblue

data.fig1$cat[data.fig1$cat == "NS True"] <- "No stage; data estimated" #lightsteelblue1
data.fig1$cat[data.fig1$cat == "Adult True"] <- "Adult; data estimated" #lightsteelblue4
data.fig1$cat[data.fig1$cat == "Juvenile True"] <- "Juvenile; data estimated" #lightslateblue

data.fig1$cat <- as.factor(data.fig1$cat)
data.fig1$cat = relevel(data.fig1$cat, "Adult; data possibly good")
data.fig1$cat <- factor(data.fig1$cat, levels = c("Adult; data possibly good", "Adult; data estimated", 
                                                  "Juvenile; data possibly good", "Juvenile; data estimated",
                                                  "No stage; data possibly good", "No stage; data estimated"))
df <- subset(data.fig1, data.fig1$scientificName == "Artibeus jamaicensis" & 
               data.fig1$measurementType == "mass" & 
               !is.na(data.fig1$measurementValue))
length(df$measurementType) #1406
unique(df$cat)
length(df$cat[df$cat == "Adult; data possibly good"]) #247
length(df$cat[df$cat == "Juvenile; data possibly good"]) #12
length(df$cat[df$cat == "No stage; data possibly good"]) #1147

p <- ggplot(data = df) + 
  geom_density(aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightslateblue"),
                    name = "Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Artibeus jamaicensis N = 1406") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 250)) +
  scale_y_continuous(name = "Density", limits = c(0, .15)) #.7
ggsave(p, file=paste0("orig.dist.lifeStage.bat",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig1, data.fig1$scientificName == "Peromyscus maniculatus" & 
               data.fig1$measurementType == "mass" & 
               !is.na(data.fig1$measurementValue))
length(df$measurementValue) #31669
unique(df$cat)
length(df$cat[df$cat == "Adult; data possibly good"]) #3025
length(df$cat[df$cat == "Juvenile; data possibly good"]) #952
length(df$cat[df$cat == "No stage; data possibly good"]) #27588
length(df$cat[df$cat == "No stage; data estimated"]) #104
p <- ggplot(data = df) + 
  geom_density(aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue1"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Peromyscus maniculatus N = 31669") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .2))
ggsave(p, file=paste0("orig.dist.lifeStage.mouse",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig1, data.fig1$scientificName == "Spermophilus beecheyi" & 
               data.fig1$measurementType == "mass" & 
               !is.na(data.fig1$measurementValue))
length(df$measurementValue) #233
unique(df$cat)
length(df$cat[df$cat == "Adult; data possibly good"]) #114
length(df$cat[df$cat == "Juvenile; data possibly good"]) #11
length(df$cat[df$cat == "No stage; data possibly good"]) #107
length(df$cat[df$cat == "No stage; data estimated"]) #1
p <- ggplot(data = df) + 
  geom_density(aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue1"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Spermophilus beecheyi N = 233") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .02)) #.2?
ggsave(p, file=paste0("orig.dist.lifeStage.squirrel",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig1, data.fig1$scientificName == "Odocoileus virginianus" & 
               data.fig1$measurementType == "mass" & 
               !is.na(data.fig1$measurementValue))
length(df$measurementValue) #932
unique(df$cat)
length(df$cat[df$cat == "Adult; data possibly good"]) #4
length(df$cat[df$cat == "Juvenile; data possibly good"]) #2
length(df$cat[df$cat == "No stage; data possibly good"]) #827
length(df$cat[df$cat == "No stage; data estimated"]) #99
p <- ggplot(data = df) + 
  geom_density(aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue1"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Odocoileus virginianus N = 932") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 80000)) +
  scale_y_continuous(name = "Density", limits = c(0, .001))
ggsave(p, file=paste0("orig.dist.lifeStage.deer2",".png"), width = 14, height = 10, units = "cm")

## TO DO:----
#make all traits with values of "0" <- NA
#only like 80 records for mass, so likely not too big of an issue

data.binom$measurementValue[data.binom$measurementValue == 0] <- NA

##Mahalanobis Outlier test----
data.test <- data.binom

#add rownames for indexing
rownames(data.test) <- seq(1, nrow(data.test),1)

#need to select out trait
#remove NAs from trait
#apply maha() function
#select out indices
#label those rows as outliers

#want to find outliers for known adults and non-estimated values
sp <- unique(data.test$scientificName)

for(i in 1:length(sp)){
  sub <- subset(data.test, subset = data.test[,"scientificName"] == sp[i] & 
                  data.test[,"measurementValueEstimated"] != "True" & 
                  data.test[,"measurementType"] == "mass" & 
                  data.test[,"lifeStage"] == "Adult", 
                select = "measurementValue") %>%
    drop_na()
  if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      data.test[index,"measurementStatus"] <- "outlier"
    }
  }
  else if(isTRUE(nrow(sub) <= 10)){
      data.test$measurementStatus[data.test$scientificName == sp[i]] <- "too few records"
    }
  else{
    next
  }
 #return(data.test)
}

for(i in 1:length(sp)){
  sub <- subset(data.test, subset = data.test[,"scientificName"] == sp[i] & 
                  data.test[,"measurementValueEstimated"] != "True" & 
                  data.test[,"measurementType"] == "total.length" & 
                  data.test[,"lifeStage"] == "Adult", 
                select = "measurementValue") %>%
    drop_na()
  if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      data.test[index,"measurementStatus"] <- "outlier"
    }
  }
  else if(isTRUE(nrow(sub) <= 10)){
    data.test$measurementStatus[data.test$scientificName == sp[i]] <- "too few records"
  }
  else{
    next
  }
  #return(data.test)
}

for(i in 1:length(sp)){
  sub <- subset(data.test, subset = data.test[,"scientificName"] == sp[i] & 
                  data.test[,"measurementValueEstimated"] != "True" & 
                  data.test[,"measurementType"] == "tail.length" & 
                  data.test[,"lifeStage"] == "Adult", 
                select = "measurementValue") %>%
    drop_na()
  if(isTRUE(nrow(sub) == 0)){
    next
  }
  else if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      data.test[index,"measurementStatus"] <- "outlier"
    }
  }
  else if(isTRUE(nrow(sub) <= 10)){
    data.test$measurementStatus[data.test$scientificName == sp[i]] <- "too few records"
  }
  else{
    next
  }
  #return(data.test)
}

##write out Mahalanobis outlier test data----
data.mh <- data.test
length(unique(data.mh$scientificName)) #4346
write.csv(data.mh, "mh.outlier.checked.data.csv")
#data.mh <- read.csv("mh.outlier.checked.csv", header = TRUE)

##Figure 1, panel 2: outliers----
#cQuality <- c("darkslateblue", "mediumslateblue", "lightslateblue", "lightsteelblue4", "lightsteelblue", "lightsteelblue1")
data.fig2 <- data.mh[data.mh$lifeStage != "Juvenile" & 
                       data.mh$measurementStatus != "too few records",]

data.fig2$cat <- paste(data.fig2$lifeStage, data.fig2$measurementStatus)
unique(data.fig2$cat)
data.fig2$cat[data.fig2$cat == "Adult "] <- "Adult; possibly good" #darkslateblue
data.fig2$cat[data.fig2$cat == "Adult outlier"] <- "Adult; outlier" #lightsteelblue4
data.fig2$cat[data.fig2$cat == "NS "] <- "No stage; untested" #lightslateblue
#lightsteelblue1
#lightsteelblue
#mediumslateblue

data.fig2$cat <- as.factor(data.fig2$cat)
data.fig2$cat = relevel(data.fig2$cat, "Adult; possibly good")
data.fig2$cat <- factor(data.fig2$cat, levels = c("Adult; possibly good", "Adult; outlier", "No stage; untested"))
                                                    
# df <- subset(data.fig2, data.fig2$scientificName == "Artibeus jamaicensis" &
#              data.fig2$measurementType == "mass" &
#              !is.na(data.fig2$measurementValue))
# length(df$measurementType) #1382
# unique(df$cat)
# length(df$cat[df$cat == "possibly good; units known"]) #1223
# length(df$cat[df$cat == "untested; units inferred"]) #159
# 
# p <- ggplot() + 
#   #geom_histogram(data = filter(df, mass.status == "outlier"), aes(x = log10(mass)), color = "darkgray", alpha = 0.3, binwidth = .005, boundary = TRUE) +
#   geom_density(data = filter(df, measurementStatus == "outlier; units known"), aes(x = measurementValue), color = NA, alpha = 0.4) + 
#   geom_rug(data = filter(df, measurementStatus == "outlier; units known"), aes(x = measurementValue), sides = "b", col = "gray34") +
#   geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4) +
#   scale_fill_manual(values = c("darkslateblue", "lightsteelblue1"),
#                     name = "Data Quality Category") +
#   #geom_density(data = df, aes(x = mass), color = "darkgray", fill = cat, alpha = 0.9) +
#   ggtitle("Artibeus jamaicensis N = 1382") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   scale_x_continuous(name = "Body Mass (g)", limits = c(0, 250)) +
#   scale_y_continuous(name = "Density", limits = c(0, .15)) #.7
# ggsave(p, file=paste0("outlier.test.bat",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig2, data.fig2$scientificName == "Peromyscus maniculatus" & 
               data.fig2$measurementType == "mass" &
               !is.na(data.fig2$measurementValue))
length(df$measurementType) #30708
unique(df$cat)
length(df$cat[df$cat == "Adult; possibly good"]) #3020
length(df$cat[df$cat == "Adult; outlier"]) #1
length(df$cat[df$cat == "No stage; untested"]) #27687
p <- ggplot() + 
  geom_density(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "lightsteelblue4", "lightslateblue"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30708, Noutlier = 1") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .2))
ggsave(p, file=paste0("outlier.test.mouse",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig2, data.fig2$scientificName == "Spermophilus beecheyi" & 
               data.fig2$measurementType == "mass" &
               !is.na(data.fig2$measurementValue))
length(df$measurementType) #222
unique(df$cat)
length(df$cat[df$cat == "Adult; possibly good"]) #107
length(df$cat[df$cat == "Adult; outlier"]) #7
length(df$cat[df$cat == "No stage; untested"]) #108
p <- ggplot() + 
  geom_density(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
  #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9) +
  #scale_fill_manual(values = ccStatus,
  #                  name="Mass Status") +
  geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkslateblue", "lightsteelblue4", "lightslateblue"),
                    name = "Data Quality Category") +
  ggtitle("Spermophilus beecheyi N = 222, Noutlier = 7") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .02))
ggsave(p, file=paste0("outlier.test.squirrel",".png"), width = 14, height = 10, units = "cm")

# df <- subset(data.fig2, data.fig2$scientificName == "Odocoileus virginianus" & 
#                data.fig2$measurementType == "mass" &
#                !is.na(data.fig2$measurementValue))
# length(df$measurementType) #930
# unique(df$cat)
# length(df$cat[df$cat == "possibly good; units known"]) #908
# length(df$cat[df$cat == "untested; units inferred"]) #22
# p <- ggplot() + 
#   geom_density(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, adjust = 1/10, alpha = 0.4) +
#   geom_rug(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
#   #geom_density(data = filter(df, mass.status != "outlier"), aes(x = mass), color = "darkgray", fill = "darkgray", alpha = 0.9, adjust = 1/10) +
#   #geom_density(data = df, aes(x = log10(mass), fill = mass.status), alpha = 0.9) +
#   #scale_fill_manual(values = ccStatus,
#   #                  name="Mass Status") +
#   geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4, adjust = 1/10) +
#   scale_fill_manual(values = c("darkslateblue", "lightsteelblue1"),
#                     name = "Data Quality Category") +
#   ggtitle("Odocoileus virginianus N = 930") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   scale_x_continuous(name = "Body Mass (g)", limits = c(0, 80000)) +
#   scale_y_continuous(name = "Density", limits = c(0, .001))#.0001
# ggsave(p, file=paste0("outlier.test.deer",".png"), width = 14, height = 10, units = "cm")

data.noInfer_Adults <- subset(data.mh, subset = c(data.mh$measurementStatus != "outlier" &
                                                    data.mh$measurementStatus != "too few records" &
                                                    data.mh$lifeStage == "Adult" &
                                                    data.mh$measurementValueEstimated != "True"))
data.noInfer_stats <- data.noInfer_Adults %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(sample.size.mass = length(measurementValue[measurementType == "mass" & !is.na(measurementValue)]),
                   avg.mass = mean(measurementValue[measurementType == "mass" & !is.na(measurementValue)], na.rm = TRUE),
                   sigma.mass = sd(measurementValue[measurementType == "mass" & !is.na(measurementValue)], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (3*sigma.mass),
                   lower.limit.mass = avg.mass - (3*sigma.mass),
                   
                   sample.size.length = length(measurementValue[measurementType == "total.length" & !is.na(measurementValue)]),
                   avg.length = mean(measurementValue[measurementType == "total.length" & !is.na(measurementValue)], na.rm = TRUE),
                   sigma.length = sd(measurementValue[measurementType == "total.length" & !is.na(measurementValue)], na.rm = TRUE),
                   upper.limit.length = avg.length + (3*sigma.length),
                   lower.limit.length = avg.length - (3*sigma.length),
                   
                   sample.size.tail = length(measurementValue[measurementType == "total.length" & !is.na(measurementValue)]),
                   avg.tail.length = mean(measurementValue[measurementType == "total.length" & !is.na(measurementValue)], na.rm = TRUE),
                   sigma.tail = sd(measurementValue[measurementType == "total.length" & !is.na(measurementValue)], na.rm = TRUE),
                   upper.limit.tail = avg.tail.length + (3*sigma.tail),
                   lower.limit.tail = avg.tail.length - (3*sigma.tail)) %>%
  as.data.frame()
nrow(data.noInfer_stats) #251

##add stats to dataframe
data.limit <- merge(data.mh, data.noInfer_stats, by = "scientificName", all.x = TRUE, all.y = FALSE)
length(unique(data.limit$scientificName)) #4346

write.csv(data.limit, "data.limit.csv")

##label outliers
#label samples that are outside of limits with outlier, and label those within limits as "g" and inferred = TRUE

data.check <- data.limit

#mass
for(i in 1:nrow(data.check["measurementType" == "mass" &
                           "measurementStatus" != "too few records" & 
                           "measurementStatus" != "outlier"])){
  if(isTRUE(data.check$measurementValue[i] <= data.check$lower.limit.mass[i])){
    data.check$measurementStatus[i] <- "outlier"
  }
  else if(isTRUE(data.check$measurementValue[i] >= data.check$upper.limit.mass[i])){
    data.check$measurementStatus[i] <- "outlier"
  }
  else{
    data.check$measurementStatus[i] <- "possibly good"
  }
}
#return(data)

for(i in 1:nrow(data.check["measurementType" == "total.length" &
                           "measurementStatus" != "too few records" & 
                           "measurementStatus" != "outlier"])){
  if(isTRUE(data.check$measurementValue[i] <= data.check$lower.limit.mass[i])){
    data.check$measurementStatus[i] <- "outlier"
  }
  else if(isTRUE(data.check$measurementValue[i] >= data.check$upper.limit.mass[i])){
    data.check$measurementStatus[i] <- "outlier"
  }
  else{
    data.check$measurementStatus[i] <- "possibly good"
  }
}

for(i in 1:nrow(data.check["measurementType" == "taill.length" &
                           "measurementStatus" != "too few records" & 
                           "measurementStatus" != "outlier"])){
  if(isTRUE(data.check$measurementValue[i] <= data.check$lower.limit.mass[i])){
    data.check$measurementStatus[i] <- "outlier"
  }
  else if(isTRUE(data.check$measurementValue[i] >= data.check$upper.limit.mass[i])){
    data.check$measurementStatus[i] <- "outlier"
  }
  else{
    data.check$measurementStatus[i] <- "possibly good"
  }
}

##write out first round of upper and lower limits checking----
write.csv(data.check, "data.check.csv")

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
