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

vertnet$individualID <- seq(1, nrow(vertnet), 1) #ends at 659543

write.csv(vertnet, "vertnetcombinded.csv")

#vertnet data has a column header "units" which were the original units. All units have been changed to either "g" or "mm"
#change column names to reflect this

vertnet.sub <- vertnet %>%
  dplyr::select(individualID,
                scientificName = scientificname, 
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
vertnet_mass <- subset(vertnet.sub, select = 1:25)
vertnet_tail.length <- subset(vertnet.sub, select = c(1:21, 38:41))
vertnet_total.length <- subset(vertnet.sub, select = c(1:21, 26:29))
vertnet_hindfoot.length <- subset(vertnet.sub, select = c(1:21, 30:33))
vertnet_forearm.length <- subset(vertnet.sub, select = c(1:21, 42:45))
vertnet_ear.length <- subset(vertnet.sub, select = c(1:21, 34:37))

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

futres$individualID <- seq(1,nrow(futres),1)
futres$individualID <- futres$individualID+max(vertnet$individualID)

#trim dataset
futres.sub <- futres %>%
  dplyr::select(individualID,
                origin,
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
futres_long <- melt(data = futres.sub, id.vars = 1:15, variable.name = "measurementType")
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

col.order <- c("individualID","origin", "scientificName", "lifeStage", "sex", "reproductiveCondition",
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

#make all traits with values of "0" <- NA
#only like 80 records for mass, so likely not too big of an issue

data.binom$measurementValue[data.binom$measurementValue == 0] <- NA

#change known taxonomy error: Spermophilus beecheyi
data.binom$scientificName[data.binom$scientificName == "Spermophilus beecheyi"] <- "Otospermophilus beecheyi"

##write data file with futres and vertnet combined----
write.csv(data.binom, "dirty.data.csv")

##Figure 1 panel 1: lifeStage----
data.fig1 <- data.binom
length(data.fig1$measurementValue[data.fig1$scientificName == "Peromyscus maniculatus" & 
                                    data.fig1$measurementType == "mass" & 
                                    !is.na(data.fig1$measurementValue)]) #31669
length(data.fig1$measurementValue[data.fig1$scientificName == "Otospermophilus beecheyi" & 
                                    data.fig1$measurementType == "mass" & 
                                    !is.na(data.fig1$measurementValue)]) #233

#care about estimated and lifeStage
data.fig1$cat <- paste(data.fig1$lifeStage, data.fig1$measurementValueEstimated)
unique(data.fig1$cat)
data.fig1$cat[data.fig1$cat == "NS NA" |
                data.fig1$cat == "NS "] <- "No stage; data possibly good" #lightgoldenrod3
data.fig1$cat[data.fig1$cat == "Adult NA" |
                data.fig1$cat == "Adult "] <- "Adult; data possibly good" #darkorchid4
data.fig1$cat[data.fig1$cat == "Juvenile NA" |
                data.fig1$cat == "Juvenile "] <- "Juvenile; data possibly good" #gray74

data.fig1$cat[data.fig1$cat == "NS True"] <- "No stage; data estimated" #lightgoldenrod1
data.fig1$cat[data.fig1$cat == "Adult True"] <- "Adult; data estimated" #darkorchid
data.fig1$cat[data.fig1$cat == "Juvenile True"] <- "Juvenile; data estimated" #gray74

data.fig1$cat <- as.factor(data.fig1$cat)
data.fig1$cat = relevel(data.fig1$cat, "Adult; data possibly good")
data.fig1$cat <- factor(data.fig1$cat, levels = c("Adult; data possibly good", "Adult; data estimated", 
                                                  "Juvenile; data possibly good", "Juvenile; data estimated",
                                                  "No stage; data possibly good", "No stage; data estimated"))

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
  geom_density(aes(x = measurementValue, fill = cat), alpha = 0.6) +
  scale_fill_manual(values = c("darkorchid4", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Peromyscus maniculatus N = 31669") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .25))
ggsave(p, file=paste0("orig.dist.lifeStage.mouse",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig1, data.fig1$scientificName == "Otospermophilus beecheyi" & 
               data.fig1$measurementType == "mass" & 
               !is.na(data.fig1$measurementValue))
length(df$measurementValue) #233
length(df$measurementValue[df$lifeStage == "Adult"]) #114
length(df$measurementValue[df$lifeStage == "NS"]) #108
length(df$measurementValue[df$lifeStage == "Juvenile"]) #11
unique(df$cat)
length(df$cat[df$cat == "Adult; data possibly good"]) #114
length(df$cat[df$cat == "Juvenile; data possibly good"]) #11
length(df$cat[df$cat == "No stage; data possibly good"]) #107
length(df$cat[df$cat == "No stage; data estimated"]) #1
p <- ggplot(data = df) + 
  geom_density(aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                    name="Data Quality Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Otospermophilus beecheyi N = 233") +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .01))
ggsave(p, file=paste0("orig.dist.lifeStage.squirrel",".png"), width = 14, height = 10, units = "cm")

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
}

for(i in 1:length(sp)){
  sub <- subset(data.test, subset = data.test[,"scientificName"] == sp[i] & 
                  data.test[,"measurementValueEstimated"] != "True" & 
                  data.test[,"measurementType"] == "total.length" & 
                  data.test[,"lifeStage"] == "Adult", 
                select = "measurementValue") %>%
    drop_na()
  if(isTRUE(nrow(sub) == 0)){
    next
  }
  else if(isTRUE(length(unique(sub$measurementValue)) == 1)){
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
}

##write out Mahalanobis outlier test data----
data.mh <- data.test
length(unique(data.mh$scientificName)) #4346
write.csv(data.mh, "mh.outlier.checked.data.csv")

##Figure 1, panel 2: outliers----
data.fig2 <- data.mh[data.mh$lifeStage != "Juvenile" & 
                       data.mh$measurementStatus != "too few records",]

data.fig2$cat <- paste(data.fig2$lifeStage, data.fig2$measurementStatus)
unique(data.fig2$cat)
data.fig2$cat[data.fig2$cat == "Adult "] <- "Adult; possibly good" #darkorchid4
data.fig2$cat[data.fig2$cat == "Adult outlier"] <- "Adult; outlier" #darkorchid1
data.fig2$cat[data.fig2$cat == "NS "] <- "No stage; untested" #lightgoldenrod1

data.fig2$cat <- as.factor(data.fig2$cat)
data.fig2$cat = relevel(data.fig2$cat, "Adult; possibly good")
data.fig2$cat <- factor(data.fig2$cat, levels = c("Adult; possibly good", "Adult; outlier", "No stage; untested"))

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
  geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod1"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30708, Noutlier = 1") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .25))
#outlier @17100g
ggsave(p, file=paste0("outlier.test.mouse",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig2, data.fig2$scientificName == "Otospermophilus beecheyi" & 
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
  geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod1"),
                    name = "Data Quality Category") +
  ggtitle("Otospermophilus beecheyi N = 222, Noutlier = 7") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .01))
ggsave(p, file=paste0("outlier.test.squirrel",".png"), width = 14, height = 10, units = "cm")

##create upper and lower limits----

data.noInfer_Adults <- subset(data.mh, subset = c(data.mh$measurementStatus != "outlier" &
                                                    data.mh$measurementStatus != "too few records" &
                                                    data.mh$lifeStage == "Adult" &
                                                    data.mh$measurementValueEstimated != "True"))
data.noInfer_stats <- data.noInfer_Adults %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(sample.size.mass = length(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0]),
                   avg.mass = mean(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   sigma.mass = sd(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (3*sigma.mass),
                   lower.limit.mass = avg.mass - (3*sigma.mass),
                   
                   sample.size.tail = length(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0]),
                   avg.tail = mean(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   sigma.tail = sd(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   upper.limit.tail = avg.tail + (3*sigma.tail),
                   lower.limit.tail = avg.tail - (3*sigma.tail),
                   
                   sample.size.length = length(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0]),
                   avg.length = mean(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   sigma.length = sd(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   upper.limit.length = avg.length + (3*sigma.length),
                   lower.limit.length = avg.length - (3*sigma.length)) %>%
  as.data.frame()
nrow(data.noInfer_stats) #271
length(unique(data.noInfer_stats$scientificName)) #271

##add stats to dataframe
data.limit <- merge(data.mh, data.noInfer_stats, by = "scientificName", all.x = TRUE, all.y = FALSE)
length(unique(data.limit$scientificName)) #4346

##write out csv with limits----
write.csv(data.limit, "data.limit.csv")

##label outliers----
#label samples that are outside of limits with outlier, and label those within limits as "g" and inferred = TRUE
data.limit$index <- rownames(data.limit)
data.limit$measurementStatus[data.limit$sample.size.length < 10] <- "too few records"
data.limit$measurementStatus[data.limit$sample.size.mass < 10] <- "too few records"
data.limit$measurementStatus[data.limit$sample.size.tail < 10] <- "too few records"
data.limit$measurementStatus[is.na(data.limit$measurementValue)] <- "ignore"
data.limit$measurementValue[data.limit$measurementValue < 0 | is.infinite(data.limit$measurementValue)] <- ""
data.check <- data.limit[data.limit$measurementStatus != "too few records" & 
                         data.limit$measurementStatus != "outlier" &
                           data.limit$measurementStatus != "ignore",]
data.uncheck <- data.limit[data.limit$measurementStatus == "too few records" | 
                             data.limit$measurementStatus == "outlier" |
                             data.limit$measurementStatus == "ignore",]

sp <- unique(data.check$scientificName)
length(sp) #240
nrow(data.check) #1681159

#need to split up datasets to test
#mass
for(i in 1:length(sp)){
  sub <- data.check[data.check$scientificName == sp[i] & 
                  data.check$measurementType == "mass",]
  for(j in 1:nrow(sub)){
    if(isTRUE(sub$measurementValue[j] <= sub$lower.limit.mass[1])){
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly not adult"
    }
    else if(isTRUE(sub$measurementValue[j] >= sub$upper.limit.mass[1])){
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly not adult"
    }
    else{
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly adult"
    }
  }
} 

#total length
for(i in 1:length(sp)){
  sub <- data.check[data.check$scientificName == sp[i] & 
                      data.check$measurementType == "total.length",]
  for(j in 1:nrow(sub)){
    if(isTRUE(sub$measurementValue[j] <= sub$lower.limit.length[1])){
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly not adult"
    }
    else if(isTRUE(sub$measurementValue[j] >= sub$upper.limit.length[1])){
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly not adult"
    }
    else{
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly adult"
    }
  }
} 

#tail length
for(i in 1:length(sp)){
  sub <- data.check[data.check$scientificName == sp[i] & 
                      data.check$measurementType == "tail.length",]
  for(j in 1:nrow(sub)){
    if(isTRUE(sub$measurementValue[j] <= sub$lower.limit.tail[1])){
      data.check$measurementStatus[data.check$inde == sub$index[j]] <- "possibly not adult"
    }
    else if(isTRUE(sub$measurementValue[j] >= sub$upper.limit.tail[1])){
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly not adult"
    }
    else{
      data.check$measurementStatus[data.check$index == sub$index[j]] <- "possibly adult"
    }
  }
} 

data.total <- rbind(data.check, data.uncheck)


##write out first round of upper and lower limits checking----
write.csv(data.check, "data.check.csv")
write.csv(data.total, "data.total.csv")

##Figure 1, Panel 3----
data.fig3 <- data.total[data.total$lifeStage != "Juvenile",]

data.fig3$cat <- paste(data.fig3$lifeStage, data.fig3$measurementStatus) #weird, no units aren't inferred and GOOD
unique(data.fig3$cat)
data.fig3$cat[data.fig3$cat == "Adult possibly good"] <- "Adult; possibly good" #darkorchid4
data.fig3$cat[data.fig3$cat == "Adult outlier"] <- "Adult; outlier" #darkorchid1
data.fig3$cat[data.fig3$cat == "Adult too few records"] <- "Adult; too few records" #gray74
data.fig3$cat[data.fig3$cat == "Adult "] <- "Adult; untested" #darkorchid
data.fig3$cat[data.fig3$cat == "NS possibly good"] <- "No stage; possibly good" #lightgoldenrod3
data.fig3$cat[data.fig3$cat == "NS outlier"] <- "No stage; outlier" #lightgoldenrodyellow
data.fig3$cat[data.fig3$cat == "NS too few records"] <- "No stage; too few records" #gray74
data.fig3$cat[data.fig3$cat == "NS "] <- "No stage; untested" #lightgoldenrod1

data.fig3$cat <- as.factor(data.fig3$cat)
data.fig3$cat = relevel(data.fig3$cat, "Adult; possibly good")
data.fig3$cat <- factor(data.fig3$cat, levels = c("Adult; possibly good", "Adult; outlier", "Adult; untested", "Adult; too few records", 
                                                  "No stage; possibly good", "No stage; outlier", "No stage; untested", "No stage; too few records"))

df <- subset(data.fig3, data.fig3$scientificName == "Peromyscus maniculatus" & 
               data.fig3$measurementType == "mass" &
               !is.na(data.fig3$measurementValue))
df$measurementValue <- as.numeric(df$measurementValue)
length(df$measurementValue) #30708
unique(df$cat)
length(df$cat[df$cat == "No stage; possibly good"]) #9554
length(df$cat[df$cat == "Adult; possibly good"]) #1060
length(df$cat[df$cat == "Adult; outlier"]) #1961
length(df$cat[df$cat == "No stage; outlier"]) #18133
p <- ggplot() + 
  geom_density(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
  geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod3", "lightgoldenrodyellow"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30708, Noutlier = 20094") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .25))
ggsave(p, file=paste0("check.test.mouse",".png"), width = 14, height = 10, units = "cm")

df <- subset(data.fig3, data.fig3$scientificName == "Otospermophilus beecheyi" & 
               data.fig3$measurementType == "mass" &
               !is.na(data.fig3$measurementValue))
df$measurementValue <- as.numeric(df$measurementValue)
length(df$measurementValue) #222
unique(df$cat)
length(df$cat[df$cat == "No stage; possibly good"]) #102
length(df$cat[df$cat == "Adult; possibly good"]) #107
length(df$cat[df$cat == "Adult; outlier"]) #7
length(df$cat[df$cat == "No stage; outlier"]) #6
p <- ggplot() + 
  geom_density(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, alpha = 0.4) +
  geom_rug(data = filter(df, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
  geom_density(data = df, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod3", "lightgoldenrodyellow"),
                    name = "Data Quality Category") +
  ggtitle("Otospermophilus beecheyi N = 222, Noutlier = 13") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .01))
ggsave(p, file=paste0("check.test.squirrel",".png"), width = 14, height = 10, units = "cm")

##info about outliers----
outlier_stats <- data.total %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.outlier.mass = length(measurementValue[measurementStatus == "outlier" & 
                                                                   measurementValue >= 0 & measurementType == "mass" &
                                                                   lifeStage != "Juvenile"]),
                   sample.mass = length(measurementValue[measurementStatus == "possibly good" & 
                                                           measurementValue >= 0 & 
                                                           measurementType == "mass" &
                                                           lifeStage != "Juvenile"])) %>%
  as.data.frame()

sum(outlier_stats$sample.outlier.mass) #154672


##write out csv of outlier stats----
write.csv(outlier_stats, "outliers.csv")

##put Ray's Horses into FuTRES----
#Ray's data was preprocessed separately; see github.com/futres/fovt_data_mapping
#only focusing on astragalus length, width, mass, and calcaneus length, width
ray <- read.csv("https://de.cyverse.org/dl/d/223A09E1-EFC5-441A-8C1B-91E7AEFEA011/FuTRES_Equid_Bernor_Subset.csv", header = TRUE)
setdiff(colnames(ray), colnames(data.total))

data.total$diagnosticID <- paste(data.total$catalogNumber, "-", data.total$institutionCode)
data.total$basisOfRecord <- rep("preserved specimen", nrow(data.total))
data.total$samplingProtocol <- rep("", nrow(data.total))
data.total$verbatimEventDate <- rep("", nrow(data.total))
data.total$yearCollected <- rep("", nrow(data.total))
data.total$minimumChronometricAge <- rep("", nrow(data.total))
data.total$maximumChronometricAge <- rep("", nrow(data.total))
data.total$minimumChronometricAgeReferenceSystem <- rep("", nrow(data.total))
data.total$maximumChronometricAgeReferenceSystem <- rep("", nrow(data.total))
data.total$measurementMethod <- rep("", nrow(data.total))
data.total$side <- rep("", nrow(data.total))

ray$lifeStage <- rep("", nrow(ray))
ray$sex <- rep("", nrow(ray))
ray$locality <- rep("protected", nrow(ray))
ray$elevation <- rep("", nrow(ray))
ray$decimalLatitude <- rep("", nrow(ray))
ray$decimalLongitude <- rep("", nrow(ray))
ray$continent <- rep("", nrow(ray))
ray$county <- rep("", nrow(ray))
ray$eventDate <- rep("", nrow(ray))
ray$institutionCode <- rep("", nrow(ray))
ray$collectionCode <- rep("", nrow(ray))
ray$higherGeography <- rep("", nrow(ray))
ray$waterBody <- rep("", nrow(ray))
ray$island <- rep("", nrow(ray))
ray$islandGroup <- rep("", nrow(ray))
ray$reproductiveCondition <- rep("", nrow(ray))
ray$measurementUnitInferred <- rep("", nrow(ray))
ray$measurementStatus <- rep("", nrow(ray))
ray$catalogNumber <- rep("", nrow(ray))
ray$verbatimMeasurementValue <- rep("", nrow(ray))
ray$verbatimMeasurementUnit <- rep("", nrow(ray))
ray$measurementValueEstimated <- rep("", nrow(ray))
ray$sigma.mass <- rep("", nrow(ray))
ray$avg.mass <- rep("", nrow(ray))
ray$sample.size.mass <- rep("", nrow(ray))
ray$upper.limit.mass <- rep("", nrow(ray))
ray$lower.limit.mass <- rep("", nrow(ray))
ray$origin <- rep("ray", nrow(ray))
ray$measurementType[ray$measurementType == "astragalus length"] <- "astragalus.length"
ray$measurementType[ray$measurementType == "talus breadth"] <- "astragalus.width"
ray$measurementType[ray$measurementType == "calcaneus length"] <- "calcaneus.GL"
ray$measurementType[ray$measurementType == "calcaneus breadth"] <- "calcaneus.GB"
ray$measurementType[ray$measurementType == "calcaneus breadth"] <- "calcaneus.GB"
ray$measurementType[ray$measurementType == "humerus length to ventral tubercle"] <- "humerus.length"
ray$measurementType[ray$measurementType == "humerus length to caput of humerus"] <- "humerus.length"

ray.sub <- ray %>%
  dplyr::select(scientificName, 
                individualID,
                diagnosticID,
                materialSampleID,
                lifeStage, 
                sex,
                locality,
                samplingProtocol,
                yearCollected,
                minimumChronometricAge,
                minimumChronometricAgeReferenceSystem,
                maximumChronometricAge,
                maximumChronometricAgeReferenceSystem,
                basisOfRecord,
                verbatimEventDate,
                side,
                elevation,
                catalogNumber,
                reproductiveCondition,
                decimalLatitude,
                decimalLongitude,
                continent,
                country,
                county,
                eventDate,
                institutionCode,
                collectionCode,
                higherGeography,
                waterBody,
                island,
                islandGroup,
                measurementType,
                measurementValue,
                measurementUnit,
                measurementMethod,
                measurementUnitInferred,
                measurementStatus,
                verbatimMeasurementUnit,
                verbatimMeasurementValue,
                measurementValueEstimated,
                sample.size.mass,
                avg.mass,
                sigma.mass,
                upper.limit.mass,
                lower.limit.mass,
                origin) %>%
  mutate_at("measurementValue", as.numeric)

data.total.sub <- data.total %>%
  dplyr::select(scientificName, 
                individualID,
                diagnosticID,
                materialSampleID,
                lifeStage, 
                sex,
                locality,
                samplingProtocol,
                yearCollected,
                minimumChronometricAge,
                minimumChronometricAgeReferenceSystem,
                maximumChronometricAge,
                maximumChronometricAgeReferenceSystem,
                basisOfRecord,
                verbatimEventDate,
                side,
                elevation,
                catalogNumber,
                reproductiveCondition,
                decimalLatitude,
                decimalLongitude,
                continent,
                country,
                county,
                eventDate,
                institutionCode,
                collectionCode,
                higherGeography,
                waterBody,
                island,
                islandGroup,
                measurementType,
                measurementValue,
                measurementUnit,
                measurementMethod,
                measurementUnitInferred,
                measurementStatus,
                verbatimMeasurementUnit,
                verbatimMeasurementValue,
                measurementValueEstimated,
                sample.size.mass,
                avg.mass,
                sigma.mass,
                upper.limit.mass,
                lower.limit.mass,
                origin) %>%
  mutate_at("measurementValue", as.numeric)

setdiff(colnames(ray.sub), colnames(data.total.sub))
setdiff(colnames(data.total.sub), colnames(ray.sub))

data.all <- rbind(data.total.sub, ray.sub)
write.csv(data.all, "data.all.csv")

##stats about data----
data.clean <- data.all[!is.na(data.all$measurementValue),]
data.clean <- data.clean[data.clean$measurementStatus != "outlier" & 
                     data.clean$lifeStage != "Juvenile",]

nrow(data.all[data.all$measurementStatus == "possibly good",]) #107429 records

nrow(data.clean) #2094245
length(unique(data.clean$scientificName)) #3958
length(unique(data.clean$catalogNumber)) + length(unique(data.clean$individualID)) #336746

nrow(data.clean[data.clean$measurementType == "mass" & !is.na(data.clean$measurementValue),]) #196098
nrow(data.clean[data.clean$measurementType == "total.length" & !is.na(data.clean$measurementValue),]) #525733
nrow(data.clean[data.clean$measurementType == "tail.length" & !is.na(data.clean$measurementValue),]) #473211
nrow(data.clean[data.clean$measurementType == "ear.length" & !is.na(data.clean$measurementValue),]) #406953
nrow(data.clean[data.clean$measurementType == "forearm.length" & !is.na(data.clean$measurementValue),]) #19346
nrow(data.clean[data.clean$measurementType == "astragalus.length" & !is.na(data.clean$measurementValue),]) #767
nrow(data.clean[data.clean$measurementType == "hindfoot.length" & !is.na(data.clean$measurementValue),]) #469877
nrow(data.clean[data.clean$measurementType == "astragalus.width" & !is.na(data.clean$measurementValue),]) #733
nrow(data.clean[data.clean$measurementType == "tooth.row" & !is.na(data.clean$measurementValue),]) #288
nrow(data.clean[data.clean$measurementType == "humerus.length" & !is.na(data.clean$measurementValue),]) #59
nrow(data.clean[data.clean$measurementType == "calcaneus.GB" & !is.na(data.clean$measurementValue),]) #341
nrow(data.clean[data.clean$measurementType == "calcaneus.GL" & !is.na(data.clean$measurementValue),]) #308

length(unique(data.clean$scientificName[data.clean$measurementType == "mass" & !is.na(data.clean$measurementValue)])) #2357
length(unique(data.clean$scientificName[data.clean$measurementType == "total.length" & !is.na(data.clean$measurementValue)])) #3755
length(unique(data.clean$scientificName[data.clean$measurementType == "tail.length" & !is.na(data.clean$measurementValue)])) #2854
length(unique(data.clean$scientificName[data.clean$measurementType == "ear.length" & !is.na(data.clean$measurementValue)])) #2714
length(unique(data.clean$scientificName[data.clean$measurementType == "forearm.length" & !is.na(data.clean$measurementValue)])) #614
length(unique(data.clean$scientificName[data.clean$measurementType == "astragalus.length" & !is.na(data.clean$measurementValue)])) #78
length(unique(data.clean$scientificName[data.clean$measurementType == "hindfoot.length" & !is.na(data.clean$measurementValue)])) #2789
length(unique(data.clean$scientificName[data.clean$measurementType == "astragalus.width" & !is.na(data.clean$measurementValue)])) #76
length(unique(data.clean$scientificName[data.clean$measurementType == "tooth.row" & !is.na(data.clean$measurementValue)])) #1
length(unique(data.clean$scientificName[data.clean$measurementType == "humerus.length" & !is.na(data.clean$measurementValue)])) #12
length(unique(data.clean$scientificName[data.clean$measurementType == "calcaneus.GB" & !is.na(data.clean$measurementValue)])) #51
length(unique(data.clean$scientificName[data.clean$measurementType == "calcaneus.GL" & !is.na(data.clean$measurementValue)])) #48


nrow(data.clean[data.clean$origin == "ray" & data.clean$measurementType == "astragalus.length" & !is.na(data.clean$measurementValue),]) #722
nrow(data.clean[data.clean$origin == "ray" & data.clean$measurementType == "astragalus.width" & !is.na(data.clean$measurementValue),]) #688
nrow(data.clean[data.clean$origin == "ray" & data.clean$measurementType == "calcaneus.GL" & !is.na(data.clean$measurementValue),]) #289
nrow(data.clean[data.clean$origin == "ray" & data.clean$measurementType == "calcaneus.GB" & !is.na(data.clean$measurementValue),]) #311
nrow(data.clean[data.clean$origin == "ray" & data.clean$measurementType == "humerus.length" & !is.na(data.clean$measurementValue),]) #45




