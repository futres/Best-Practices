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
#require(OutlierDetection)
#call maha function from https://cran.r-project.org/src/contrib/Archive/OutlierDetection/
require(utils)

##Load data----

options(stringsAsFactors = FALSE)

## FuTRES data
#about futres data:
#has various terms for adult and juvenile
#currently aligned manually; fixed Amelia's weight data to be g, deleted lone mass that did not have units
#need to fix OR units (currently in lb and in)
#note: astragalus width is actually astragalus breadth
# Vertnet data
#extracted from R. LaFrance using Traiter to extract traits and trait values in fields like "dynamicProperties"
#inferred means that units were converted to g or mm
#estimated means that trait values were in non-standard ways (e.g., [5])
#lots of lifeStage info missing
#for traits we're interested in (e.g., not sided traits like testes and ovaries), use first measurement
#too big for GEOME, stored in the CyVerse datastore in parts and reassembled here

#futres download from jdeck88
geome.processed <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/BestPracticesData/V2/futres_data_processed.csv", header = TRUE)

#Villaseñor = Project 277
p277 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/BestPracticesData/V2/project_277.csv", header = TRUE, stringsAsFactors = FALSE)
#Blois 2018 = Project 278
p278 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/BestPracticesData/V2/project_278.csv", header = TRUE, stringsAsFactors = FALSE)
#EAP K.Emery = Project 282
p282 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/BestPracticesData/V2/project_282.csv", header = TRUE, stringsAsFactors = FALSE)
#ODFW = Project 294
p294 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/BestPracticesData/V2/project_294.csv", header = TRUE, stringsAsFactors = FALSE)
#Bernor = Project 314
p314 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/BestPracticesData/V2/project_314.csv", header = TRUE, stringsAsFactors = FALSE)

geome <- rbind(p277, p278, p282, p294, p314)

#vertnet
vertnet1 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/ValidatedData/FuTRES_Mammals_VertNet_Global_Modern_1.csv", header = TRUE)
vertnet2 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/ValidatedData/FuTRES_Mammals_VertNet_Global_Modern_2.csv", header = TRUE)
vertnet3 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/ValidatedData/FuTRES_Mammals_VertNet_Global_Modern_3.csv", header = TRUE)
vertnet4 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/ValidatedData/FuTRES_Mammals_VertNet_Global_Modern_4.csv", header = TRUE)
vertnet5 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/ValidatedData/FuTRES_Mammals_VertNet_Global_Modern_5.csv", header = TRUE)
vertnet6 <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/ValidatedData/FuTRES_Mammals_VertNet_Global_Modern_6.csv", header = TRUE)

vertnet <- rbind(vertnet1, vertnet2, vertnet3, vertnet4, vertnet5, vertnet6)

#check that they all have same number of columns
length(geome)
length(vertnet)

cols.add <- setdiff(colnames(geome), colnames(vertnet))
setdiff(colnames(vertnet), colnames(geome))

geome$occurrenceRemarks <- ""

vertnet[ , cols.add] <- ""
vertnet$projectId <- "vertnet"

nrow(geome)
length(unique(geome$scientificName))

nrow(vertnet)
length(unique(vertnet$scientificName))

col.order <- c("projectId", "expeditionCode", "individualID", "materialSampleID", "diagnosticID",  
               "Event_bcid", "Sample_bcid", "Diagnostics_bcid", "verbatimEventDate",
               "institutionCode", "collectionCode", "catalogNumber", "otherCatalogNumbers",
               "basisOfRecord", "materialSampleCondition",
               "scientificName", "previousIdentifications",
               "lifeStage", "sex", "reproductiveCondition",
               "locality", "country", "stateProvince", 
               "decimalLatitude", "decimalLongitude", "verbatimElevation",
               "minimumChronometricAge", "maximumChronometricAge",
               "measurementSide", "measurementType", "measurementValue", "measurementUnit", "measurementMethod")

##comnbine datasets
geome.sort <- geome %>%
  select(col.order)
vertnet.sort <- vertnet %>%
  select(col.order)

df <- rbind(geome.sort, vertnet.sort)

##clean scientificCNames
df$scientificName <- word(df$scientificName, 1,2, sep = " ") 
df.binom<- df[!grepl('sp.', df$scientificName),]
length(unique(df.binom$scientificName)) 

# in mamm: 2766 spp; and now 1738 because got rid of trinomials
df.binom <- df.binom[!is.na(df.binom$scientificName),]
df.binom <- df.binom[df.binom$scientificName != "(new SW",]
length(unique(df.binom$scientificName))

unique(df.binom$lifeStage)
df.binom$lifeStage[df.binom$lifeStage != "adult" & df.binom$lifeStage != "juvenile"] <- "NS"
df.binom$lifeStage[is.na(df.binom$lifeStage)] <- "NS"

df.binom$measurementValue <- as.numeric(df.binom$measurementValue)
df.binom$measurementValue[df.binom$measurementValue == 0] <- NA

#change known taxonomy error: Spermophilus beecheyi
df.binom$scientificName[df.binom$scientificName == "Spermophilus beecheyi"] <- "Otospermophilus beecheyi"

##write df file----
write.csv(df.binom, "df.before.flagging.csv")

##Figure 1 panel 1: lifeStage----
df.fig1 <- df.binom
length(df.fig1$measurementValue[df.fig1$scientificName == "Peromyscus maniculatus" & 
                                    df.fig1$measurementType == "body mass" & 
                                    !is.na(df.fig1$measurementValue)])
length(df.fig1$measurementValue[df.fig1$scientificName == "Otospermophilus beecheyi" & 
                                    df.fig1$measurementType == "body mass" & 
                                    !is.na(df.fig1$measurementValue)]) #233

#care about estimated and lifeStage
#inferred value = already converted units to "mm" or "g"
#estimated value = made assumptions about which part of the string was the trait value
#I will called estimated value "inferred value" for the category/figure

df.fig1$cat <- paste(df.fig1$lifeStage, df.fig1$measurementMethod)
unique(df.fig1$cat)
df.fig1$cat[df.fig1$cat == "NS Unknown" |
                df.fig1$cat == "NS Extracted with Traiter" | 
                df.fig1$cat == "NS Extracted with Traiter ; inferred value"] <- "No stage; value possibly good" #lightgoldenrod3
df.fig1$cat[df.fig1$cat == "adult Unknown" |
            df.fig1$cat == "adult Extracted with Traiter" | 
            df.fig1$cat == "adult Extracted with Traiter ; inferred value"] <- "Adult; value possibly good" #darkorchid4
df.fig1$cat[df.fig1$cat == "juvenile Unknown" |
            df.fig1$cat == "juvenile Extracted with Traiter" | 
            df.fig1$cat == "juvenile Extracted with Traiter ; inferred value"] <- "Juvenile; value possibly good" #gray74

df.fig1$cat[df.fig1$cat == "NS Extracted with Traiter ; estimated value" |
            df.fig1$cat == "NS Extracted with Traiter ; estimated value; inferred value"] <- "No stage; value inferred" #lightgoldenrod1
df.fig1$cat[df.fig1$cat == "adult Extracted with Traiter ; estimated value" |
            df.fig1$cat == "adult Extracted with Traiter ; estimated value; inferred value"] <- "Adult; value inferred" #darkorchid
df.fig1$cat[df.fig1$cat == "juvenile Extracted with Traiter ; estimated value" |
            df.fig1$cat == "juvenile Extracted with Traiter ; estimated value; inferred value"] <- "Juvenile; value inferred" #gray74

df.fig1$cat <- as.factor(df.fig1$cat)
df.fig1$cat = relevel(df.fig1$cat, "Adult; value possibly good")
df.fig1$cat <- factor(df.fig1$cat, levels = c("Adult; value possibly good", "Adult; value inferred", 
                                              "Juvenile; value possibly good", "Juvenile; value inferred",
                                              "No stage; value possibly good", "No stage; value inferred"))

df.pema <- subset(df.fig1, df.fig1$scientificName == "Peromyscus maniculatus" & 
                  df.fig1$measurementType == "body mass" & 
                  !is.na(df.fig1$measurementValue))
length(df.pema$measurementValue)
unique(df.pema$cat)
length(df.pema$cat[df.pema$cat == "Adult; value possibly good"])
length(df.pema$cat[df.pema$cat == "Juvenile; value possibly good"])
length(df.pema$cat[df.pema$cat == "No stage; value possibly good"])
length(df.pema$cat[df.pema$cat == "No stage; value inferred"])
p <- ggplot(df = df.pema) + 
     geom_density(aes(x = df.pema$measurementValue, fill = df.pema$cat), alpha = 0.6) +
     scale_fill_manual(values = c("darkorchid4", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                       name="df Quality Category") +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black")) +
     ggtitle("Peromyscus maniculatus N = 31659") +
     scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
     scale_y_continuous(name = "Density", limits = c(0, .25))
ggsave(p, file=paste0("orig.dist.lifeStage.mouse",".png"), width = 14, height = 10, units = "cm")

df.otbe <- subset(df.fig1, df.fig1$scientificName == "Otospermophilus beecheyi" & 
                  df.fig1$measurementType == "body mass" & 
                  !is.na(df.fig1$measurementValue))
length(df.otbe$measurementValue)
length(df.otbe$measurementValue[df.otbe$lifeStage == "adult"]) 
length(df.otbe$measurementValue[df.otbe$lifeStage == "NS"]) 
length(df.otbe$measurementValue[df.otbe$lifeStage == "juvenile"])
unique(df.otbe$cat)
length(df.otbe$cat[df.otbe$cat == "Adult; value possibly good"]) 
length(df.otbe$cat[df.otbe$cat == "Juvenile; value possibly good"]) 
length(df.otbe$cat[df.otbe$cat == "No stage; value possibly good"]) 
length(df.otbe$cat[df.otbe$cat == "No stage; value inferred"])
p <- ggplot(df = df.otbe) + 
     geom_density(aes(x = df.otbe$measurementValue, fill = df.otbe$cat), alpha = 0.4) +
     scale_fill_manual(values = c("darkorchid4", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                       name="df Quality Category") +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black")) +
     ggtitle("Otospermophilus beecheyi N = 233") +
     scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
     scale_y_continuous(name = "Density", limits = c(0, .01))
ggsave(p, file=paste0("orig.dist.lifeStage.squirrel",".png"), width = 14, height = 10, units = "cm")

##Mahalanobis Outlier test----
df.test <- df.binom

#add rownames for indexing
rownames(df.test) <- seq(1, nrow(df.test),1)

#need to select out trait
#remove NAs from trait
#apply maha() function
#select out indices
#label those rows as outliers

#want to find outliers for known adults and non-estimated values
sp <- unique(df.test$scientificName)
length(sp) 
nrow(df.test) 

df.test$measurementStatus <- ""

##maha function from https://cran.r-project.org/src/contrib/Archive/OutlierDetection/
source("Maha.R")

##test with P. maniculatus
pema <- subset(df.test, subset = df.test[,"scientificName"] == "Peromyscus maniculatus" &
                 df.test[,"measurementType"] == "body mass" & 
                 df.test[,"lifeStage"] == "adult", 
               select = "measurementValue") %>%
  drop_na()

outlier.pema <- maha(pema, cutoff = 0.95, rnames = FALSE)

##now for all:

for(i in 1:length(sp)){
  sub <- subset(df.test, subset = df.test[,"scientificName"] == sp[i] &
                df.test[,"measurementType"] == "body mass" & 
                df.test[,"lifeStage"] == "adult", 
                select = "measurementValue") %>%
    drop_na()
  if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      df.test[index,"measurementStatus"] <- "outlier"
    }
  }
  else if(isTRUE(nrow(sub) <= 10)){
      df.test$measurementStatus[df.test$scientificName == sp[i]] <- "too few records"
    }
  else{
    next
  }
}

for(i in 1:length(sp)){
  sub <- subset(df.test, subset = df.test[,"scientificName"] == sp[i] & 
                  df.test[,"measurementType"] == "body length" & 
                  df.test[,"lifeStage"] == "adult", 
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
      df.test[index,"measurementStatus"] <- "outlier"
    }
  }
  else if(isTRUE(nrow(sub) <= 10)){
    df.test$measurementStatus[df.test$scientificName == sp[i]] <- "too few records"
  }
  else{
    next
  }
}

for(i in 1:length(sp)){
  sub <- subset(df.test, subset = df.test[,"scientificName"] == sp[i] & 
                  df.test[,"measurementType"] == "tail.length" & 
                  df.test[,"lifeStage"] == "adult", 
                select = "measurementValue") %>%
    drop_na()
  if(isTRUE(nrow(sub) == 0)){
    next
  }
  else if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      df.test[index,"measurementStatus"] <- "outlier"
    }
  }
  else if(isTRUE(nrow(sub) <= 10)){
    df.test$measurementStatus[df.test$scientificName == sp[i]] <- "too few records"
  }
  else{
    next
  }
}

##write out Mahalanobis outlier test data----
data.mh <- data.test
length(unique(data.mh$scientificName)) #3061
nrow(data.mh) #2078861
write.csv(data.mh, "mh.outlier.flagged.data.csv")

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

##test for normality----

data.mh$normality <- ""

data.noInfer_Adults <- subset(data.mh, subset = c(data.mh$measurementStatus != "outlier" &
                                                  data.mh$measurementStatus != "too few records" &
                                                  data.mh$lifeStage == "Adult" &
                                                  data.mh$measurementValueEstimated != "True"))

nrow(data.noInfer_Adults) #263674
length(unique(data.noInfer_Adults$scientificName)) #271

##test using Peromyscus maniculatus
test.pema <- subset(data.mh, subset = data.mh$scientificName == "Peromyscus maniculatus" &
                    data.mh$measurementStatus != "outlier" &
                    data.mh$measurementStatus != "too few records" &
                    data.mh$lifeStage == "Adult" &
                    data.mh$measurementValueEstimated != "True")
normal.pema <- shapiro.test(test.pema$measurementValue[test.pema$measurementType == "mass"]) #if sig then not normally distributed

#extract sig values
normal.pema[[2]]
#isTRUE(normal[[2]] < 0.05)

##TO DO: make into a function

sp <- unique(data.mh$scientificName)

##mass
for(i in 1:length(sp)){
  sub <- subset(data.mh, subset = c(data.mh[,"scientificName"] == sp[i] &
                                    data.mh$measurementStatus != "outlier" &
                                    data.mh$measurementStatus != "too few records" &
                                    data.mh$lifeStage == "Adult" &
                                    data.mh$measurementValueEstimated != "True"))
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "mass"])) < 3)){
    data.mh$normality[data.mh$scientificName == sp[i] & 
                      data.mh$measurementType == "mass" &
                      data.mh$measurementStatus != "outlier" &
                      data.mh$measurementStatus != "too few records" &
                      data.mh$lifeStage == "Adult" &
                      data.mh$measurementValueEstimated != "True"] <- "too few records"
  }
  else if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "mass"])) >= 3)){
    normal.mass <- shapiro.test(sub$measurementValue[sub$measurementType == "mass"])
       if(isTRUE(normal.mass[[2]] < 0.5)){
        data.mh$normality[data.mh$measurementType == "mass" & 
                          data.mhscientificName == sp[i] &
                          data.mh$measurementStatus != "outlier" &
                          data.mh$measurementStatus != "too few records" &
                          data.mh$lifeStage == "Adult" &
                          data.mh$measurementValueEstimated != "True"] <- "non-normal"
      }
      else if(isTRUE(normal.mass[[2]] >= 0.5)){
        data.mh$normality[data.mh$measurementType == "mass" & 
                          data.mh$scientificName == sp[i] &
                          data.mh$measurementStatus != "outlier" &
                          data.mh$measurementStatus != "too few records" &
                          data.mh$lifeStage == "Adult" &
                          data.mh$measurementValueEstimated != "True"] <- "normal"
      }
  }
  else{
    next
  }
}

##total length
for(i in 1:length(sp)){
  sub <- subset(data.mh, subset = c(data.mh[,"scientificName"] == sp[i] &
                                     data.mh$measurementStatus != "outlier" &
                                     data.mh$measurementStatus != "too few records" &
                                     data.mh$lifeStage == "Adult" &
                                     data.mh$measurementValueEstimated != "True"))
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "total.length"])) < 3)){
    data.mh$normality[data.mh$scientificName == sp[i] & 
                      data.mh$measurementType == "total.length" &
                      data.mh$measurementStatus != "outlier" &
                      data.mh$measurementStatus != "too few records" &
                      data.mh$lifeStage == "Adult" &
                      data.mh$measurementValueEstimated != "True"] <- "too few records"
  }
  else if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "total.length"])) >= 3)){
    normal.total.length <- shapiro.test(sub$measurementValue[sub$measurementType == "total.length"])
    if(isTRUE(normal.total.length[[2]] < 0.5)){
      data.mh$normality[data.mh$measurementType == "total.length" & 
                        data.mh$scientificName == sp[i] &
                        data.mh$measurementStatus != "outlier" &
                        data.mh$measurementStatus != "too few records" &
                        data.mh$lifeStage == "Adult" &
                        data.mh$measurementValueEstimated != "True"] <- "non-normal"
    }
    else if(isTRUE(normal.total.length[[2]] >= 0.5)){
      data.mh$normality[data.mh$measurementType == "total.length" & 
                        data.mh$scientificName == sp[i] &
                        data.mh$measurementStatus != "outlier" &
                        data.mh$measurementStatus != "too few records" &
                        data.mh$lifeStage == "Adult" &
                        data.mh$measurementValueEstimated != "True"] <- "normal"
    }
  }
  else{
    next
  }
}

##tail length
for(i in 1:length(sp)){
  sub <- subset(data.mh, subset = c(data.mh[,"scientificName"] == sp[i] &
                                      data.mh$measurementStatus != "outlier" &
                                      data.mh$measurementStatus != "too few records" &
                                      data.mh$lifeStage == "Adult" &
                                      data.mh$measurementValueEstimated != "True"))
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "tail.length"])) < 3)){
    data.mh$normality[data.mh$scientificName == sp[i] & 
                      data.mh$measurementType == "tail.length" &
                      data.mh$measurementStatus != "outlier" &
                      data.mh$measurementStatus != "too few records" &
                      data.mh$lifeStage == "Adult" &
                      data.mh$measurementValueEstimated != "True"] <- "too few records"
  }
  else if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "tail.length"])) >= 3)){
    normal.tail <- shapiro.test(sub$measurementValue[sub$measurementType == "tail.length"])
    if(isTRUE(normal.tail[[2]] < 0.5)){
      data.mh$normality[data.mh$measurementType == "tail.length" &
                        data.mh$scientificName == sp[i] &
                        data.mh$measurementStatus != "outlier" &
                        data.mh$measurementStatus != "too few records" &
                        data.mh$lifeStage == "Adult" &
                        data.mh$measurementValueEstimated != "True"] <- "non-normal"
    }
    else if(isTRUE(normal.tail[[2]] >= 0.5)){
      data.mh$normality[data.mh$measurementType == "tail.length" & 
                        data.mh$scientificName == sp[i]&
                        data.mh$measurementStatus != "outlier" &
                        data.mh$measurementStatus != "too few records" &
                        data.mh$lifeStage == "Adult" &
                        data.mh$measurementValueEstimated != "True"] <- "normal"
    }
  }
  else{
    next
  }
}

nrow(data.norm.test) #should be nrow(data)

##write normality test----
data.norm <- data.mh
write.csv(df.norm, "normality.test.flagged.data.csv")

####log transform data----

data.transform <- data.norm
data.transform$logMeasurementValue <- log10(data.transform$measurementValue)
data.transform[!is.finite(data.transform$logMeasurementValue),] <- NA

df.transform <- subset(df.norm, subset = c(df.norm$measurementStatus != "outlier" &
                                           df.norm$measurementStatus != "too few records" &
                                           df.norm$lifeStage == "Adult" &
                                           df.norm$measurementValueEstimated != "True" &
                                           df.norm$normality == "non-normal"))

#testing
df.pema1 <- data.transform[data.transform$scientificName == "Peromyscus maniculatus", 
                           data.transform$measurementStatus != "outlier" &
                           data.transform$measurementStatus != "too few records" &
                           data.transform$lifeStage == "Adult" &
                           data.transform$measurementValueEstimated != "True" &
                           data.transform$normality == "non-normal"]

sd.pema1 <- sd(df.pema1$measurementValue[df.pema1$measurementType == "mass"], na.rm = TRUE)
mean.pema1 <- mean(df.pema1$measurementValue[df.pema1$measurementType == "mass"], na.rm = TRUE)
upper.limit1 <- mean.pema+3*sd.pema
lower.limit1 <- mean.pema-3*sd.pema

plot(density(df.pema1$measurementValue[df.pema1$measurementType == "mass" &
                                       df.pema1$measurementStatue != "outlier"], bw = .5,na.rm = TRUE))

hist(df.pema1$measurementValue[df.pema1$measurementType == "mass" &
                               df.pema1$measurementStatus != "outlier"], breaks = 100,
     xlim = c(0, 50),
     main = substitute(paste("Histogram of", italic("Peromyscus maniculatus"))),
     xlab = "Body Mass (g)")
     abline(v = upper.limit, col = "black", lty = 2)
     abline(v = lower.limit, col = "black", lty = 2)
  
df.pema2 <- data.transform[data.transform$scientificName == "Peromyscus maniculatus",
                           data.transform$measurementStatus != "outlier" &
                           data.transform$measurementStatus != "too few records" &
                           data.transform$lifeStage == "Adult" &
                           data.transform$measurementValueEstimated != "True" &
                           data.transform$normality == "non-normal"] %>%
  drop_na()

outlier2 <- maha(df.pema2$measurementValue[df.pema2$measurementType == "mass"], cutoff = 0.95, rnames = FALSE)
index <= names(outlier2[[2]])

df.pema2[index,"measurementStatus"] <- "outlier"

sd.pema2 <- sd(df.pema2$measurementValue[df.pema2$measurementType == "mass" &
                                         df.pema2$measurementStatus != "outlier"], na.rm = TRUE)
mean.pema2 <- mean(df.pema2$measurementValue[df.pema2$measurementType == "mass" &
                                             df.pema2$measurementStatus != "outlier"], na.rm = TRUE)
upper.limit2 <- mean.pema+3*sd.pema
lower.limit2 <- mean.pema-3*sd.pema

plot(density(df.test2$measurementValue[df.test2$measurementType == "mass"], bw = .5,na.rm = TRUE))

hist(df.test2$measurementValue[df.test2$measurementType == "mass"], breaks = 100,
     xlim = c(0, 50),
     main = substitute(paste("Histogram of", italic("Peromyscus maniculatus"))),
     xlab = "Body Mass (g)")
abline(v = upper.limit, col = "black", lty = 2)
abline(v = lower.limit, col = "black", lty = 2)

#all species

sp.transform <- unique(df.transform$scientificName)

for(i in 1:length(sp.transform)){
  sub <- subset(df.transform, subset = c(df.transform$scientificName == sp.transform[i] &
                                         df.transform$measurementStatus != "outlier" &
                                         df.transform$measurementValueEstimated != "True" &
                                         df.transform$lifeStage == "Adult" &
                                         df.transform$measurementStatus != "too few records" &
                                         df.transform$normality == "non-normal"))
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "mass"])) < 3)){
    df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                           df.transform$measurementType == "mass" & 
                           df.transform$measurementStatus != "outlier" &
                           df.transform$measurementValueEstimated != "True" &
                           df.transform$lifeStage == "Adult" &
                           df.transform$measurementStatus != "too few records" &
                           df.transform$normality == "non-normal"] <- "too few records"
  }
  else if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "mass"])) >= 3)){
    log.normal.mass <- shapiro.test(sub$logMeasurementValue[sub$measurementType == "mass"])
    if(isTRUE(log.normal.mass[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "mass" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementValueEstimated != "True" &
                             df.transform$lifeStage == "Adult" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "mass" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementValueEstimated != "True" &
                             df.transform$lifeStage == "Adult" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$normality == "non-normal"] <- "non-normal (even logged)"
    }
  }
  else{
    next
  }
}

####STOPPED HERE----
for(i in 1:length(sp.transform)){
  sub <- subset(df.transform, subset = c(df.transform$scientificName == sp.transform[i]))
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "total.length"])) < 3)){
    df.transform$normality[df.transform$scientificName == sp.transform[i] & df.transform$measurementType == "total.length"] <- "too few records"
  }
  else if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "total.length"])) >= 3)){
    log.normal.total.length <- shapiro.test(sub$logMeasurementValue[sub$measurementType == "total.length"])
    if(isTRUE(log.normal.total.length[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & df.transform$measurementType == "total.length"] <- "log normal"
    }
    else if(isTRUE(log.normal.total.length[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & df.transform$measurementType == "total.length"] <- "non-normal (even logged)"
    }
  }
  else{
    next
  }
}

for(i in 1:length(sp.transform)){
  sub <- subset(df.transform, subset = c(df.transform$scientificName == sp.transform[i]))
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "tail.length"])) < 3)){
    df.transform$normality[df.transform$scientificName == sp.transform[i] & df.transform$measurementType == "tail.length"] <- "too few records"
  }
  else if(isTRUE(length(unique(sub$measurementValue[sub$measurementType == "tail.length"])) >= 3)){
    log.normal.tail.length <- shapiro.test(sub$logMeasurementValue[sub$measurementType == "tail.length"])
    if(isTRUE(log.normal.tail.length[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & df.transform$measurementType == "tail.length"] <- "log normal"
    }
    else if(isTRUE(log.normal.tail.length[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & df.transform$measurementType == "tail.length"] <- "non-normal (even logged)"
    }
  }
  else{
    next
  }
}

df.logged <- rbind(df.transform, df.theRest)
write.csv(df.logged, "data.logged.normal.csv")

####quantile----

df.noInfer.adults <- subset(df.logged, subset = c(df.logged$measurementStatus != "outlier" &
                                                  df.logged$measurementStatus != "too few records" &
                                                  df.logged$lifeStage == "Adult" &
                                                  df.logged$measurementValueEstimated != "True"))
  
df.pema.quant <- subset(df.noInfer.adults, subset = df.noInfer.adults$scientificName == "Peromyscus maniculatus")
quant.pema.mass <- quantile(df.pema.quant$measurementValue[df.pema.quant$measurementType == "mass" & !is.na(df.pema.quant$measurementValue)], probs = seq(0,1,.05))
quant.pema.mass[[2]] #5%
quant.pema.mass[[20]] #95%

df.noInfer.adults_quants <- df.noInfer.adults %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(sample.size.mass = length(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0]),
                   upper.quantile.mass = quantile(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0], probs = seq(0,1,.05))[[20]],
                   lower.quantile.mass = quantile(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0], probs = seq(0,1,.05))[[2]],
                   
                   sample.size.tail = length(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0]),
                   upper.quantile.tail = quantile(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0], probs = seq(0,1,.05))[[20]],
                   lower.quantile.tail = quantile(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0], probs = seq(0,1,.05))[[2]],
                   
                   sample.size.length = length(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0]),
                   upper.quantile.length = quantile(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0], probs = seq(0,1,.05))[[20]],
                   lower.quantile.length = quantile(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0], probs = seq(0,1,.05))[[2]]) %>%
  as.data.frame()

data.quants <- merge(df.logged, df.noInfer.adults_quants, by = "scientificName", all.x = TRUE, all.y = FALSE)

##label outliers----
#label samples that are outside of limits with outlier, and label those within limits as "g" and inferred = TRUE
data.quants$index <- rownames(data.quants)
data.quants$measurementStatus[data.quants$sample.size.length < 10] <- "too few records"
data.quants$measurementStatus[data.quants$sample.size.mass < 10] <- "too few records"
data.quants$measurementStatus[data.quants$sample.size.tail < 10] <- "too few records"
data.quants$measurementStatus[is.na(data.quants$measurementValue)] <- "ignore"
data.quants$measurementValue[data.quants$measurementValue < 0 | is.infinite(data.quants$measurementValue)] <- ""
data.check.quants <- data.quants[data.quants$measurementStatus != "too few records" & 
                           data.quants$measurementStatus != "outlier" &
                           data.quants$measurementStatus != "ignore",]
data.uncheck.quants <- data.quants[data.quants$measurementStatus == "too few records" | 
                             data.quants$measurementStatus == "outlier" |
                             data.quants$measurementStatus == "ignore",]

sp.quants <- unique(data.check.quants$scientificName)
length(sp.quants) #240
nrow(data.check.quants) #1681159

#need to split up datasets to test
#mass
for(i in 1:length(sp.quants)){
  sub <- data.check.quants[data.check.quants$scientificName == sp.quants[i] & 
                      data.check.quants$measurementType == "mass",]
  for(j in 1:nrow(sub)){
    if(isTRUE(sub$measurementValue[j] <= sub$lower.limit.mass[1])){
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly not adult"
    }
    else if(isTRUE(sub$measurementValue[j] >= sub$upper.limit.mass[1])){
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly not adult"
    }
    else{
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly adult"
    }
  }
} 

#total length
for(i in 1:length(sp.quants)){
  sub <- data.check.quants[data.check.quants$scientificName == sp.quants[i] & 
                      data.check.quants$measurementType == "total.length",]
  for(j in 1:nrow(sub)){
    if(isTRUE(sub$measurementValue[j] <= sub$lower.limit.length[1])){
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly not adult"
    }
    else if(isTRUE(sub$measurementValue[j] >= sub$upper.limit.length[1])){
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly not adult"
    }
    else{
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly adult"
    }
  }
} 

#tail length
for(i in 1:length(sp.quants)){
  sub <- data.check.quants[data.check.quants$scientificName == sp.quants[i] & 
                      data.check.quants$measurementType == "tail.length",]
  for(j in 1:nrow(sub)){
    if(isTRUE(sub$measurementValue[j] <= sub$lower.limit.tail[1])){
      data.check.quants$measurementStatus[data.check.quants$inde == sub$index[j]] <- "possibly not adult"
    }
    else if(isTRUE(sub$measurementValue[j] >= sub$upper.limit.tail[1])){
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly not adult"
    }
    else{
      data.check.quants$measurementStatus[data.check.quants$index == sub$index[j]] <- "possibly adult"
    }
  }
} 

data.total.quants <- rbind(data.check.quants, data.uncheck.quants)

####3 sigma----
data.noInfer.adults <- subset(data.total.quants, subset = c(data.total.quants$measurementStatus != "outlier" &
                                                    data.total.quants$measurementStatus != "too few records" &
                                                    data.total.quants$lifeStage == "Adult" &
                                                    data.total.quants$measurementValueEstimated != "True"))


data.noInfer_sd <- data.noInfer.adults %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(#sample.size.mass = length(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0]),
                   avg.mass = mean(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   sigma.mass = sd(measurementValue[measurementType == "mass" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   upper.limit.mass = avg.mass + (3*sigma.mass),
                   lower.limit.mass = avg.mass - (3*sigma.mass),
                   
                   #sample.size.tail = length(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0]),
                   avg.tail = mean(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   sigma.tail = sd(measurementValue[measurementType == "tail.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   upper.limit.tail = avg.tail + (3*sigma.tail),
                   lower.limit.tail = avg.tail - (3*sigma.tail),
                   
                   #sample.size.length = length(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0]),
                   avg.length = mean(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   sigma.length = sd(measurementValue[measurementType == "total.length" & !is.na(measurementValue) & measurementValue > 0], na.rm = TRUE),
                   upper.limit.length = avg.length + (3*sigma.length),
                   lower.limit.length = avg.length - (3*sigma.length)) %>%
  as.data.frame()
nrow(data.noInfer_sd) #271
length(unique(data.noInfer_sd$scientificName)) #271

##add stats to dataframe
data.limit <- merge(data.total.quants, data.noInfer_sd, by = "scientificName", all.x = TRUE, all.y = FALSE)
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

sp.limits <- unique(data.check$scientificName)
length(sp.limits) #240
nrow(data.check) #1681159

#need to split up datasets to test
#mass
for(i in 1:length(sp.limits)){
  sub <- data.check[data.check$scientificName == sp.limits[i] & 
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
for(i in 1:length(sp.limits)){
  sub <- data.check[data.check$scientificName == sp.limits[i] & 
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
for(i in 1:length(sp.limits)){
  sub <- data.check[data.check$scientificName == sp.limits[i] & 
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




