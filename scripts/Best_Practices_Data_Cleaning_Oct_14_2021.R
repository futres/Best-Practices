# Best Practices Data Clean Up
# Meghan A. Balk
# balk@battelleecology.org

#### LOAD PACKAGES---- 
require(tidyverse)
require(nlme)
require(dplyr)
require(tidyr)
require(ggplot2)
require(reshape2)
require(plyr)
require(stringr)
#require(OutlierDetection)
#call maha function from https://cran.r-project.org/src/contrib/Archive/OutlierDetection/
require(utils)

#rfutres package
library(devtools)
devtools::install_github("futres/rfutres")
library(rfutres)

#### LOAD DATA ----

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

#futres download from FuTRES datastore

r_all_futres <- futres_query_all()

#Villaseñor = Project 277
#Blois 2018 = Project 278
#EAP K.Emery = Project 282
#ODFW = Project 294
#Bernor = Project 314
#Hopkins = Project 410
#Yrarrazaval = Project 406
#Vertnet, extracted using traiter (2217422 records of mammals not including bats; 211327 records of just bats; 2428749 total records)

## write out compiled list
write.csv(r_all_futres, "futres_datastore.csv")

## write out data
futres <- r_all_futres$data
nrow(futres)
#2417473 rows
nrow(futres[futres$projectID == "Vertnet",])
#2401543 are VertNet 
write.csv(futres, "futres.data.csv")

df <- futres

##get rid of projects not included in paper
df <- df %>%
  dplyr::filter(projectID != "https://geome-db.org/workbench/project-overview?projectId=410",
                projectID != "https://geome-db.org/workbench/project-overview?projectId=406")

##clean up lifeStage
unique(df$lifeStage)
df$lifeStage[df$lifeStage == "" |
             df$lifeStage == "lifeStage"] <- "Not Collected"

##clean up measurementValue
df$measurementValue <- as.numeric(df$measurementValue)
df$measurementValue[df$measurementValue == 0] <- NA

df <- df[!grepl('sp.', df$scientificName),]
df <- df[!grepl('aff.', df$scientificName),]

##clean up scientificName
#turn trinomials into binomials
df$scientificName <- word(df$scientificName, 1,2, sep = " ") 

##extract only binomials
#code from https://stackoverflow.com/questions/8920145/count-the-number-of-all-words-in-a-string
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}
df$nwords <- nwords(df$scientificName)
df <- df %>%
  filter(nwords == 2) %>%
  dplyr::select(-nwords)
length(unique(df$scientificName)) 

#change known taxonomy error: Spermophilus beecheyi
df$scientificName[df$scientificName == "Spermophilus beechyi"] <- "Spermophilus beecheyi"
df$scientificName[df$scientificName == "Spermophilus beecheyi"] <- "Otospermophilus beecheyi"

length(unique(df$scientificName)) 

## write out data
write.csv(df, "futres.trim1.csv")

#### Figure 1 panel 1: lifeStage ----
df.fig1 <- df
length(df.fig1$measurementValue[df.fig1$scientificName == "Peromyscus maniculatus" & 
                                df.fig1$measurementType == "body mass" &
                                !is.na(df.fig1$measurementValue)]) 
length(df.fig1$measurementValue[df.fig1$scientificName == "Otospermophilus beecheyi" & 
                                    df.fig1$measurementType == "body mass"]) 

#care about estimated and lifeStage
#inferred value = already converted units to "mm" or "g"
#estimated value = made assumptions about which part of the string was the trait value
#I will called estimated value "inferred value" for the category/figure

df.fig1$cat <- paste(df.fig1$lifeStage, df.fig1$measurementMethod)
unique(df.fig1$cat)
df.fig1$cat[df.fig1$cat == "Not Collected Unknown" |
            df.fig1$cat == " Unknown" |
            df.fig1$cat == "lifeStage measurementMethod" |
            df.fig1$cat == "Not Collected Extracted with Traiter" | 
            df.fig1$cat == "Not Collected Extracted with Traiter ; inferred value" |
            df.fig1$cat == "Not Collected Hopkins, S. S. (2008). Reassessing the Mass of Exceptionally Large Rodents Using Toothrow Length and Area as Proxies for Body Mass. Journal of Mammalogy, 89(1), 232–243. https://doi.org/10.1644/06-mamm-a-306.1"] <- "No stage; value possibly good" #lightgoldenrod3
df.fig1$cat[df.fig1$cat == "adult Unknown" |
            df.fig1$cat == "adult Von den Driesch (1976)" |
            df.fig1$cat == "adult Extracted with Traiter" | 
            df.fig1$cat == "adult Extracted with Traiter ; inferred value"] <- "Adult; value possibly good" #darkorchid4
df.fig1$cat[df.fig1$cat == "juvenile Unknown" |
            df.fig1$cat == "juvenile Extracted with Traiter" | 
            df.fig1$cat == "juvenile Extracted with Traiter ; inferred value"] <- "Juvenile; value possibly good" #gray74
df.fig1$cat[df.fig1$cat == "Not Collected Extracted with Traiter ; estimated value" |
            df.fig1$cat == "Not Collected Extracted with Traiter ; estimated value; inferred value"] <- "No stage; value inferred" #lightgoldenrod1
df.fig1$cat[df.fig1$cat == "adult Extracted with Traiter ; estimated value" |
            df.fig1$cat == "adult Extracted with Traiter ; estimated value; inferred value"] <- "Adult; value inferred" #darkorchid
df.fig1$cat[df.fig1$cat == "juvenile Extracted with Traiter ; estimated value" |
            df.fig1$cat == "juvenile Extracted with Traiter ; estimated value; inferred value"] <- "Juvenile; value inferred" #gray74

df.fig1$cat <- as.factor(df.fig1$cat)
df.fig1$cat = relevel(df.fig1$cat, "Adult; value possibly good")
df.fig1$cat <- factor(df.fig1$cat, levels = c("Adult; value possibly good", "Adult; value inferred", 
                                              "Juvenile; value possibly good", "Juvenile; value inferred",
                                              "No stage; value possibly good", "No stage; value inferred"))

df.pema1 <- subset(df.fig1, df.fig1$scientificName == "Peromyscus maniculatus" & 
                   df.fig1$measurementType == "body mass" & 
                   !is.na(df.fig1$measurementValue))
length(df.pema1$measurementValue)
unique(df.pema1$cat)
length(df.pema1$cat[df.pema1$cat == "Adult; value possibly good"]) #3021
length(df.pema1$cat[df.pema1$cat == "Juvenile; value possibly good"]) #951
length(df.pema1$cat[df.pema1$cat == "No stage; value possibly good"]) #27583
length(df.pema1$cat[df.pema1$cat == "No stage; value inferred"]) #104
p.pema1 <- ggplot() + 
                  geom_density(aes(x = df.pema1$measurementValue, fill = df.pema1$cat), alpha = 0.6) +
                  scale_fill_manual(values = c("darkorchid4", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                                    name="Data Quality Category") +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                  ggtitle("Peromyscus maniculatus N = 31659") +
                  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
                  scale_y_continuous(name = "Density", limits = c(0, .25))
ggsave(p.pema1, file = paste0("orig.dist.lifeStage.mouse",".png"), 
       width = 14, height = 10, units = "cm")

df.otbe1 <- subset(df.fig1, df.fig1$scientificName == "Otospermophilus beecheyi" & 
                   df.fig1$measurementType == "body mass" & 
                   !is.na(df.fig1$measurementValue))
length(df.otbe1$measurementValue) #233
length(df.otbe1$measurementValue[df.otbe1$lifeStage == "adult"]) #28
length(df.otbe1$measurementValue[df.otbe1$lifeStage == "Not Collected"]) #194
length(df.otbe1$measurementValue[df.otbe1$lifeStage == "juvenile"]) #11
unique(df.otbe1$cat)
length(df.otbe1$cat[df.otbe1$cat == "Adult; value possibly good"]) #28
length(df.otbe1$cat[df.otbe1$cat == "Juvenile; value possibly good"]) #11
length(df.otbe1$cat[df.otbe1$cat == "No stage; value possibly good"]) #193
length(df.otbe1$cat[df.otbe1$cat == "No stage; value inferred"]) #1
p.otbe1 <- ggplot() + 
           geom_density(aes(x = df.otbe1$measurementValue, fill = df.otbe1$cat), alpha = 0.4) +
           scale_fill_manual(values = c("darkorchid4", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                             name="Data Quality Category") +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")) +
           ggtitle("Otospermophilus beecheyi N = 385") +
           scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
           scale_y_continuous(name = "Density", limits = c(0, .0125))
ggsave(p.otbe1, file=paste0("orig.dist.lifeStage.squirrel",".png"), 
       width = 14, height = 10, units = "cm")

#### MAHALANOBIS OUTLIER TEST ----
df.test <- df

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
setwd("~/GitHub/futres/Best-Practices-Paper/scripts/OutlierDetection/R")
source("Maha.R")

##test with P. maniculatus
pema <- subset(df.test, subset = df.test[,"scientificName"] == "Peromyscus maniculatus" &
               df.test[,"measurementType"] == "body mass" & 
               df.test[,"lifeStage"] == "adult", 
               select = "measurementValue") %>%
  mutate_at("measurementValue", as.numeric)%>%
  drop_na()
  
outlier.pema <- maha(pema, cutoff = 0.95, rnames = FALSE)

##now for all:

#mass
for(i in 1:length(sp)){
  sub <- subset(df.test, subset = df.test[,"scientificName"] == sp[i] &
                df.test[,"measurementType"] == "body mass" & 
                df.test[,"lifeStage"] == "adult", 
                select = "measurementValue") %>%
         mutate_at("measurementValue", as.numeric) %>%
         drop_na()
  
  if(isTRUE(nrow(sub) == 0)){
    next
  }
  
  else if(isTRUE(length(unique(sub$measurementValue)) == 1)){
    df.test$measurementStatus[df.test[,"scientificName"] == sp[i] & 
                                df.test[,"measurementType"] == "body mass" & 
                                df.test[,"lifeStage"] == "adult"] <- "too few records"
  }
  
  else if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      df.test[index,"measurementStatus"] <- "outlier"
    }
  }
 
  else if(isTRUE(nrow(sub) <= 10)){
      df.test$measurementStatus[df.test[,"scientificName"] == sp[i] & 
                                df.test[,"measurementType"] == "body mass" & 
                                df.test[,"lifeStage"] == "adult"] <- "too few records"
    }
  
  else{
    next
  }
}

unique(df.test$measurementStatus) #get all three options!
unique(df.test$measurementStatus[df.test$measurementType == "body mass"])
unique(df.test$measurementStatus[df.test$measurementType == "tail length"]) #should only be ""

#total length
for(i in 1:length(sp)){
  sub <- subset(df.test, subset = df.test[,"scientificName"] == sp[i] & 
                df.test[,"measurementType"] == "body length" |
                df.test[,"measurementType"] == "body length with tail"  & 
                df.test[,"lifeStage"] == "adult", 
                select = "measurementValue") %>%
         mutate_at("measurementValue", as.numeric) %>%
         drop_na()
  
  if(isTRUE(nrow(sub) == 0)){
    next
  }
  
  else if(isTRUE(length(unique(sub$measurementValue)) == 1)){
    df.test$measurementStatus[df.test[,"scientificName"] == sp[i] & 
                              df.test[,"measurementType"] == "body length" |
                              df.test[,"measurementType"] == "body length with tail" & 
                              df.test[,"lifeStage"] == "adult"] <- "too few records"
  }
  
  else if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      df.test[index,"measurementStatus"] <- "outlier"
    }
  }
  
  else if(isTRUE(nrow(sub) <= 10)){
    df.test$measurementStatus[df.test[,"scientificName"] == sp[i] & 
                              df.test[,"measurementType"] == "body length" |
                              df.test[,"measurementType"] == "body length with tail"  & 
                              df.test[,"lifeStage"] == "adult"] <- "too few records"
  }
  
  else{
    next
  }
}

unique(df.test$measurementStatus[df.test$measurementType == "body length" |
                                 df.test$measurementType == "body length without tail"])

#tail length
for(i in 1:length(sp)){
  sub <- subset(df.test, subset = df.test[,"scientificName"] == sp[i] & 
                df.test[,"measurementType"] == "tail length" & 
                df.test[,"lifeStage"] == "adult", 
                select = "measurementValue") %>%
         mutate_at("measurementValue", as.numeric) %>%
         drop_na()
  
  if(isTRUE(nrow(sub) == 0)){
    next
  }
  
  else if(isTRUE(length(unique(sub$measurementValue)) == 1)){
    df.test$measurementStatus[df.test[,"scientificName"] == sp[i] & 
                              df.test[,"measurementType"] == "tail length" & 
                              df.test[,"lifeStage"] == "adult"] <- "too few records"
  }
  
  else if(isTRUE(nrow(sub) >= 10)){
    outlier <- maha(sub, cutoff = 0.95, rnames = FALSE)
    index <- names(outlier[[2]])
    if(isTRUE(length(index) != 0)){
      df.test[index,"measurementStatus"] <- "outlier"
    }
  }
  
  else if(isTRUE(nrow(sub) <= 10)){
    df.test$measurementStatus[df.test[,"scientificName"] == sp[i] & 
                              df.test[,"measurementType"] == "body length" & 
                              df.test[,"lifeStage"] == "adult"] <- "too few records"
  }
  
  else{
    next
  }
}

unique(df.test$measurementStatus[df.test$measurementType == "tail length"])

##write out Mahalanobis outlier test data
length(unique(df.test$scientificName)) #3741; same as df
nrow(df.test) #2266119; same as df
write.csv(df.test, "mh.outlier.flagged.data.csv")

##Figure 1, panel 2: outliers----
df.fig2 <- df.test[df.test$lifeStage != "juvenile" & 
                   df.test$measurementStatus != "too few records",]

df.fig2$cat <- paste(df.fig2$lifeStage, df.fig2$measurementStatus)
unique(df.fig2$cat)
df.fig2$cat[df.fig2$cat == "adult "] <- "Adult; possibly good" #darkorchid4
df.fig2$cat[df.fig2$cat == "adult outlier"] <- "Adult; outlier" #darkorchid1
df.fig2$cat[df.fig2$cat == "Not Collected "] <- "No stage; untested" #lightgoldenrod1
df.fig2$cat[df.fig2$cat == "Not Collected outlier"] <- "No stage; outlier" #lightgoldenrodyellow

df.fig2$cat <- as.factor(df.fig2$cat)
df.fig2$cat = relevel(df.fig2$cat, "Adult; possibly good")
df.fig2$cat <- factor(df.fig2$cat, levels = c("Adult; possibly good", 
                                              "Adult; outlier", 
                                              "No stage; untested",
                                              "No stage; outlier"))

df.pema2 <- subset(df.fig2, df.fig2$scientificName == "Peromyscus maniculatus" & 
                   df.fig2$measurementType == "body mass" &
                   !is.na(df.fig2$measurementValue))
df.pema2$measurementValue <- as.numeric(df.pema2$measurementValue)
length(df.pema2$measurementType) #30708
unique(df.pema2$cat)
length(df.pema2$cat[df.pema2$cat == "Adult; possibly good"]) #3020
length(df.pema2$cat[df.pema2$cat == "Adult; outlier"]) #1
length(df.pema2$cat[df.pema2$cat == "No stage; untested"]) #27687 
p.pema2 <- ggplot() + 
           geom_density(data = filter(df.pema2, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, alpha = 0.4) + 
           geom_rug(data = filter(df.pema2, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
           geom_density(data = df.pema2, aes(x = measurementValue, fill = cat), alpha = 0.4) +
           scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod1"),
                             name = "Data Quality Category") +
           ggtitle("Peromyscus maniculatus N = 30708, Noutlier = 1") +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")) +
           scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
           scale_y_continuous(name = "Density", limits = c(0, .25))
df.pema2$measurementValue[df.pema2$cat == "Adult; outlier"]
#outlier @17100g
ggsave(p.pema2, file=paste0("outlier.test.mouse",".png"), width = 14, height = 10, units = "cm")

df.otbe2 <- subset(df.fig2, df.fig2$scientificName == "Otospermophilus beecheyi" & 
                   df.fig2$measurementType == "body mass" &
                   !is.na(df.fig2$measurementValue))
length(df.otbe2$measurementType) #222
unique(df.otbe2$cat)
length(df.otbe2$cat[df.otbe2$cat == "Adult; possibly good"]) #27
length(df.otbe2$cat[df.otbe2$cat == "Adult; outlier"]) #1
length(df.otbe2$cat[df.otbe2$cat == "No stage; untested"]) #194 
p.otbe2 <- ggplot() + 
           geom_density(data = filter(df.otbe2, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, alpha = 0.4) + 
           geom_rug(data = filter(df.otbe2, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
           geom_density(data = df.otbe2, aes(x = measurementValue, fill = cat), alpha = 0.4) +
           scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod1"),
                             name = "Data Quality Category") +
           ggtitle("Otospermophilus beecheyi N = 222, Noutlier = 1") +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")) +
           scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
           scale_y_continuous(name = "Density", limits = c(0, .0125))
df.otbe2$measurementValue[df.otbe2$cat == "Adult; outlier"]
#outlier @169.5
ggsave(p.otbe2, file=paste0("outlier.test.squirrel",".png"), width = 14, height = 10, units = "cm")

#### TEST FOR NORMALITY ----
df.norm <- df.test
df.norm$normality <- ""

#only want to test normality on "good" data: known adults, no outliers, and not on estimated values
#want to not test on species with less than 10 records

##test using Peromyscus maniculatus
test.pema <- subset(df.norm, subset = df.norm$scientificName == "Peromyscus maniculatus" &
                    df.norm$measurementStatus != "outlier" &
                    df.norm$measurementStatus != "too few records" &
                    df.norm$lifeStage == "adult" &
                    df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                    df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value") %>%
  drop_na(measurementValue)
normal.pema <- shapiro.test(test.pema$measurementValue[test.pema$measurementType == "body mass"]) #if sig then not normally distributed

#extract sig values
normal.pema[[2]]
#isTRUE(normal[[2]] < 0.05)

##now for all:
sp <- unique(df.norm$scientificName)

#mass
for(i in 1:length(sp)){
  sub <- subset(df.norm, subset = c(df.norm[,"scientificName"] == sp[i] &
                                    df.norm$measurementStatus != "outlier" &
                                    df.norm$measurementStatus != "too few records" &
                                    df.norm$lifeStage == "adult" &
                                    df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                                    df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"))
  sub <- sub %>%
    drop_na(measurementValue)
  
  if(isTRUE(length(sub$measurementValue[sub$measurementType == "body mass"]) < 3 |
            length(unique(sub$measurementValue[sub$measurementType == "body mass"])) < 3)){
    df.norm$measurementStatus[df.norm$scientificName == sp[i] & 
                              df.norm$measurementType == "body mass" &
                              df.norm$measurementStatus != "outlier" &
                              df.norm$measurementStatus != "too few records" &
                              df.norm$lifeStage == "adult" &
                              df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                              df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "too few records"
  }
  
  else if(isTRUE(length(sub$measurementValue[sub$measurementType == "body mass"]) > 5000)){
    x <- sample(sub$measurementValue[sub$measurementType == "body mass"], 
                5000, replace = FALSE, prob = NULL)
    normal.total.length <- shapiro.test(x)
      if(isTRUE(normal.total.length[[2]] < 0.05)){
      df.norm$normality[df.norm$measurementType == "body mass" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "non-normal"
      }
      else if(isTRUE(normal.total.length[[2]] >= 0.05)){
        df.norm$normality[df.norm$measurementType == "body mass" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "normal"
      }
  }
  
  else if(isTRUE(length(sub$measurementValue[sub$measurementType == "body mass"]) <= 5000 & length(unique(sub$measurementValue[sub$measurementType == "body mass"])) >= 3)){
    normal.mass <- shapiro.test(sub$measurementValue[sub$measurementType == "body mass"])
       if(isTRUE(normal.mass[[2]] < 0.05)){
        df.norm$normality[df.norm$measurementType == "body mass" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "non-normal"
      }
      else if(isTRUE(normal.mass[[2]] >= 0.05)){
        df.norm$normality[df.norm$measurementType == "body mass" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "normal"
      }
  }
  
  else{
    next
  }
}

unique(df.norm$normality)
#has all the values!
unique(df.norm$normality[df.norm$measurementType == "body mass"])
unique(df.norm$normality[df.norm$measurementType == "tail length"])

#total length
for(i in 1:length(sp)){
  sub <- subset(df.norm, subset = c(df.norm[,"scientificName"] == sp[i] &
                                      df.norm$measurementStatus != "outlier" &
                                      df.norm$measurementStatus != "too few records" &
                                      df.norm$lifeStage == "adult" &
                                      df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                                      df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"))
  sub <- sub %>%
    drop_na(measurementValue) %>%
    mutate_at("measurementValue", as.numeric)
  
  if(isTRUE(length(sub$measurementValue[sub$measurementType == "body length" |
                                        sub$measurementType == "body length with tail"]) < 3 |
            length(unique(sub$measurementValue[sub$measurementType == "body length"])) < 3)){
    df.norm$measurementStatus[df.norm$scientificName == sp[i] & 
                              df.norm$measurementType == "body length" |
                              df.norm$measurementType == "body length with tail" &
                              df.norm$measurementStatus != "outlier" &
                              df.norm$measurementStatus != "too few records" &
                              df.norm$lifeStage == "adult" &
                              df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                              df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "too few records"
  }
  
  else if(isTRUE(length(sub$measurementValue[sub$measurementType == "body length" |
                                             sub$measurementType == "body length with tail"]) > 5000)){
    x <- sample(sub$measurementValue[sub$measurementType == "body length"], 
                5000, replace = FALSE, prob = NULL)
    normal.total.length <- shapiro.test(x)
    if(isTRUE(normal.total.length[[2]] < 0.05)){
      df.norm$normality[df.norm$measurementType == "body length" |
                        df.norm$measurementType == "body length with tail" & 
                        df.norm$scientificName == sp[i] &
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "non-normal"
    }
    else if(isTRUE(normal.total.length[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "body length"  |
                        df.norm$measurementType == "body length with tail" & 
                        df.norm$scientificName == sp[i] &
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "normal"
    }
  }
  
  else if(isTRUE(length(sub$measurementValue[sub$measurementType == "body length" |
                                             sub$measurementType == "body length with tail"]) <= 5000 & 
                 length(sub$measurementValue[sub$measurementType == "body length" |
                                             sub$measurementType == "body length with tail"]) >= 3) & 
                 length(unique(sub$measurementValue[sub$measurementType == "body length" |
                                                    sub$measurementType == "body length with tail"])) > 3){
    normal.total.length <- shapiro.test(sub$measurementValue[sub$measurementType == "body length" |
                                                             sub$measurementType == "body length with tail"])
    if(isTRUE(normal.total.length[[2]] < 0.05)){
      df.norm$normality[df.norm$measurementType == "body length" |
                        df.norm$measurementType == "body length with tail" & 
                        df.norm$scientificName == sp[i] &
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "non-normal"
    }
    else if(isTRUE(normal.total.length[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "body length"  |
                        df.norm$measurementType == "body length with tail"& 
                        df.norm$scientificName == sp[i] &
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "normal"
    }
  }
  
  else{
    next
  }
}

unique(df.norm$normality[df.norm$measurementType == "body length" |
                         df.norm$measurementType == "body length with tail"])


#tail length
for(i in 1:length(sp)){
  sub <- subset(df.norm, subset = c(df.norm[,"scientificName"] == sp[i] &
                                    df.norm$measurementStatus != "outlier" &
                                    df.norm$measurementStatus != "too few records" &
                                    df.norm$lifeStage == "adult" &
                                    df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                                    df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"))
  sub <- sub %>%
    drop_na(measurementValue)
  
  if(isTRUE(length(sub$measurementValue[sub$measurementType == "tail length"]) < 3 |
            length(unique(sub$measurementValue[sub$measurementType == "tail length"])) < 3)){
    df.norm$measurementStatus[df.norm$scientificName == sp[i] & 
                              df.norm$measurementType == "tail length" &
                              df.norm$measurementStatus != "outlier" &
                              df.norm$measurementStatus != "too few records" &
                              df.norm$lifeStage == "adult" &
                              df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                              df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "too few records"
  }
  
  else if(isTRUE(length(sub$measurementValue[sub$measurementType == "tail length"]) > 5000)){
    x <- sample(sub$measurementValue[sub$measurementType == "tail length"], 
                5000, replace = FALSE, prob = NULL)
    normal.total.length <- shapiro.test(x)
    if(isTRUE(normal.total.length[[2]] < 0.05)){
      df.norm$normality[df.norm$measurementType == "tail length" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "non-normal"
    }
    else if(isTRUE(normal.total.length[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "tail length" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                          df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "normal"
    }
  }
  
  else if(isTRUE(length(sub$measurementValue[sub$measurementType == "tail length"]) <= 5000 & 
                 length(sub$measurementValue[sub$measurementType == "tail length"]) >= 3)){
    normal.tail <- shapiro.test(sub$measurementValue[sub$measurementType == "tail length"])
    if(isTRUE(normal.tail[[2]] < 0.05)){
      df.norm$normality[df.norm$measurementType == "tail length" &
                        df.norm$scientificName == sp[i] &
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "non-normal"
    }
    else if(isTRUE(normal.tail[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "tail length" & 
                        df.norm$scientificName == sp[i]&
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value" &
                        df.norm$measurementMethod != "Extracted with Traiter ; estimated value; inferred value"] <- "normal"
    }
  }
  
  else{
    next
  }
}


#make sure no data was lost
nrow(df.norm)

##write normality test
write.csv(df.norm, "normality.test.flagged.data.csv")

#### LOG TRANSFORM DATA ----

df.transform <- df.norm
df.transform$logMeasurementValue <- log10(df.transform$measurementValue)
df.transform[!is.finite(df.transform$logMeasurementValue),] <- NA

##testing
df.pema1 <- subset(df.transform, subset = c(df.transform[,"scientificName"] == "Peromyscus maniculatus" &
                                              df.transform$measurementStatus != "outlier" &
                                              df.transform$measurementStatus != "too few records" &
                                              df.transform$lifeStage == "adult" &
                                              df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                              df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                              df.transform$normality == "non-normal"))


sd.pema1 <- sd(df.pema1$measurementValue[df.pema1$measurementType == "body mass"], na.rm = TRUE)
mean.pema1 <- mean(df.pema1$measurementValue[df.pema1$measurementType == "body mass"], na.rm = TRUE)
upper.limit1 <- mean.pema1+3*sd.pema1
lower.limit1 <- mean.pema1-3*sd.pema1

plot(density(df.pema1$measurementValue[df.pema1$measurementType == "body mass"], 
             bw = .5, na.rm = TRUE))

hist(df.pema1$measurementValue[df.pema1$measurementType == "body mass"], 
     breaks = 100,
     xlim = c(0, 50),
     main = substitute(paste("Histogram of", italic("Peromyscus maniculatus"))),
     xlab = "Body Mass (g)")
     abline(v = upper.limit1, col = "black", lty = 2)
     abline(v = lower.limit1, col = "black", lty = 2)
  
df.pema2 <- subset(df.transform, subset = c(df.transform[,"scientificName"] == "Peromyscus maniculatus" &
                                                          df.transform$measurementStatus != "outlier" &
                                                          df.transform$measurementStatus != "too few records" &
                                                          df.transform$lifeStage == "adult" &
                                                          df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                                          df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                                          df.transform$normality == "non-normal")) 
df.pema3 <- df.pema2 %>%
  select(measurementValue) %>%
  drop_na(measurementValue)

outlier2 <- maha(df.pema3, 
                 cutoff = 0.95, rnames = FALSE)
index <- names(outlier2[[2]])

df.pema2[index,"measurementStatus"] <- "outlier"

sd.pema2 <- sd(df.pema2$measurementValue[df.pema2$measurementType == "body mass" &
                                         df.pema2$measurementStatus != "outlier"], na.rm = TRUE)
mean.pema2 <- mean(df.pema2$measurementValue[df.pema2$measurementType == "body mass" &
                                             df.pema2$measurementStatus != "outlier"], na.rm = TRUE)
upper.limit2 <- mean.pema2+3*sd.pema2
lower.limit2 <- mean.pema2-3*sd.pema2

plot(density(df.pema2$measurementValue[df.pema2$measurementType == "body mass"], 
             bw = .5,na.rm = TRUE))

hist(df.pema2$measurementValue[df.pema2$measurementType == "body mass"], 
     breaks = 100,
     xlim = c(0, 50),
     main = substitute(paste("Histogram of", italic("Peromyscus maniculatus"))),
     xlab = "Body Mass (g)")
abline(v = upper.limit2, col = "black", lty = 2)
abline(v = lower.limit2, col = "black", lty = 2)

## now for all species:

sp.transform <- unique(df.transform$scientificName)

#body mass
for(i in 1:length(sp.transform)){
  sub <- subset(df.transform, subset = c(df.transform$scientificName == sp.transform[i] &
                                         df.transform$measurementStatus != "outlier" &
                                         df.transform$measurementStatus != "too few records" &
                                         df.transform$lifeStage == "adult" &
                                         df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                         df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                         df.transform$normality == "non-normal"))
  sub <- sub %>%
    drop_na(logMeasurementValue)
  
  if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "body mass"]) < 3 |
            length(unique(sub$logMeasurementValue[sub$measurementType == "body mass"])) < 3)){
    df.transform$measurementStatus[df.transform$scientificName == sp.transform[i] & 
                                   df.transform$measurementType == "body mass" & 
                                   df.transform$measurementStatus != "outlier" &
                                   df.transform$measurementStatus != "too few records" &
                                   df.transform$lifeStage == "adult" &
                                   df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                   df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                   df.transform$normality == "non-normal"] <- "too few records"
  }
  
  else if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "body mass"]) > 5000)){
    x <- sample(sub$logMeasurementValue[sub$measurementType == "body mass"], 
                5000, replace = FALSE, prob = NULL)
    log.normal.body.mass <- shapiro.test(x)
    if(isTRUE(log.normal.mass[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body mass" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body mass" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "non-log.normal"
    }
  }
  else if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "body mass"]) <= 5000 & 
                   length(sub$logMeasurementValue[sub$measurementType == "body mass"]) >= 3)){
    log.normal.mass <- shapiro.test(sub$logMeasurementValue[sub$measurementType == "body mass"])
    if(isTRUE(log.normal.mass[[2]] > 0.05)){
        df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                               df.transform$measurementType == "body mass" & 
                               df.transform$measurementStatus != "outlier" &
                               df.transform$measurementStatus != "too few records" &
                               df.transform$lifeStage == "adult" &
                               df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                               df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                               df.transform$normality == "non-normal"] <- "log normal"
      }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body mass" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "non-log.normal"
    }
  }
  
  else{
    next
  }
}

unique(df.transform$normality[df.transform$measurementType == "body mass"])

#total length
for(i in 1:length(sp.transform)){
  sub <- subset(df.transform, subset = c(df.transform$scientificName == sp.transform[i] &
                                         df.transform$measurementStatus != "outlier" &
                                         df.transform$measurementStatus != "too few records" &
                                         df.transform$lifeStage == "adult" &
                                         df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                         df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                         df.transform$normality == "non-normal"))
  sub <- sub %>%
    drop_na(logMeasurementValue)
  
  if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "body length" |
                                           sub$measurementType == "body length without tail"]) < 3 |
            length(unique(sub$logMeasurementValue[sub$measurementType == "body length" |
                                                  sub$measurementType == "body length without tail"])) < 3)){
    df.transform$measurementStatus[df.transform$scientificName == sp.transform[i] & 
                                   df.transform$measurementType == "body length"|
                                   df.transform$measurementType == "body length without tail" & 
                                   df.transform$measurementStatus != "outlier" &
                                   df.transform$measurementStatus != "too few records" &
                                   df.transform$lifeStage == "adult" &
                                   df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                   df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                   df.transform$normality == "non-normal"] <- "less than 10 records"
  }
  
  else if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "body length" |
                                                sub$measurementType == "body length without tail"]) > 5000)){
    x <- sample(sub$logMeasurementValue[sub$measurementType == "body length" |
                                          sub$measurementType == "body length without tail"], 
                5000, replace = FALSE, prob = NULL)
    log.normal.body.mass <- shapiro.test(x)
    if(isTRUE(log.normal.mass[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body length" |
                             df.transform$measurementType == "body length without tail" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body length" |
                             df.transform$measurementType == "body length without tail" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "non-log.normal"
    }
  }
  
  else if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "body length" |
                                                sub$measurementType == "body length without tail"]) <= 5000 & 
                 length(sub$logMeasurementValue[sub$measurementType == "body length" |
                                                sub$measurementType == "body length without tail"]) >= 3)){
    log.normal.mass <- shapiro.test(sub$logMeasurementValue[sub$measurementType == "body length" |
                                                            sub$measurementType == "body length without tail"])
    if(isTRUE(log.normal.mass[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body length" |
                             df.transform$measurementType == "body length without tail" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body length" |
                             df.transform$measurementType == "body length without tail" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "non-log.normal"
    }
  }
  
  else{
    next
  }
}

#tail length
for(i in 1:length(sp.transform)){
  sub <- subset(df.transform, subset = c(df.transform$scientificName == sp.transform[i] &
                                         df.transform$measurementStatus != "outlier" &
                                         df.transform$measurementStatus != "too few records" &
                                         df.transform$lifeStage == "adult" &
                                         df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                         df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                         df.transform$normality == "non-normal"))
  sub <- sub %>%
    drop_na(logMeasurementValue)
  
  if(isTRUE(length(sub$measurementValue[sub$measurementType == "tail length"]) < 3)){
    df.transform$measurementStatus[df.transform$scientificName == sp.transform[i] & 
                                   df.transform$measurementType == "tail length" & 
                                   df.transform$measurementStatus != "outlier" &
                                   df.transform$measurementStatus != "too few records" &
                                   df.transform$lifeStage == "adult" &
                                   df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                                   df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                   df.transform$normality == "non-normal"] <- "too few records"
  }
  
  else if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "tail length"]) > 5000)){
    x <- sample(sub$logMeasurementValue[sub$measurementType == "tail length"], 
                5000, replace = FALSE, prob = NULL)
    log.normal.body.mass <- shapiro.test(x)
    if(isTRUE(log.normal.mass[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "tail length" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "tail length" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "non-log.normal"
    }
  }
  
  else if(isTRUE(length(sub$logMeasurementValue[sub$measurementType == "tail length"]) <= 5000 & 
                 length(sub$logMeasurementValue[sub$measurementType == "tail length"]) >= 3)){
    log.normal.mass <- shapiro.test(sub$logMeasurementValue[sub$measurementType == "tail length"])
    if(isTRUE(log.normal.mass[[2]] > 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "tail length" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "tail length" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value" &
                             df.transform$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             df.transform$normality == "non-normal"] <- "non-log.normal"
    }
  }
  
  else{
    next
  }
}

unique(df.transform$normality[df.transform$measurementType == "tail length"])

#make sure no data was lost
nrow(df.transform)

##write normality test
write.csv(df.transform, "log.normality.test.flagged.data.csv")

#### QUANTILE LIMITS ----
df.quant <- df.transform

df.pema.quant <- subset(df.quant, subset = df.quant$scientificName == "Peromyscus maniculatus" &
                                           df.quant$measurementType == "body mass" &
                                           df.quant$measurementStatus != "outlier" &
                                           df.quant$measurementStatus != "too few records" &
                                           df.quant$lifeStage == "adult" &
                                           df.quant$measurementMethod != "Extracted with Traiter ; estimated value" &
                                           df.quant$measurementMethod != "Extracted with Traiter ; estimated value; inferred value")
df.pema.quant <- df.pema.quant %>%
  drop_na(measurementValue)
quant.pema.mass <- quantile(df.pema.quant$measurementValue, 
                            probs = seq(0,1,.05))
quant.pema.mass[[2]] #5%
quant.pema.mass[[20]] #95%
nrow(df.pema.quant)

df.quant$lowerLimit <- ""
df.quant$upperLimit <- ""
df.quant$lowerLimitMethod <- ""
df.quant$upperLimitMethod <- ""

##do quantiles for non-normal data (only unique values are non-log.normal)

sp <- unique(df.quant$scientificName)

#mass
for(i in 1:length(sp)){
  sub <- subset(df.quant, subset = df.quant$scientificName == sp[i] &
                                   df.quant$measurementType == "body mass" &
                                   df.quant$lifeStage == "adult" &
                                   df.quant$measurementStatus != "outlier" &
                                   df.quant$measurementStatus != "too few records" &
                                   df.quant$measurementMethod != "Extracted with Traiter ; estimated value" &
                                   df.quant$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                   df.quant$normality == "non-log.normal")
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(nrow(sub) > 0)){
  df.quant$upperLimit[df.quant$measurementType == "body mass" &
                      df.quant$scientificName == sp[i] &
                      df.quant$lifeStage != "juvenile"] <- quantile(sub$measurementValue, probs = seq(0,1,.05))[[20]]
  
  df.quant$lowerLimit[df.quant$measurementType == "body mass" &
                      df.quant$scientificName == sp[i] &
                      df.quant$lifeStage != "juvenile"] <- quantile(sub$measurementValue, probs = seq(0,1,.05))[[2]]
  
  df.quant$lowerLimitMethod[df.quant$measurementType == "body mass" &
                            df.quant$scientificName == sp[i] &
                            df.quant$lifeStage != "juvenile"] <- "quantile adults, non-estimated values, no outliers"
  
  df.quant$upperLimitMethod[df.quant$measurementType == "body mass" &
                            df.quant$scientificName == sp[i] &
                            df.quant$lifeStage != "juvenile"] <- "quantile adults, non-estimated values, no outliers"
  
  }
  else{
    next
  }
}

unique(df.quant$lowerLimitMethod)
head(df.quant$lowerLimit[df.quant$normality != "non-log.normal"]) #should be ""
head(df.quant$lowerLimit[df.quant$normality == "non-log.normal"]) #should have values

#total length
for(i in 1:length(sp)){
  sub <- subset(df.quant, subset = df.quant$scientificName == sp[i] &
                df.quant$measurementType == "body length" |
                df.quant$measurementType == "body length with tail" &
                df.quant$lifeStage == "adult" &
                df.quant$measurementStatus != "outlier" &
                df.quant$measurementStatus != "too few records" &
                df.quant$measurementMethod != "Extracted with Traiter ; estimated value" &
                df.quant$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                df.quant$normality == "non-log.normal")
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(nrow(sub) > 0)){
    df.quant$upperLimit[df.quant$measurementType == "body length"  |
                        df.quant$measurementType == "body length with tail" &
                        df.quant$scientificName == sp[i] &
                        df.quant$lifeStage != "juvenile"] <-quantile(sub$measurementValue, probs = seq(0,1,.05))[[20]]
    
    df.quant$lowerLimit[df.quant$measurementType == "body length"  |
                        df.quant$measurementType == "body length with tail" &
                        df.quant$scientificName == sp[i] &
                        df.quant$lifeStage != "juvenile"] <- quantile(sub$measurementValue, probs = seq(0,1,.05))[[2]]
    
    df.quant$lowerLimitMethod[df.quant$measurementType == "body length"  |
                              df.quant$measurementType == "body length with tail" &
                              df.quant$scientificName == sp[i] &
                              df.quant$lifeStage != "juvenile"] <- "quantile adults, non-estimated values, no outliers"
    
    df.quant$upperLimitMethod[df.quant$measurementType == "body length"  |
                              df.quant$measurementType == "body length with tail" &
                              df.quant$scientificName == sp[i] &
                              df.quant$lifeStage != "juvenile"] <- "quantile adults, non-estimated values, no outliers"
    
  }
  else{
    next
  }
}

unique(df.quant$upperLimitMethod[df.quant$measurementType == "body length" |
                                 df.quant$measurementType == "body length with tail"])

#tail length
for(i in 1:length(sp)){
  sub <- subset(df.quant, subset = df.quant$scientificName == sp[i] &
                df.quant$measurementType == "tail length" &
                df.quant$lifeStage == "adult" &
                df.quant$measurementStatus != "outlier" &
                df.quant$measurementStatus != "too few records" &
                df.quant$measurementMethod != "Extracted with Traiter ; estimated value" &
                df.quant$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                df.quant$normality == "non-log.normal")
  sub <- sub %>%
    drop_na(measurementValue)
  if(isTRUE(nrow(sub) > 0)){
    df.quant$upperLimit[df.quant$measurementType == "tail length" &
                        df.quant$scientificName == sp[i] &
                        df.quant$lifeStage != "juvenile"] <- quantile(sub$measurementValue, probs = seq(0,1,.05))[[20]]
    
    df.quant$lowerLimit[df.quant$measurementType == "tail length" &
                        df.quant$scientificName == sp[i] &
                        df.quant$lifeStage != "juvenile"] <- quantile(sub$measurementValue, probs = seq(0,1,.05))[[2]]
    
    df.quant$lowerLimitMethod[df.quant$measurementType == "tail length" &
                              df.quant$scientificName == sp[i] &
                              df.quant$lifeStage != "juvenile"] <- "quantile adults, non-estimated values, no outliers"
    
    df.quant$upperLimitMethod[df.quant$measurementType == "tail length" &
                              df.quant$scientificName == sp[i] &
                              df.quant$lifeStage != "juvenile"] <- "quantile adults, non-estimated values, no outliers"
    
  }
  else{
    next
  }
}

unique(df.quant$upperLimitMethod[df.quant$measurementType == "tail length"])

## LABEL OUTLIERS
df.quant$measurementStatus[df.quant$measurementValue < df.quant$lowerLimit &
                           df.quant$measurementStatus != "outlier" &
                           df.quant$lowerLimitMethod == "quantile adults, non-estimated values, no outliers" &
                           df.quant$measurementStatus != "too few records" &
                           df.quant$lifeStage != "juvenile"] <- "juvenile.quant"

unique(df.quant$measurementStatus)
unique(df.quant$measurementStatus[df.quant$measurementValue < df.quant$lowerLimit &
                                    df.quant$measurementStatus != "outlier" &
                                    df.quant$lowerLimitMethod == "quantile adults, non-estimated values, no outliers" &
                                    df.quant$measurementStatus != "too few records" &
                                    df.quant$lifeStage != "juvenile" &
                                    df.quant$normality == "non-log.normal"] )

df.quant$measurementStatus[df.quant$measurementValue > df.quant$upperLimit &
                           df.quant$upperLimitMethod == "quantile adults, non-estimated values, no outliers" &
                           df.quant$measurementStatus != "outlier" &
                           df.quant$measurementStatus != "too few records" &
                           df.quant$lifeStage != "juvenile"] <- "outlier.quant"

df.quant$measurementStatus[df.quant$measurementValue >= df.quant$lowerLimit &
                           df.quant$measurementValue <= df.quant$upperLimit  &
                           df.quant$lowerLimitMethod == "quantile adults, non-estimated values, no outliers" &
                           df.quant$upperLimitMethod == "quantile adults, non-estimated values, no outliers" &
                           df.quant$measurementStatus != "outlier" &
                           df.quant$measurementStatus != "too few records" &
                           df.quant$lifeStage != "juvenile"] <- "possible adult; possibly good"
                

##write out dataset
write.csv(df.quant, "quantile.flagged.data.csv")

#### SD LIMITS ----
df.sigma <- df.quant

df.sigma$meanValue <- NA
df.sigma$meanValue <- as.numeric(df.sigma$meanValue)
df.sigma$sdValue <- NA
df.sigma$sdValue <- as.numeric(df.sigma$sdValue)
df.sigma$upperLimit <- as.numeric(df.sigma$upperLimit)
df.sigma$lowerLimit <- as.numeric(df.sigma$lowerLimit)

##test
limit.test <- df.sigma[df.sigma$scientificName == "Microtus californicus" &
                         df.sigma$measurementType == "body mass",] %>%
  drop_na(measurementValue)

limit.test.sub <- limit.test[limit.test$lifeStage == "adult" &
                             limit.test$measurementStatus != "outlier" &
                             limit.test$measurementStatus != "too few records" &
                             limit.test$measurementMethod != "Extracted with Traiter ; estimated value" &
                             limit.test$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                             limit.test$normality == "normal",]  

limit.test$meanValue[limit.test$lifeStage != "juvenile"] <- mean(limit.test.sub$measurementValue, na.rm = TRUE)

limit.test$sdValue[limit.test$lifeStage != "juvenile"] <- sd(limit.test.sub$measurementValue, na.rm = TRUE)

limit.test$lowerLimit[limit.test$lifeStage != "juvenile"] <- mean(limit.test.sub$measurementValue, na.rm = TRUE) - 3*sd(limit.test.sub$measurementValue, na.rm = TRUE)

limit.test$upperLimit[limit.test$lifeStage != "juvenile"] <- mean(limit.test.sub$measurementValue, na.rm = TRUE) + 3*sd(limit.test.sub$measurementValue, na.rm = TRUE)

limit.test$measurementStatus[limit.test$measurementValue < limit.test$lowerLimit &
                             limit.test$measurementStatus != "outlier" &
                             limit.test$measurementStatus != "too few records" &
                             limit.test$lifeStage != "juvenile"] <- "juvenile.sd"

limit.test$measurementStatus[limit.test$measurementValue > limit.test$upperLimit &
                             limit.test$measurementStatus != "outlier" &
                             limit.test$measurementStatus != "too few records" &
                             limit.test$lifeStage != "juvenile"] <- "outlier.sd"

limit.test$measurementStatus[limit.test$measurementValue >= limit.test$lowerLimit &
                               limit.test$measurementValue <= limit.test$upperLimit  &
                               limit.test$measurementStatus != "outlier" &
                               limit.test$measurementStatus != "too few records" &
                               limit.test$lifeStage != "juvenile"] <- "possible adult; possibly good"
##now for all:

#mass 
for(i in 1:length(sp)){
  sub <- subset(df.sigma, subset = df.sigma$scientificName == sp[i] &
                                   df.sigma$measurementType == "body mass" &
                                   df.sigma$lifeStage == "adult" &
                                   df.sigma$measurementStatus != "outlier" &
                                   df.sigma$measurementStatus != "too few records" &
                                   df.sigma$measurementMethod != "Extracted with Traiter ; estimated value" &
                                   df.sigma$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                                   df.sigma$normality == "normal")
  
  sub <- sub %>%
    drop_na(measurementValue)
        
  if(isTRUE(nrow(sub) > 0) & isTRUE(nrow(sub) > 3)){
    df.sigma$meanValue[df.sigma$scientificName == sp[i] &
                       df.sigma$measurementType == "body mass" &
                       df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$sdValue[df.sigma$scientificName == sp[i] &
                     df.sigma$measurementType == "body mass" &
                     df.sigma$lifeStage != "juvenile"] <- sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$upperLimit[df.sigma$scientificName == sp[i] &
                        df.sigma$measurementType == "body mass" &
                        df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE) + 3*sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$lowerLimit[df.sigma$scientificName == sp[i] &
                        df.sigma$measurementType == "body mass" &
                        df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE) - 3*sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$meanMethod[df.sigma$scientificName == sp[i] &
                        df.sigma$measurementType == "body mass" &
                        df.sigma$lifeStage != "juvenile"] <- "mean adults, non-estimated values, no outliers"
    
    df.sigma$sdMethod[df.sigma$scientificName == sp[i] &
                      df.sigma$measurementType == "body mass" &
                      df.sigma$lifeStage != "juvenile"] <- "sd adults, non-estimated values, no outliers"
    
    df.sigma$upperLimitMethod[df.sigma$scientificName == sp[i] &
                              df.sigma$measurementType == "body mass" &
                              df.sigma$lifeStage != "juvenile"] <- "sd"
    
    df.sigma$lowerLimitMethod[df.sigma$scientificName == sp[i] &
                              df.sigma$measurementType == "body mass" &
                              df.sigma$lifeStage != "juvenile"] <- "sd"
    
  }
  else{
    next
  }
}

unique(df.sigma$lowerLimitMethod)
head(df.sigma$lowerLimit[df.sigma$normality != "normal"]) #should be "" or "sigma..."
head(df.sigma$lowerLimit[df.sigma$normality == "normal" & 
                         !is.na(df.sigma$meanValue)]) #should have values

#total length
for(i in 1:length(sp)){
  sub <- subset(df.sigma, subset = df.sigma$scientificName == sp[i] &
                  df.sigma$measurementType == "body length" |
                  df.sigma$measurementType == "body length with tail" &
                  df.sigma$lifeStage == "adult" &
                  df.sigma$measurementStatus != "outlier" &
                  df.sigma$measurementStatus != "too few records" &
                  df.sigma$measurementMethod != "Extracted with Traiter ; estimated value" &
                  df.sigma$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                  df.sigma$normality == "normal")
  
  sub <- sub %>%
    drop_na(measurementValue)
  
  if(isTRUE(nrow(sub) > 0) & isTRUE(nrow(sub) > 3)){
    df.sigma$meanValue[df.sigma$scientificName == sp[i] &
                         df.sigma$measurementType == "body length"  |
                         df.sigma$measurementType == "body length with tail" &
                         df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$sdValue[df.sigma$scientificName == sp[i] &
                       df.sigma$measurementType == "body length"  |
                       df.sigma$measurementType == "body length with tail" &
                       df.sigma$lifeStage != "juvenile"] <- sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$upperLimit[df.sigma$scientificName == sp[i] &
                          df.sigma$measurementType == "body length"  |
                          df.sigma$measurementType == "body length with tail" &
                          df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE) + 3*sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$lowerLimit[df.sigma$scientificName == sp[i] &
                          df.sigma$measurementType == "body length"  |
                          df.sigma$measurementType == "body length with tail" &
                          df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE) - 3*sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$meanMethod[df.sigma$scientificName == sp[i] &
                          df.sigma$measurementType == "body length"  |
                          df.sigma$measurementType == "body length with tail" &
                          df.sigma$lifeStage != "juvenile"] <- "mean adults, non-estimated values, no outliers"
    
    df.sigma$sdMethod[df.sigma$scientificName == sp[i] &
                        df.sigma$measurementType == "body length"  |
                        df.sigma$measurementType == "body length with tail" &
                        df.sigma$lifeStage != "juvenile"] <- "sd adults, non-estimated values, no outliers"
    
    df.sigma$upperLimitMethod[df.sigma$scientificName == sp[i] &
                                df.sigma$measurementType == "body length"  |
                                df.sigma$measurementType == "body length with tail" &
                                df.sigma$lifeStage != "juvenile"] <- "sd"
    
    df.sigma$lowerLimitMethod[df.sigma$scientificName == sp[i] &
                                df.sigma$measurementType == "body length"  |
                                df.sigma$measurementType == "body length with tail" &
                                df.sigma$lifeStage != "juvenile"] <- "sd"
    
  }
  else{
    next
  }
}

#tail length
for(i in 1:length(sp)){
  sub <- subset(df.sigma, subset = df.sigma$scientificName == sp[i] &
                  df.sigma$measurementType == "tail length" &
                  df.sigma$lifeStage == "adult" &
                  df.sigma$measurementStatus != "outlier" &
                  df.sigma$measurementStatus != "too few records" &
                  df.sigma$measurementMethod != "Extracted with Traiter ; estimated value" &
                  df.sigma$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                  df.sigma$normality == "normal")
  
  sub <- sub %>%
    drop_na(measurementValue)
  
  if(isTRUE(nrow(sub) > 0) & isTRUE(nrow(sub) > 3)){
    df.sigma$meanValue[df.sigma$scientificName == sp[i] &
                         df.sigma$measurementType == "tail length" &
                         df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$sdValue[df.sigma$scientificName == sp[i] &
                       df.sigma$measurementType == "tail length" &
                       df.sigma$lifeStage != "juvenile"] <- sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$upperLimit[df.sigma$scientificName == sp[i] &
                          df.sigma$measurementType == "tail length" &
                          df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE) + 3*sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$lowerLimit[df.sigma$scientificName == sp[i] &
                          df.sigma$measurementType == "tail length" &
                          df.sigma$lifeStage != "juvenile"] <- mean(sub$measurementValue, na.rm = TRUE) - 3*sd(sub$measurementValue, na.rm = TRUE)
    
    df.sigma$meanMethod[df.sigma$scientificName == sp[i] &
                          df.sigma$measurementType == "tail length" &
                          df.sigma$lifeStage != "juvenile"] <- "mean adults, non-estimated values, no outliers"
    
    df.sigma$sdMethod[df.sigma$scientificName == sp[i] &
                        df.sigma$measurementType == "tail length" &
                        df.sigma$lifeStage != "juvenile"] <- "sd adults, non-estimated values, no outliers"
    
    df.sigma$upperLimitMethod[df.sigma$scientificName == sp[i] &
                                df.sigma$measurementType == "tail length" &
                                df.sigma$lifeStage != "juvenile"] <- "sd"
    
    df.sigma$lowerLimitMethod[df.sigma$scientificName == sp[i] &
                                df.sigma$measurementType == "tail length" &
                                df.sigma$lifeStage != "juvenile"] <- "sd"
    
  }
  else{
    next
  }
}

## LABEL OUTLIERS
df.sigma$measurementStatus[df.sigma$measurementValue < df.sigma$lowerLimit &
                           df.sigma$lowerLimitMethod == "sd" &
                           df.sigma$measurementStatus != "outlier" &
                           df.sigma$measurementStatus != "too few records" &
                           df.sigma$lifeStage != "juvenile"] <- "juvenile.sd"

df.sigma$measurementStatus[df.sigma$measurementValue > df.sigma$upperLimit &
                           df.sigma$upperLimitMethod == "sd" &
                           df.sigma$measurementStatus != "outlier" &
                           df.sigma$measurementStatus != "too few records" &
                           df.sigma$lifeStage != "juvenile"] <- "outlier.sd"

df.sigma$measurementStatus[df.sigma$measurementValue >= df.sigma$lowerLimit &
                           df.sigma$measurementValue <= df.sigma$upperLimit  &
                           df.sigma$lowerLimitMethod == "sd" &
                           df.sigma$upperLimitMethod == "sd" &
                           df.sigma$measurementStatus != "outlier" &
                           df.sigma$measurementStatus != "too few records" &
                           df.sigma$lifeStage != "juvenile"] <- "possible adult; possibly good"


##write out data
write.csv(df.sigma, "normal.outliers.flagged.csv")

#### LOG SD LIMITS ----
df.logSigma <- df.sigma

#mass
for(i in 1:length(sp)){
  sub <- subset(df.logSigma, subset = df.logSigma$scientificName == sp[i] &
                  df.logSigma$measurementType == "body mass" &
                  df.logSigma$lifeStage == "adult" &
                  df.logSigma$measurementStatus != "outlier" &
                  df.logSigma$measurementStatus != "too few records" &
                  df.logSigma$measurementMethod != "Extracted with Traiter ; estimated value" &
                  df.logSigma$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                  df.logSigma$normality == "log normal")
  
  sub <- sub %>%
    drop_na(logMeasurementValue)
  
  if(isTRUE(nrow(sub) > 0) & isTRUE(nrow(sub) > 3)){
    df.logSigma$meanValue[df.logSigma$scientificName == sp[i] &
                       df.logSigma$measurementType == "body mass" &
                       df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$sdValue[df.logSigma$scientificName == sp[i] &
                     df.logSigma$measurementType == "body mass" &
                     df.logSigma$lifeStage != "juvenile"] <- sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$upperLimit[df.logSigma$scientificName == sp[i] &
                        df.logSigma$measurementType == "body mass" &
                        df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE) + 3*sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$lowerLimit[df.logSigma$scientificName == sp[i] &
                          df.logSigma$measurementType == "body mass" &
                          df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE) - 3*sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$meanMethod[df.logSigma$scientificName == sp[i] &
                          df.logSigma$measurementType == "body mass" &
                          df.logSigma$lifeStage != "juvenile"] <- "log mean adults, non-estimated values, no outliers"
    
    df.logSigma$sdMethod[df.logSigma$scientificName == sp[i] &
                        df.logSigma$measurementType == "body mass" &
                        df.logSigma$lifeStage != "juvenile"] <- "log sd adults, non-estimated values, no outliers"
    
    df.logSigma$upperLimitMethod[df.logSigma$scientificName == sp[i] &
                                df.logSigma$measurementType == "body mass" &
                                df.logSigma$lifeStage != "juvenile"] <- "log sd"
    
    df.logSigma$lowerLimitMethod[df.logSigma$scientificName == sp[i] &
                                df.logSigma$measurementType == "body mass" &
                                df.logSigma$lifeStage != "juvenile"] <- "log sd"
    
  }
  else{
    next
  }
}


unique(df.logSigma$lowerLimitMethod)
head(df.logSigma$lowerLimit[df.logSigma$normality != "log normal"]) #should be "" or "sigma..."
head(df.logSigma$lowerLimit[df.logSigma$normality == "log normal" & 
                           !is.na(df.logSigma$meanValue)]) #should have values

#total length
for(i in 1:length(sp)){
  sub <- subset(df.logSigma, subset = df.logSigma$scientificName == sp[i] &
                df.logSigma$measurementType == "body length" |
                df.logSigma$measurementType == "body length with tail" &
                df.logSigma$lifeStage == "adult" &
                df.logSigma$measurementStatus != "outlier" &
                df.logSigma$measurementStatus != "too few records" &
                df.logSigma$measurementMethod != "Extracted with Traiter ; estimated value" &
                df.logSigma$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                df.logSigma$normality == "log normal")
  
  sub <- sub %>%
    drop_na(logMeasurementValue)
  
  if(isTRUE(nrow(sub) > 0) & isTRUE(nrow(sub) > 3)){
    df.logSigma$meanValue[df.logSigma$scientificName == sp[i] &
                          df.logSigma$measurementType == "body length" |
                          df.logSigma$measurementType == "body length with tail"  &
                          df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$sdValue[df.logSigma$scientificName == sp[i] &
                        df.logSigma$measurementType == "body length"|
                        df.logSigma$measurementType == "body length with tail" &
                        df.logSigma$lifeStage != "juvenile"] <- sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$upperLimit[df.logSigma$scientificName == sp[i] &
                           df.logSigma$measurementType == "body length" |
                           df.logSigma$measurementType == "body length with tail" &
                           df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE) + 3*sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$lowerLimit[df.logSigma$scientificName == sp[i] &
                           df.logSigma$measurementType == "body length" |
                           df.logSigma$measurementType == "body length with tail"  &
                           df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE) - 3*sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$meanMethod[df.logSigma$scientificName == sp[i] &
                           df.logSigma$measurementType == "body length" |
                           df.logSigma$measurementType == "body length with tail"  &
                           df.logSigma$lifeStage != "juvenile"] <- "log mean adults, non-estimated values, no outliers"
    
    df.logSigma$sdMethod[df.logSigma$scientificName == sp[i] &
                         df.logSigma$measurementType == "body length" |
                         df.logSigma$measurementType == "body length with tail"  &
                         df.logSigma$lifeStage != "juvenile"] <- "log sd adults, non-estimated values, no outliers"
    
    df.logSigma$upperLimitMethod[df.logSigma$scientificName == sp[i] &
                                 df.logSigma$measurementType == "body length" |
                                 df.logSigma$measurementType == "body length with tail"  &
                                 df.logSigma$lifeStage != "juvenile"] <- "log sd"
    
    df.logSigma$lowerLimitMethod[df.logSigma$scientificName == sp[i] &
                                 df.logSigma$measurementType == "body length" |
                                 df.logSigma$measurementType == "body length with tail"  &
                                 df.logSigma$lifeStage != "juvenile"] <- "log sd"
    
  }
  else{
    next
  }
}

#tail length
for(i in 1:length(sp)){
  sub <- subset(df.logSigma, subset = df.logSigma$scientificName == sp[i] &
                  df.logSigma$measurementType == "tail length" &
                  df.logSigma$lifeStage == "adult" &
                  df.logSigma$measurementStatus != "outlier" &
                  df.logSigma$measurementStatus != "too few records" &
                  df.logSigma$measurementMethod != "Extracted with Traiter ; estimated value" &
                  df.logSigma$measurementMethod != "Extracted with Traiter ; estimated value; inferred value" &
                  df.logSigma$normality == "log normal")
  
  sub <- sub %>%
    drop_na(logMeasurementValue)
  
  if(isTRUE(nrow(sub) > 0) & isTRUE(nrow(sub) > 3)){
    df.logSigma$meanValue[df.logSigma$scientificName == sp[i] &
                         df.logSigma$measurementType == "tail length" &
                         df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$sdValue[df.logSigma$scientificName == sp[i] &
                       df.logSigma$measurementType == "tail length" &
                       df.logSigma$lifeStage != "juvenile"] <- sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$upperLimit[df.logSigma$scientificName == sp[i] &
                          df.logSigma$measurementType == "tail length" &
                          df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE) + 3*sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$lowerLimit[df.logSigma$scientificName == sp[i] &
                          df.logSigma$measurementType == "tail length" &
                          df.logSigma$lifeStage != "juvenile"] <- mean(sub$logMeasurementValue, na.rm = TRUE) - 3*sd(sub$logMeasurementValue, na.rm = TRUE)
    
    df.logSigma$meanMethod[df.logSigma$scientificName == sp[i] &
                          df.logSigma$measurementType == "tail length" &
                          df.logSigma$lifeStage != "juvenile"] <- "log mean adults, non-estimated values, no outliers"
    
    df.logSigma$sdMethod[df.logSigma$scientificName == sp[i] &
                        df.logSigma$measurementType == "tail length" &
                        df.logSigma$lifeStage != "juvenile"] <- "log sd adults, non-estimated values, no outliers"
    
    df.logSigma$upperLimitMethod[df.logSigma$scientificName == sp[i] &
                                df.logSigma$measurementType == "tail length" &
                                df.logSigma$lifeStage != "juvenile"] <- "log sd"
    
    df.logSigma$lowerLimitMethod[df.logSigma$scientificName == sp[i] &
                                df.logSigma$measurementType == "tail length" &
                                df.logSigma$lifeStage != "juvenile"] <- "log sd"
    
  }
  else{
    next
  }
}

## LABEL OUTLIERS

df.logSigma$measurementStatus[df.logSigma$logMeasurementValue < df.logSigma$lowerLimit &
                              df.logSigma$lowerLimitMethod == "log sd" &
                              df.logSigma$measurementStatus != "outlier" &
                              df.logSigma$measurementStatus != "too few records" &
                              df.logSigma$lifeStage != "juvenile"] <- "juvenile.log.sd"

df.logSigma$measurementStatus[df.logSigma$logMeasurementValue > df.logSigma$upperLimit &
                              df.logSigma$upperLimitMethod == "log sd" &
                              df.logSigma$measurementStatus != "outlier" &
                              df.logSigma$measurementStatus != "too few records" &
                              df.logSigma$lifeStage != "juvenile"] <- "outlier.log.sd"

df.logSigma$measurementStatus[df.logSigma$logMeasurementValue >= df.logSigma$lowerLimit &
                              df.logSigma$logMeasurementValue <= df.logSigma$upperLimit  &
                              df.logSigma$lowerLimitMethod == "log sd" &
                              df.logSigma$upperLimitMethod == "log sd" &
                              df.logSigma$measurementStatus != "outlier" &
                              df.logSigma$measurementStatus != "too few records" &
                              df.logSigma$lifeStage != "juvenile"] <- "possible adult; possibly good"

###write out data
write.csv(df.logSigma, "log.normal.flagged.data.csv")

##Figure 1, Panel 3----
df.fig3 <- df.logSigma[df.logSigma$lifeStage != "juvenile",]

df.fig3$cat <- paste(df.fig3$lifeStage, df.fig3$measurementStatus)
unique(df.fig3$cat)
df.fig3$cat[df.fig3$cat == "adult possible adult; possibly good"] <- "Adult; possibly good" #darkorchid4
df.fig3$cat[df.fig3$cat == "adult outlier" |
            df.fig3$cat == "adult juvenile.sd" |
            df.fig3$cat == "adult juvenile.log.sd" |
            df.fig3$cat == "adult outlier.quant" |
            df.fig3$cat == "adult juvenile.quant" |
            df.fig3$cat == "adult outlier.log.sd"] <- "Adult; outlier" #darkorchid1
df.fig3$cat[df.fig3$cat == "adult too few records"] <- "Adult; too few records" #gray74
df.fig3$cat[df.fig3$cat == "adult "] <- "Adult; untested" #darkorchid
df.fig3$cat[df.fig3$cat == "Not Collected possible adult; possibly good"] <- "No stage; possibly good" #lightgoldenrod3
df.fig3$cat[df.fig3$cat == "Not Collected outlier" |
            df.fig3$cat == "Not Collected outlier.quant" |
            df.fig3$cat == "Not Collected outlier.sd" |
            df.fig3$cat == "Not Collected outlier.log.sd" |
            df.fig3$cat == "Not Collected juvenile.quant" | 
            df.fig3$cat == "Not Collected juvenile.sd" |
            df.fig3$cat == "Not Collected juvenile.log.sd"] <- "No stage; outlier" #lightgoldenrodyellow
df.fig3$cat[df.fig3$cat == "Not Collected too few records"] <- "No stage; too few records" #gray74
df.fig3$cat[df.fig3$cat == "Not Collected " |
            df.fig3$cat == "NA NA"] <- "No stage; untested" #lightgoldenrod1

df.fig3$cat <- as.factor(df.fig3$cat)
df.fig3$cat = relevel(df.fig3$cat, "Adult; possibly good")
df.fig3$cat <- factor(df.fig3$cat, levels = c("Adult; possibly good", "Adult; outlier", "Adult; untested", "Adult; too few records", 
                                                  "No stage; possibly good", "No stage; outlier", "No stage; untested", "No stage; too few records"))

df.pema3 <- subset(df.fig3, df.fig3$scientificName == "Peromyscus maniculatus" & 
                              df.fig3$measurementType == "body mass" &
                              !is.na(df.fig3$measurementValue))
df.pema3$measurementValue <- as.numeric(df.pema3$measurementValue)
length(df.pema3$measurementValue) #30738
unique(df.pema3$cat)
length(df.pema3$cat[df.pema3$cat == "Adult; possibly good"]) #2773
length(df.pema3$cat[df.pema3$cat == "Adult; outlier"]) #248
length(df.pema3$cat[df.pema3$cat == "No stage; possibly good"]) #24266
length(df.pema3$cat[df.pema3$cat == "No stage; outlier"]) #3421

outlier <- c("outlier", "outlier.log.sd", "juvenile.sd", "juvenile.log.sd", "juvenile.quant", "outlier.quant")

p.pema3 <- ggplot() + 
  geom_density(data = filter(df.pema3, measurementStatus %in% outlier), aes(x = measurementValue), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df.pema3, measurementStatus %in% outlier), aes(x = measurementValue), sides = "b", col = "gray34") +
  geom_density(data = df.pema3, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod3", "lightgoldenrodyellow"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30708, Noutlier = 3673") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 50)) +
  scale_y_continuous(name = "Density", limits = c(0, .25))
ggsave(p.pema3, file=paste0("check.test.mouse",".png"), width = 14, height = 10, units = "cm")

df.otbe3 <- subset(df.fig3, df.fig3$scientificName == "Otospermophilus beecheyi" & 
                              df.fig3$measurementType == "body mass" &
                              !is.na(df.fig3$measurementValue))
df.otbe3$measurementValue <- as.numeric(df.otbe3$measurementValue)
length(df.otbe3$measurementValue) #222
unique(df.otbe3$cat)
length(df.otbe3$cat[df.otbe3$cat == "No stage; outlier"]) #8
length(df.otbe3$cat[df.otbe3$cat == "No stage; possibly good"]) #186
length(df.otbe3$cat[df.otbe3$cat == "Adult; possibly good"]) #27
length(df.otbe3$cat[df.otbe3$cat == "Adult; outlier"]) #1
p.otbe3 <- ggplot() + 
  geom_density(data = filter(df.otbe3, measurementStatus %in% outlier), aes(x = measurementValue), color = NA, alpha = 0.4) +
  geom_rug(data = filter(df.otbe3, measurementStatus %in% outlier), aes(x = measurementValue), sides = "b", col = "gray34") +
  geom_density(data = df.otbe3, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod3", "lightgoldenrodyellow"),
                    name = "Data Quality Category") +
  ggtitle("Otospermophilus beecheyi N = 222, Noutlier = 9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .0125))
ggsave(p.otbe3, file=paste0("check.test.squirrel",".png"), width = 14, height = 10, units = "cm")

##info about outliers----
outlier <- c("juvenile.quant", "juvenile.sd", "juvenile.log.sd", "outlier", "outlier.quant", "outlier.sd", "outlier.log.sd")
good <- c("possible adult; possibly good", "", NA)

outlier_stats <- df.logSigma %>%
  group_by(scientificName) %>%
  dplyr::summarise(sample.outlier.mass = length(measurementValue[measurementStatus %in% outlier & 
                                                                   measurementValue >= 0 & measurementType == "body mass" &
                                                                   lifeStage != "Juvenile"]),
                   sample.mass = length(measurementValue[measurementStatus %in% good & 
                                                           measurementValue >= 0 & 
                                                           measurementType == "body mass" &
                                                           lifeStage != "Juvenile"])) %>%
  as.data.frame()

sum(outlier_stats$sample.outlier.mass) #54659


##write out csv of outlier stats----
write.csv(outlier_stats, "outliers.csv")

##table showing data we have
df <- df.logSigma
df.mass <- df[df$measurementType == "body mass" &
              df$measurementStatus %in% good &
              df$lifeStage != "juvenile",] %>%
  drop_na(measurementValue)

df.stats <- df.mass %>%
  group_by(scientificName, sex, lifeStage) %>%
  dplyr::summarise(N = n())

#### FINALL DATA ----

write.csv(df.logSigma, "BPP.data.csv")


