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

r_all_futres <- futres_data()

#Villaseñor = Project 277
#Blois 2018 = Project 278
#EAP K.Emery = Project 282
#ODFW = Project 294
#Bernor = Project 314
#Hopkins = Project 410; not part of BPP
#Yrarrazaval = Project 406; not part of BPP
#Machado = Project 375; not part of BPP
#Reuter = Project 409; not part of BPP
#Vertnet, extracted using traiter; all values converted to standard units g or mm; include metadata on inferring measurement unit and estimating value if in a string 

## write out compiled list
write.csv(r_all_futres, "futres_datastore.csv")

## write out data
futres <- r_all_futres$data
nrow(futres)
#2408414 rows
nrow(futres[futres$projectID == "Vertnet",])
#2384293 are VertNet 
colnames(futres)

write.csv(futres, "futres.data.csv")

df <- futres

nrow(df) #2408414

##get rid of projects not included in paper
unique(df$projectID)
df <- df %>%
  dplyr::filter(projectID != "https://geome-db.org/workbench/project-overview?projectId=410",
                projectID != "https://geome-db.org/workbench/project-overview?projectId=406",
                projectID != "https://geome-db.org/workbench/project-overview?projectId=409",
                projectID != "https://geome-db.org/workbench/project-overview?projectId=375")

##clean up lifeStage
unique(df$lifeStage)
df$lifeStage[df$lifeStage == "" |
             df$lifeStage == "lifeStage"] <- "Not Collected"

##clean up measurementValue
df$measurementValue <- as.numeric(df$measurementValue)
df$measurementValue[df$measurementValue == 0] <- NA

df <- df[!grepl('sp.', df$scientificName),]
df <- df[!grepl('aff.', df$scientificName),]
df <- df[!grepl('cf.', df$scientificName),]

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
length(unique(df$scientificName)) #3252

x <- df %>%
  group_by(scientificName) %>%
  dplyr::summarise(n = n())

#change known taxonomy errors:
df$scientificName[df$scientificName == "Spermophilus beechyi"] <- "Spermophilus beecheyi"
df$scientificName[df$scientificName == "Spermophilus beecheyi"] <- "Otospermophilus beecheyi"

df <- df[df$scientificName != "Microtus sp" |
         df$scientificName != "Microtus ssp" |
         df$scientificName != "Acomys I." |
         df$scientificName != "Neotoma Say" |
         df$scientificName != "Lemmus Link" |
         df$scientificName != "Equid lg." |
         df$scientificName != "Peromyscus sp" |
         df$scientificName != "Peromyscus ssp" |
         df$scientificName != "Plecotus E." |
         df$scientificName != "Rattus cf" |
         df$scientificName != "Redunca C." |
         df$scientificName != "Sorex Linnaeus" |
         df$scientificName != "Otomys F." |
         df$scientificName != "Oligoryzomys b" |
         df$scientificName != "Beremendia Kormos" |
         df$scientificName != "Tatera cf" |
         df$scientificName != "Sylvilagus sp" |
         df$scientificName != "Cervidae Gray" |
         df$scientificName != "Kobus A." |
         df$scientificName != "Beremendia Kormos" |
         df$scientificName != "Megacricetodon (Fahlbusch",]

df$scientificName[df$scientificName == "Loxodonta Africana"] <- "Loxodonta africana"

df$scientificName[df$scientificName == "Reithrodontomys humilus"] <- "Reithrodontomys humulis"

df$scientificName[df$scientificName == "Reithrodontomys megalotus"] <- "Reithrodontomys megalotis"

df$scientificName[df$scientificName == "Tatera robustas"] <-"Tatera robustus"

df$scientificName[df$scientificName == "Zapus hudsonicus"] <- "Zapus hudsonius"

df$scientificName[df$scientificName == "Synaptomys coopari"] <- "Synaptomys cooperi"

df$scientificName[df$scientificName == "Urocyon cinereogenteus"] <- "Urocyon cinereoargenteus"

df$scientificName[df$scientificName == "Tamias minmus"] <- "Tamias minimus"

df$scientificName[df$scientificName == "Tamias quadrivittatas"] <- "Tamias quadrivittatus"

df$scientificName[df$scientificName == "Tamiasciurus douglasi"] <- "Tamiasciurus douglasii"

df$scientificName[df$scientificName == "Tamiasciurus hudsonius" | 
                  df$scientificName == "Tamiasciurus husonicus"] <- "Tamiasciurus hudsonicus"

df$scientificName[df$scientificName == "Spermophilus parryi"] <- "Spermophilus parryii"

df$scientificName[df$scientificName == "Spermophilus richardsoni"] <- "Spermophilus richardsonii"

df$scientificName[df$scientificName == "Spermophilus tridecenlineatus"] <- "Spermophilus tridecemlineatus"

df$scientificName[df$scientificName == "Sylvilagus audoboni" | 
                  df$scientificName == "Sylvilagus auduboni"] <- "Sylvilagus audobonii"

df$scientificName[df$scientificName == "Sylvilagus cunicularis"] <- "Sylvilagus cunicularius"

df$scientificName[df$scientificName == "Sylvilagus nuttali" | 
                  df$scientificName == "Sylvilagus nuttalli"] <- "Sylvilagus nuttallii"

df$scientificName[df$scientificName == "Spilogale putoris"] <- "Spilogale putorius"

df$scientificName[df$scientificName == "Sciurus hudsonius"] <- "Sciurus hudsonicus"

df$scientificName[df$scientificName == "Scotophilus dingani"] <- "Scotophilus dinganii"

df$scientificName[df$scientificName == "Sivalhippus perimense"] <- "Sivalhippus perimensis"

df$scientificName[df$scientificName == "Sorex bendiri"] <- "Sorex bendirii"

df$scientificName[df$scientificName == "Sorex haydenii"] <- "Sorex haydeni"

df$scientificName[df$scientificName == "Reithrodontomys motanus"] <- "Reithrodontomys montanus"

df$scientificName[df$scientificName == "Rhinopoma hardwickei"] <- "Rhinopoma hardwickii"

df$scientificName[df$scientificName == "Pitymys qusiater"] <- "Pitymys quasiater"

df$scientificName[df$scientificName == "Peropteryx leucopterus"] <- "Peropteryx leucoptera"

df$scientificName[df$scientificName == "Peromyscus truei."] <- "Peromyscus truei"

df$scientificName[df$scientificName == "Pecari tejacu"] <- "Pecari tajacu"

df$scientificName[df$scientificName == "Peromyscus crinitis"] <- "Peromyscus crinitus"

df$scientificName[df$scientificName == "Perognathus penicllatus"] <- "Perognathus penicillatus"

df$scientificName[df$scientificName == "Peromyscus boylli"] <- "Peromyscus boylii"

df$scientificName[df$scientificName == "Pappogeomys castinops"] <- "Pappogeomys castanops"

df$scientificName[df$scientificName == "Neurotrichus gibbsi"] <- "Neurotrichus gibbsii"

df$scientificName[df$scientificName == "Oligoryzomys fulcescens" |
                  df$scientificName == "Oryzomys fuluescins" |
                  df$scientificName == "Oryzomys fulvescens"] <- "Oligoryzomys fulvescens"

df$scientificName[df$scientificName == "Onychomys luecogaster"] <- "Onychomys leucogaster"

df$scientificName[df$scientificName == "Ondatra zibethica"] <- "Ondatra zibethicus"

df$scientificName[df$scientificName == "Mazama gouazoupira"] <- "Mazama gouazoubira"

df$scientificName[df$scientificName == "Mastomys couchi"] <- "Mastomys coucha"

df$scientificName[df$scientificName == "Metachirus nudicausatus"] <- "Metachirus nudicaudatus"

df$scientificName[df$scientificName == "Microdipodops megalocephalus"] <- "Microdipodops megacephalus"

df$scientificName[df$scientificName == "Macaca nemestrina" |
                  df$scientificName == "Macaca nemestrinas"] <- "Macaca nemestrinus"

df$scientificName[df$scientificName == "Crocidura siberica"] <- "Crocidura sibirica"

df$scientificName[df$scientificName == "Lepus townsendi"] <- "Lepus townsendii"

df$scientificName[df$scientificName == "Heteromys gaumari"] <- "Heteromys gaumeri"

df$scientificName[df$scientificName == "Lasiurus cinerea"] <- "Lasiurus cinereus"

df$scientificName[df$scientificName == "Lasionycteris noctivagens"] <- "Lasionycteris noctivagans"

df$scientificName[df$scientificName == "Hydrochaeris hydrochaeris"] <- "Hydrochoeris hydrochaeris"

df$scientificName[df$scientificName == "Dipodomys heermani"] <- "Dipodomys heermanni"

df$scientificName[df$scientificName == "Dipodomys ordi"] <- "Dipodomys ordii"

df$scientificName[df$scientificName == "Dryomys nitidula"] <- "Dryomys nitedula"

df$scientificName[df$scientificName == "Cynopterus horsfieldi"] <- "Cynopterus horsfieldii"

df$scientificName[df$scientificName == "Dasypus novencinctus"] <- "Dasypus novemcinctus"

df$scientificName[df$scientificName == "Microtus townsendi"] <- "Microtus townsendii"

df$scientificName[df$scientificName == "Dipodomys deseri"] <- "Dipodomys deserti"

df$scientificName[df$scientificName == "Chinchilla laniqera"] <- "Chinchilla lanigera"

df$scientificName[df$scientificName == "Caluronmys lanatus"] <- "Caluromys lanatus"

df$scientificName[df$scientificName == "Cratogeomys castenops"] <- "Cratogeomys castanops"

df$scientificName[df$scientificName == "Chilonycteris parnellii"] <- "Chilonycteris parnelli"

df$scientificName[df$scientificName == "Chaerephon pumila"] <- "Chaerephon pumilus"

df$scientificName[df$scientificName == "Cavia tschudi"] <- "Cavia tschudii"

df$scientificName[df$scientificName == "Cervus elephas"] <- "Cervus elaphus"

df$scientificName[df$scientificName == "Carollia sowelii"] <- "Carollia sowelli"

df$scientificName[df$scientificName == "Blarina brevicada"] <- "Blarina bravicauda"

df$scientificName[df$scientificName == "Aonyx cinereus"] <- "Aonyx cinerea"

df$scientificName[df$scientificName == "Myotis daubentoni"] <- "Myotis daubentonii"

df$scientificName[df$scientificName == "Napeozapus insignis"] <- "Napaeozapus insignis"

df$scientificName[df$scientificName == "Neotoma floridana."] <- "Neotoma floridana"

df$scientificName[df$scientificName == "Neotoma cinera"] <- "Neotoma cinerea"

df$scientificName[df$scientificName == "Myotis nigracans"] <- "Myotis nigricans"

df$scientificName[df$scientificName == "Myotis bocagei"] <- "Myotis bocagii"

df$scientificName[df$scientificName == "Myotis keeni"] <- "Myotis keenii"

df$scientificName[df$scientificName == "Myotis brandti"] <- "Myotis brandtii"

df$scientificName[df$scientificName == "Myotis daubentoni"] <- "Myotis daubentonii"

df$scientificName[df$scientificName == "Octodon degu"] <- "Octodon degus"

df$scientificName[df$scientificName == "Neurotrichus gibbsi"] <- "Neurotrichus gibbsii"

df$scientificName[df$scientificName == "Ochrotomys nuttali"] <- "Ochrotomys nuttalli"

df$scientificName[df$scientificName == "Miniopterus schreibersi"] <- "Miniopterus schreibersii"

df$scientificName[df$scientificName == "Mustela vision"] <- "Mustela vison"

df$scientificName[df$scientificName == "Muntiacus reevsi"] <- "Muntiacus reevesi"

df$scientificName[df$scientificName == "Myodes rutilis"] <- "Myodes rutilus"

df$scientificName[df$scientificName == "Mimon bennetti"] <- "Mimon bennettii"

df$scientificName[df$scientificName == "Metachirus nudicausatus"] <- "Metachirus nudicaudatus"

df$scientificName[df$scientificName == "Microtus pennsylavnicus"] <- "Microtus pennsylvanicus"

df$scientificName[df$scientificName == "MIcrotus ochrogaster"] <- "Microtus ochrogaster"

df$scientificName[df$scientificName == "Microtus richardson"] <- "Microtus richardsoni"

length(unique(df$scientificName)) #3164
nrow(df) #2252702

#change known individualID for kitty's data (proj 282)
df$catalogNumber[df$catalogNumber == "--" &
                 df$projectID == "https://geome-db.org/workbench/project-overview?projectId=282"] <- "EAP1"
df$individualID[df$projectID == "https://geome-db.org/workbench/project-overview?projectId=282"] <- df$catalogNumber[df$projectID == "https://geome-db.org/workbench/project-overview?projectId=282"]

## write out data
write.csv(df, "futres.trim1.csv")

#### Figure 1 panel 1: lifeStage ----
df.fig1 <- df
length(df.fig1$measurementValue[df.fig1$scientificName == "Peromyscus maniculatus" & 
                                df.fig1$measurementType == "body mass" &
                                !is.na(df.fig1$measurementValue)]) #31759
length(df.fig1$measurementValue[df.fig1$scientificName == "Otospermophilus beecheyi" & 
                                df.fig1$measurementType == "body mass" &
                                !is.na(df.fig1$measurementValue)]) #233

#care about estimated and lifeStage
#inferred value = assumed what units the value was in; this has the highest error
#want to flag inferred value as "value inferred" so that we can omit from determining upper and lower limits
#estimated value = made assumptions about which part of the string was the trait value

df.fig1$cat <- paste(df.fig1$lifeStage, df.fig1$measurementMethod)
unique(df.fig1$cat)

df.fig1$cat[df.fig1$cat == "Not Collected Unknown" |
            df.fig1$cat == " Unknown" |
            df.fig1$cat == "lifeStage measurementMethod" |
            df.fig1$cat == "Not Collected Extracted with Traiter" |
            df.fig1$cat == "Not Collected Extracted with Traiter ; estimated value"] <- "No stage; value possibly good" #lightgoldenrod3

df.fig1$cat[df.fig1$cat == "adult Unknown" |
            df.fig1$cat == "adult Extracted with Traiter" |
            df.fig1$cat == "adult Extracted with Traiter ; estimated value"] <- "Adult; value possibly good" #darkorchid4

df.fig1$cat[df.fig1$cat == "juvenile Unknown" |
            df.fig1$cat == "juvenile Extracted with Traiter" |
            df.fig1$cat == "juvenile Extracted with Traiter ; estimated value"] <- "Juvenile; value possibly good" #gray74

df.fig1$cat[df.fig1$cat == "Not Collected Extracted with Traiter ; estimated value; inferred value"| 
            df.fig1$cat == "Not Collected Extracted with Traiter ; inferred value"] <- "No stage; value inferred" #lightgoldenrod1

df.fig1$cat[df.fig1$cat == "adult Extracted with Traiter ; estimated value; inferred value" | 
            df.fig1$cat == "adult Extracted with Traiter ; inferred value"] <- "Adult; value inferred" #darkorchid

df.fig1$cat[df.fig1$cat == "juvenile Extracted with Traiter ; inferred value" |
            df.fig1$cat == "juvenile Extracted with Traiter ; estimated value; inferred value"] <- "Juvenile; value inferred" #gray74

df.fig1$cat <- as.factor(df.fig1$cat)
df.fig1$cat = relevel(df.fig1$cat, "Adult; value possibly good")
df.fig1$cat <- factor(df.fig1$cat, levels = c("Adult; value possibly good", "Adult; value inferred", 
                                              "Juvenile; value possibly good", "Juvenile; value inferred",
                                              "No stage; value possibly good", "No stage; value inferred"))

df.pema1 <- subset(df.fig1, df.fig1$scientificName == "Peromyscus maniculatus" & 
                   df.fig1$measurementType == "body mass" & 
                   !is.na(df.fig1$measurementValue))
length(df.pema1$measurementValue) #31759
unique(df.pema1$cat)
length(df.pema1$cat[df.pema1$cat == "Adult; value possibly good"]) #2951
length(df.pema1$cat[df.pema1$cat == "Juvenile; value possibly good"]) #920
length(df.pema1$cat[df.pema1$cat == "No stage; value possibly good"]) #21795
length(df.pema1$cat[df.pema1$cat == "No stage; value inferred"]) #5992
length(df.pema1$cat[df.pema1$cat == "Adult; value inferred"]) #70
length(df.pema1$cat[df.pema1$cat == "Juvenile; value inferred"]) #31
p.pema1 <- ggplot() + 
                  geom_density(aes(x = df.pema1$measurementValue, fill = df.pema1$cat), alpha = 0.6) +
                  scale_fill_manual(values = c("darkorchid4", "darkorchid", "gray74", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                                    name="Data Quality Category") +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                  ggtitle("Peromyscus maniculatus N = 31759") +
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
length(df.otbe1$cat[df.otbe1$cat == "Adult; value possibly good"]) #27
length(df.otbe1$cat[df.otbe1$cat == "Juvenile; value possibly good"]) #9
length(df.otbe1$cat[df.otbe1$cat == "No stage; value possibly good"]) #138
length(df.otbe1$cat[df.otbe1$cat == "No stage; value inferred"]) #56
length(df.otbe1$cat[df.otbe1$cat == "Adult; value inferred"]) #1
length(df.otbe1$cat[df.otbe1$cat == "Juvenile; value inferred"]) #2
p.otbe1 <- ggplot() + 
           geom_density(aes(x = df.otbe1$measurementValue, fill = df.otbe1$cat), alpha = 0.4) +
           scale_fill_manual(values = c("darkorchid4", "darkorchid", "gray74", "gray74", "lightgoldenrod3", "lightgoldenrod1"), 
                             name="Data Quality Category") +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")) +
           ggtitle("Otospermophilus beecheyi N = 233") +
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
length(sp) # 3164
nrow(df.test) #2252702

df.test$measurementStatus <- ""

##maha function from https://cran.r-project.org/src/contrib/Archive/OutlierDetection/
setwd("~/GitHub/futres/Best-Practices-Paper/scripts/OutlierDetection/R")
source("Maha.R")

inferred <- c("Extracted with Traiter ; inferred value", "Extracted with Traiter ; estimated value; inferred value")
  
##test with P. maniculatus
pema <- subset(df.test, subset = df.test[,"scientificName"] == "Peromyscus maniculatus" &
               df.test[,"measurementType"] == "body mass" & 
               df.test[,"lifeStage"] == "adult" &
               !(df.test[, "measurementMethod"] %in% inferred), 
               select = "measurementValue") %>%
  mutate_at("measurementValue", as.numeric)%>%
  drop_na()
  
outlier.pema <- maha(pema, cutoff = 0.95, rnames = FALSE)

##now for all:

#mass
for(i in 1:length(sp)){
  sub <- subset(df.test, subset = df.test[,"scientificName"] == sp[i] &
                df.test[,"measurementType"] == "body mass" & 
                df.test[,"lifeStage"] == "adult" &
                !(df.test[, "measurementMethod"] %in% inferred),
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
 
  else if(isTRUE(nrow(sub) < 10)){
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
                df.test[,"lifeStage"] == "adult" &
                !(df.test[, "measurementMethod"] %in% inferred),
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
                df.test[,"lifeStage"] == "adult" &
                !(df.test[, "measurementMethod"] %in% inferred), 
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
length(unique(df.test$scientificName)) #3164; same as df
nrow(df.test) #2252702; same as df
write.csv(df.test, "mh.outlier.flagged.data.csv")

##Figure 1, panel 2: outliers----
df.fig2 <- df.test[df.test$lifeStage != "juvenile" & 
                   df.test$measurementStatus != "too few records",]

df.fig2$cat <- paste(df.fig2$lifeStage, df.fig2$measurementStatus)
unique(df.fig2$cat)
df.fig2$cat[df.fig2$cat == "adult "] <- "Adult; possibly good" #darkorchid4
df.fig2$cat[df.fig2$cat == "adult outlier"] <- "Adult; outlier" #darkorchid1
df.fig2$cat[df.fig2$cat == "Not Collected " | 
            df.fig2$cat == " "] <- "No stage; untested" #lightgoldenrod1
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
length(df.pema2$measurementType) #30808
unique(df.pema2$cat)
length(df.pema2$cat[df.pema2$cat == "Adult; possibly good"]) #3020
length(df.pema2$cat[df.pema2$cat == "Adult; outlier"]) #1
length(df.pema2$cat[df.pema2$cat == "No stage; untested"]) #27787 
p.pema2 <- ggplot() + 
           geom_density(data = filter(df.pema2, measurementStatus == "outlier"), aes(x = measurementValue), color = NA, alpha = 0.4) + 
           geom_rug(data = filter(df.pema2, measurementStatus == "outlier"), aes(x = measurementValue), sides = "b", col = "gray34") +
           geom_density(data = df.pema2, aes(x = measurementValue, fill = cat), alpha = 0.4) +
           scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod1"),
                             name = "Data Quality Category") +
           ggtitle("Peromyscus maniculatus N = 30808, Noutlier = 1") +
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
                    !(df.norm$measurementMethod %in% inferred)) %>%
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
                                    !(df.norm$measurementMethod %in% inferred)))
  sub <- sub %>%
    drop_na(measurementValue)
  
  if(isTRUE(length(sub$measurementValue[sub$measurementType == "body mass"]) < 3 |
            length(unique(sub$measurementValue[sub$measurementType == "body mass"])) < 3)){
    df.norm$measurementStatus[df.norm$scientificName == sp[i] & 
                              df.norm$measurementType == "body mass" &
                              df.norm$measurementStatus != "outlier" &
                              df.norm$measurementStatus != "too few records" &
                              df.norm$lifeStage == "adult" &
                              !(df.norm$measurementMethod %in% inferred)] <- "too few records"
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
                          !(df.norm$measurementMethod %in% inferred)] <- "non-normal"
      }
      else if(isTRUE(normal.total.length[[2]] >= 0.05)){
        df.norm$normality[df.norm$measurementType == "body mass" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          !(df.norm$measurementMethod %in% inferred)] <- "normal"
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
                          !(df.norm$measurementMethod %in% inferred)] <- "non-normal"
      }
      else if(isTRUE(normal.mass[[2]] >= 0.05)){
        df.norm$normality[df.norm$measurementType == "body mass" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          !(df.norm$measurementMethod %in% inferred)] <- "normal"
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
                                      !(df.norm$measurementMethod %in% inferred)))
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
                              !(df.norm$measurementMethod %in% inferred)] <- "too few records"
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
                        !(df.norm$measurementMethod %in% inferred)] <- "non-normal"
    }
    else if(isTRUE(normal.total.length[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "body length"  |
                        df.norm$measurementType == "body length with tail" & 
                        df.norm$scientificName == sp[i] &
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        !(df.norm$measurementMethod %in% inferred)] <- "normal"
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
                        !(df.norm$measurementMethod %in% inferred)] <- "non-normal"
    }
    else if(isTRUE(normal.total.length[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "body length"  |
                        df.norm$measurementType == "body length with tail"& 
                        df.norm$scientificName == sp[i] &
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        !(df.norm$measurementMethod %in% inferred)] <- "normal"
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
                                    !(df.norm$measurementMethod %in% inferred)))
  sub <- sub %>%
    drop_na(measurementValue)
  
  if(isTRUE(length(sub$measurementValue[sub$measurementType == "tail length"]) < 3 |
            length(unique(sub$measurementValue[sub$measurementType == "tail length"])) < 3)){
    df.norm$measurementStatus[df.norm$scientificName == sp[i] & 
                              df.norm$measurementType == "tail length" &
                              df.norm$measurementStatus != "outlier" &
                              df.norm$measurementStatus != "too few records" &
                              df.norm$lifeStage == "adult" &
                              !(df.norm$measurementMethod %in% inferred)] <- "too few records"
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
                          !(df.norm$measurementMethod %in% inferred)] <- "non-normal"
    }
    else if(isTRUE(normal.total.length[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "tail length" & 
                          df.norm$scientificName == sp[i] &
                          df.norm$measurementStatus != "outlier" &
                          df.norm$measurementStatus != "too few records" &
                          df.norm$lifeStage == "adult" &
                          !(df.norm$measurementMethod %in% inferred)] <- "normal"
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
                        !(df.norm$measurementMethod %in% inferred)] <- "non-normal"
    }
    else if(isTRUE(normal.tail[[2]] >= 0.05)){
      df.norm$normality[df.norm$measurementType == "tail length" & 
                        df.norm$scientificName == sp[i]&
                        df.norm$measurementStatus != "outlier" &
                        df.norm$measurementStatus != "too few records" &
                        df.norm$lifeStage == "adult" &
                        !(df.norm$measurementMethod %in% inferred)] <- "normal"
    }
  }
  
  else{
    next
  }
}

#make sure no data was lost
nrow(df.norm) #2252702

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
                                              !(df.transform$measurementMethod %in% inferred) &
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
                                                          !(df.transform$measurementMethod %in% inferred) &
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
                                         !(df.transform$measurementMethod %in% inferred) &
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
                                   !(df.transform$measurementMethod %in% inferred) &
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
                             !(df.transform$measurementMethod %in% inferred) &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body mass" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             !(df.transform$measurementMethod %in% inferred) &
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
                               !(df.transform$measurementMethod %in% inferred) &
                               df.transform$normality == "non-normal"] <- "log normal"
      }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body mass" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             !(df.transform$measurementMethod %in% inferred) &
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
                                         !(df.transform$measurementMethod %in% inferred) &
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
                                   !(df.transform$measurementMethod %in% inferred) &
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
                             !(df.transform$measurementMethod %in% inferred) &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body length" |
                             df.transform$measurementType == "body length without tail" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             !(df.transform$measurementMethod %in% inferred) &
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
                             !(df.transform$measurementMethod %in% inferred) &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "body length" |
                             df.transform$measurementType == "body length without tail" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             !(df.transform$measurementMethod %in% inferred) &
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
                                         !(df.transform$measurementMethod %in% inferred) &
                                         df.transform$normality == "non-normal"))
  sub <- sub %>%
    drop_na(logMeasurementValue)
  
  if(isTRUE(length(sub$measurementValue[sub$measurementType == "tail length"]) < 3)){
    df.transform$measurementStatus[df.transform$scientificName == sp.transform[i] & 
                                   df.transform$measurementType == "tail length" & 
                                   df.transform$measurementStatus != "outlier" &
                                   df.transform$measurementStatus != "too few records" &
                                   df.transform$lifeStage == "adult" &
                                   !(df.transform$measurementMethod %in% inferred) &
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
                             !(df.transform$measurementMethod %in% inferred) &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "tail length" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             !(df.transform$measurementMethod %in% inferred) &
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
                             !(df.transform$measurementMethod %in% inferred) &
                             df.transform$normality == "non-normal"] <- "log normal"
    }
    else if(isTRUE(log.normal.mass[[2]] <= 0.05)){
      df.transform$normality[df.transform$scientificName == sp.transform[i] & 
                             df.transform$measurementType == "tail length" & 
                             df.transform$measurementStatus != "outlier" &
                             df.transform$measurementStatus != "too few records" &
                             df.transform$lifeStage == "adult" &
                             !(df.transform$measurementMethod %in% inferred) &
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
                                           !(df.quant$measurementMethod %in% inferred) &
                                           df.quant$normality == "non-log.normal")


df.pema.quant <- df.pema.quant %>%
  drop_na(measurementValue)
quant.pema.mass <- quantile(df.pema.quant$measurementValue, 
                            probs = seq(0,1,.05))
quant.pema.mass[[2]] #5%
quant.pema.mass[[20]] #95%

df.pema.quant$lowerLimit <- quant.pema.mass[[2]]
df.pema.quant$upperLimit <- quant.pema.mass[[20]]

df.pema.quant$measurementStatus[df.pema.quant$measurementValue < df.pema.quant$lowerLimit] <- "possible juvenile"
df.pema.quant$measurementStatus[df.pema.quant$measurementValue > df.pema.quant$upperLimit] <- "outlier"
df.pema.quant$measurementStatus[df.pema.quant$measurementValue <= df.pema.quant$upperLimit &
                                df.pema.quant$measurementValue >= df.pema.quant$lowerLimit] <- "possible adult; possibly good"


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
                                   !(df.quant$measurementMethod %in% inferred) &
                                   df.quant$normality == "non-log.normal") #non-normal was overridden
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
head(unique(df.quant$lowerLimit[df.quant$normality == "non-log.normal"])) #should have values

#total length
for(i in 1:length(sp)){
  sub <- subset(df.quant, subset = df.quant$scientificName == sp[i] &
                df.quant$measurementType == "body length" |
                df.quant$measurementType == "body length with tail" &
                df.quant$lifeStage == "adult" &
                df.quant$measurementStatus != "outlier" &
                df.quant$measurementStatus != "too few records" &
                !(df.quant$measurementMethod %in% inferred) &
                df.quant$normality == "non-log.normal") #non-normal was overridden
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
                !(df.quant$measurementMethod %in% inferred) &
                df.quant$normality == "non-log.normal") #non-normal was overridden
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
                           df.quant$lifeStage != "juvenile"] <- "possible juvenile"

unique(df.quant$measurementStatus)
unique(df.quant$measurementStatus[df.quant$measurementValue < df.quant$lowerLimit &
                                    df.quant$measurementStatus != "outlier" &
                                    df.quant$lowerLimitMethod == "quantile adults, non-estimated values, no outliers" &
                                    df.quant$measurementStatus != "too few records" &
                                    df.quant$lifeStage != "juvenile" &
                                    df.quant$normality == "non-log.normal"])

df.quant$measurementStatus[df.quant$measurementValue > df.quant$upperLimit &
                           df.quant$upperLimitMethod == "quantile adults, non-estimated values, no outliers" &
                           df.quant$measurementStatus != "outlier" &
                           df.quant$measurementStatus != "too few records" &
                           df.quant$lifeStage != "juvenile"] <- "outlier"

df.quant$measurementStatus[df.quant$measurementValue >= df.quant$lowerLimit &
                           df.quant$measurementValue <= df.quant$upperLimit  &
                           df.quant$lowerLimitMethod == "quantile adults, non-estimated values, no outliers" &
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
                             !(limit.test$measurementMethod %in% inferred) &
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
                                   !(df.sigma$measurementMethod %in% inferred) &
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
head(df.sigma$lowerLimit[df.sigma$normality != "normal" &
                         !(is.na(df.sigma$lowerLimit))]) #should be "" or "sigma..."
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
                  !(df.sigma$measurementMethod %in% inferred) &
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
                  !(df.sigma$measurementMethod %in% inferred) &
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
                           df.sigma$lifeStage != "juvenile"] <- "possible juvenile"

df.sigma$measurementStatus[df.sigma$measurementValue > df.sigma$upperLimit &
                           df.sigma$upperLimitMethod == "sd" &
                           df.sigma$measurementStatus != "outlier" &
                           df.sigma$measurementStatus != "too few records" &
                           df.sigma$lifeStage != "juvenile"] <- "outlier"

df.sigma$measurementStatus[df.sigma$measurementValue >= df.sigma$lowerLimit &
                           df.sigma$measurementValue <= df.sigma$upperLimit  &
                           df.sigma$lowerLimitMethod == "sd" &
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
                  !(df.logSigma$measurementMethod %in% inferred) &
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
head(df.logSigma$lowerLimit[df.logSigma$normality != "log normal" &
                            !is.na(df.logSigma$lowerLimit)]) #should be "" or "sigma..."
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
                !(df.logSigma$measurementMethod %in% inferred) &
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
                  !(df.logSigma$measurementMethod %in% inferred) &
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
                              df.logSigma$lifeStage != "juvenile"] <- "possible juvenile"

df.logSigma$measurementStatus[df.logSigma$logMeasurementValue > df.logSigma$upperLimit &
                              df.logSigma$upperLimitMethod == "log sd" &
                              df.logSigma$measurementStatus != "outlier" &
                              df.logSigma$measurementStatus != "too few records" &
                              df.logSigma$lifeStage != "juvenile"] <- "outlier"

df.logSigma$measurementStatus[df.logSigma$logMeasurementValue >= df.logSigma$lowerLimit &
                              df.logSigma$logMeasurementValue <= df.logSigma$upperLimit  &
                              df.logSigma$lowerLimitMethod == "log sd" &
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
            df.fig3$cat == "adult possible juvenile"] <- "Adult; outlier" #darkorchid1
df.fig3$cat[df.fig3$cat == "adult too few records" |
            df.fig3$cat == "adult less than 10 records"] <- "Adult; too few records" #gray74
df.fig3$cat[df.fig3$cat == "adult "] <- "Adult; untested" #darkorchid
df.fig3$cat[df.fig3$cat == "Not Collected possible adult; possibly good"] <- "No stage; possibly good" #lightgoldenrod3
df.fig3$cat[df.fig3$cat == "Not Collected outlier" |
            df.fig3$cat == "Not Collected possible juvenile"] <- "No stage; outlier" #lightgoldenrodyellow
df.fig3$cat[df.fig3$cat == "Not Collected too few records" |
            df.fig3$cat == "Not Collected less than 10 records"] <- "No stage; too few records" #gray74
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
length(df.pema3$measurementValue) #30808
unique(df.pema3$cat)
length(df.pema3$cat[df.pema3$cat == "Adult; possibly good"]) #2773
length(df.pema3$cat[df.pema3$cat == "Adult; outlier"]) #248
length(df.pema3$cat[df.pema3$cat == "No stage; possibly good"]) #24343
length(df.pema3$cat[df.pema3$cat == "No stage; outlier"]) #3444

outlier <- c("outlier", "possible juvenile",
             "too few records", "less than 10 records")

p.pema3 <- ggplot() + 
  geom_density(data = filter(df.pema3, measurementStatus %in% outlier), aes(x = measurementValue), color = NA, alpha = 0.4) + 
  geom_rug(data = filter(df.pema3, measurementStatus %in% outlier), aes(x = measurementValue), sides = "b", col = "gray34") +
  geom_density(data = df.pema3, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod3", "lightgoldenrodyellow"),
                    name = "Data Quality Category") +
  ggtitle("Peromyscus maniculatus N = 30808, Noutlier = 3669") +
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
length(df.otbe3$cat[df.otbe3$cat == "No stage; outlier"]) #7
length(df.otbe3$cat[df.otbe3$cat == "No stage; possibly good"]) #187
length(df.otbe3$cat[df.otbe3$cat == "Adult; possibly good"]) #27
length(df.otbe3$cat[df.otbe3$cat == "Adult; outlier"]) #1
p.otbe3 <- ggplot() + 
  geom_density(data = filter(df.otbe3, measurementStatus %in% outlier), aes(x = measurementValue), color = NA, alpha = 0.4) +
  geom_rug(data = filter(df.otbe3, measurementStatus %in% outlier), aes(x = measurementValue), sides = "b", col = "gray34") +
  geom_density(data = df.otbe3, aes(x = measurementValue, fill = cat), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid4", "darkorchid1", "lightgoldenrod3", "lightgoldenrodyellow"),
                    name = "Data Quality Category") +
  ggtitle("Otospermophilus beecheyi N = 222, Noutlier = 8") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Body Mass (g)", limits = c(0, 1500)) +
  scale_y_continuous(name = "Density", limits = c(0, .0125))
ggsave(p.otbe3, file=paste0("check.test.squirrel",".png"), width = 14, height = 10, units = "cm")

##info about outliers----
outlier <- c("possible juvenile", "outlier",
             "too few records", "less than 10 records")
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

sum(outlier_stats$sample.outlier.mass) #56993


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

write.csv(df.stats, "df.stats.csv")

#### FINAL DATA ----

write.csv(df.logSigma, "BPP.data.csv")



