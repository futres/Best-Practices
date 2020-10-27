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
library(tidyr)

##Upload data----
pan <- read.csv("https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("https://de.cyverse.org/dl/d/74880F82-59DF-4558-BBCB-A51EA1631592/labeled.clean.data.csv", header = TRUE)
#data <- data.outlier
data <- data[data$mass.status == "GOOD" & data$mass.units == "g" & !is.na(data$mass),]
length(unique(data$scientificName)) #869

###head-body-length----
data$head.body.length <- data$total.length - data$tail.length
# need to get bunnies and deer

#pan.data$head.body.length[pan.data$MSW05_Family == "Cervidae"] <- pan.data$total.length[pan.data$MSW05_Family == "Cervidae"]
#pan.data$head.body.length[pan.data$MSW05_Family == "Leporidae"] <- pan.data$total.length[pan.data$MSW05_Family == "Leporidae"]
#says NA not allowed for subscripts and can't get it working...

for(i in 1:nrow(data)){
  if(is.na(data$head.body.length[data$MSW05_Family == "Cervidae"][i])){
    data$head.body.length[i] <- data$total.length[i]
  }
  else if(isTRUE(data$total.length[data$MSW05_Family == "Cervidae"][i] >= 0)){
    data$head.body.length[i] <- data$total.length[i]
  }
  else{
    next
  }
}

for(i in 1:nrow(data)){
  if(is.na(data$head.body.length[data$MSW05_Family == "Leporidae"][i])){
    data$head.body.length[i] <- data$total.length[i]
  }
  else if(isTRUE(data$total.length[data$MSW05_Family == "Leporidae"][i] >= 0)){
    data$head.body.length[i] <- data$total.length[i]
  }
  else{
    next
  }
}

##combine with pantheria----
sp.data <- unique(data$scientificName) 
nrow(pan[pan$MSW05_Binomial %in% sp.data,]) #883 spp

pan.data <- merge(pan, data, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = TRUE, all.x = FALSE)
length(unique(pan.data$MSW05_Binomial)) #1020

write.csv(pan.data, "pan.data.csv")
write.csv(pan.data, "data.for.analyses.csv")

##data cleaned for mass----
#pan.data <- read.csv("", header = TRUE)

pan.data.mass.sample <- pan.data %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size.mass.pan = length(mass[!is.na(mass)])) %>%
  as.data.frame()

pan.keep <- pan.data.mass.sample$MSW05_Binomial[pan.data.mass.sample$sample.size.mass.pan >= 10]
pan.data.10 <- pan.data[pan.data$MSW05_Binomial %in% pan.keep,]
pan.data.mass <- pan.data.10[!is.na(pan.data.10$X5.1_AdultBodyMass_g) & pan.data.10$X5.1_AdultBodyMass_g > 0 & pan.data.10$mass > 0,]

##Q1 compare to pantheria----
length(unique(pan.data.mass$MSW05_Binomial)) #690

pan.adult_stats <- pan.data.mass %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size = length(mass), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.err.mass = sd(mass, na.rm = TRUE)/sqrt(sample.size),
                   pan.mass = X5.1_AdultBodyMass_g[1],
                   mass.diff = (pan.mass - avg.mass) / sd.err.mass,
                   abs.mass.diff = abs((pan.mass - avg.mass) / sd.err.mass)) %>%
  as.data.frame()
write.csv(pan.adult_stats, "pan.results.csv")

plot(x = log10(pan.adult_stats$avg.mass), y = log10(pan.adult_stats$sd.err.mass),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Standard Error of Mass",
     title(main = "Relationship between standard error and average mass",
           sub = "slope = 1.16, p<0.001"))
model <- lm(log10(pan.adult_stats$sd.err.mass) ~ log10(pan.adult_stats$avg.mass))
summary(model)

plot(x = pan.adult_stats$sample.size, y = pan.adult_stats$abs.mass.diff,
     xlab = "Sample Size",
     ylab = "Absolute Difference between Pan Mass and Avg. Mass",
     title(main = "Relationship between Sample Size on Mass differences",
           sub = "slope = 0, p=0.9"))
model3 <- lm(pan.adult_stats$abs.mass.diff ~ pan.adult_stats$sample.size + pan.adult_stats$avg.mass)
summary(model3)
model4 <- lm(pan.adult_stats$abs.mass.diff ~ pan.adult_stats$sample.size + pan.adult_stats$sd.err.mass)
summary(model4)

plot(x = log10(pan.adult_stats$avg.mass), y = log10(pan.adult_stats$mass.diff),
     xlab = "Log10 Average Mass (g)",
     ylab = "Log10 Mass Difference",
     title(main = "Relationship between animal size and degree of difference between masses",
           sub = "slope = 0.003, p=0.925"))
model2 <- lm(log10(pan.adult_stats$mass.diff) ~ log10(pan.adult_stats$avg.mass))
summary(model2)

pan.adult_stats2 <- pan.adult_stats[pan.adult_stats$MSW05_Binomial != "Stenella longirostris" &
                                      pan.adult_stats$MSW05_Binomial != "Catagonus wagneri" &
                                      pan.adult_stats$MSW05_Binomial != "Alces alces" & 
                                      pan.adult_stats$MSW05_Binomial != "Hylobates lar" &
                                      pan.adult_stats$MSW05_Binomial != "Arctocephalus townsendi" &
                                      pan.adult_stats$MSW05_Binomial != "Lepus arcticus" &
                                      pan.adult_stats$MSW05_Binomial != "Enhydra lutris",] 

##STOPPED HERE----
nrow(pan.adult_stats2) #683
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3]) #182 26.6%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3]) #501; 73.4%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3]) #420 61.5%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3]) #81; 11.9%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 100]) #504 (73.8% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 100]) #130; 25.8%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 100]) #374; 74.2%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 100]) #311; 61.7%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 100]) #63; 12.5%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 1000 & pan.adult_stats2$avg.mass > 100]) #108 (15.8% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #40; 37.0%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #68; 63.0%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #60; 55.6%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 1000  & pan.adult_stats2$avg.mass > 100]) #8; 7.4%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) #34 (5.0% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) #6; 17.6%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 10000 & pan.adult_stats2$avg.mass > 1000]) #28; 82.4%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 10000  & pan.adult_stats2$avg.mass > 1000]) #27; 79.4%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 10000  & pan.adult_stats2$avg.mass > 1000]) #1; 2.9%

length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) #19 (2.8% of total spp)
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff >= -3 & pan.adult_stats2$mass.diff <= 3 & pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) #1; 5.3%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$abs.mass.diff > 3 & pan.adult_stats2$avg.mass < 100000 & pan.adult_stats2$avg.mass > 10000]) #18; 94.7%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff > 3 & pan.adult_stats2$avg.mass < 100000  & pan.adult_stats2$avg.mass > 10000]) #17; 89.5%
length(pan.adult_stats2$MSW05_Binomial[pan.adult_stats2$mass.diff < -3 & pan.adult_stats2$avg.mass < 100000  & pan.adult_stats2$avg.mass > 10000]) #1; 5.3%

##FIGURE: mass difference
p <- ggplot(data = pan.adult_stats2, aes(x = mass.diff)) +
  geom_density(col = "slateblue4") +
  geom_rug(sides = "b", col = "slateblue4") +
  ggtitle("Difference in Mean Mass (PanTHERIA) to Mean Mass (this paper)") + 
  scale_x_continuous(name = 'Standard Deviations from Mean') + 
  scale_y_continuous(name = 'Probability') + 
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", col = "darkgray") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(p, file=paste0("diff.mass", ".png"), width = 14, height = 10, units = "cm")

##FIGURE: body mass distributions w/ line from PanTHERIA----

pan.data.stats <- pan.data.mass[pan.data.mass$MSW05_Binomial %in% pan.adult_stats2$MSW05_Binomial,] #222

uniq_species <- unique(pan.data.stats$MSW05_Binomial)
for (i in uniq_species) {
  p = ggplot(data = subset(pan.adult.clean.10, MSW05_Binomial == i)) + 
    geom_density(aes(log10(mass)), fill = "slateblue4") +
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_continuous(name = 'Probability') + 
    geom_vline(xintercept = log10(pan.adult.clean.10$X5.1_AdultBodyMass_g[pan.adult.clean.10$MSW05_Binomial == i][1]))
  ggsave(p, file=paste0("dist_", i,".png"), width = 14, height = 10, units = "cm")
}

##Q2: length v. mass----
#clean data
pan.data.mass.sample <- pan.data %>%
  dplyr::group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size.mass.pan = length(mass[!is.na(mass)])) %>%
  as.data.frame()

pan.keep <- pan.data.mass.sample$MSW05_Binomial[pan.data.mass.sample$sample.size.mass.pan >= 10]
pan.data.10 <- pan.data[pan.data$MSW05_Binomial %in% pan.keep,]
pan.data.mass <- pan.data.10[!is.na(pan.data.10$X5.1_AdultBodyMass_g) & pan.data.10$X5.1_AdultBodyMass_g > 0 & pan.data.10$mass > 0,]



pan.data.mass <- pan.data[!is.na(pan.data$mass) & pan.data$mass.status != "outlier",]
pan.data.mass.length <- pan.data.mass[!is.na(pan.data.mass$total.length) & pan.data.mass$total.length.status != "outlier",]
pan.data.adult <- pan.data.mass.length[pan.data.mass.length$lifeStage != "Juvenile",]
length(unique(pan.data.adult$MSW05_Binomial)) #1191sp

##some measurements are zero, oy vey
pan.data.adult.clean <- pan.data.adult[pan.data.adult$mass > 0 & pan.data.adult$head.body.length > 0,]
length(unique(pan.data.adult.clean$MSW05_Binomial)) #1191

#recount sample sizes
pan.data.adult_stats <- pan.data.adult.clean %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(counts = n())

keep.10 <- pan.data.adult_stats$MSW05_Binomial[pan.data.adult_stats$counts >= 10] #286 sp
pan.data.adult.10 <- pan.data.adult.clean[pan.data.adult.clean$MSW05_Binomial %in% keep.10,]
length(unique(pan.data.adult.10$MSW05_Binomial)) #872 sp

sp.models <- unique(pan.data.adult.10$MSW05_Binomial)
model.results.species <- data.frame()
for(i in 1:length(sp.models)){
  sub.data <- as.data.frame(pan.data.adult.10[pan.data.adult.10$MSW05_Binomial == sp.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$head.body.length), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = sub.data$MSW05_Binomial[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(sub.data$mass))
  model.results.species <- rbind(model.results.species, sub)
}

#lots of repeats for Neofiber alleni; only 4 unique occurrence ids....why???

#write.csv(model.results.species, "model.results.species.csv")

uniq_species <- unique(pan.data.adult.10$MSW05_Binomial)
for (i in uniq_species) {
  p = ggplot(data = subset(pan.data.adult.10, MSW05_Binomial  == i)) + 
    geom_point(aes(x = log10(mass), y = log10(head.body.length))) +
    geom_smooth(aes(x = log10(mass), y = log10(head.body.length)),
                method = "lm", color = "slateblue4")
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_log10(name = expression(log[10]~Head~Body~Length~(mm))) + 
  ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}


# data.adult.10$infer.type <- rep("", nrow(data.adult.10))
# for(i in 1:nrow(data.adult.10)){
#   if(isTRUE(data.adult.10$total.length.units.inferred[i] ==" TRUE" | data.adult.10$total.length.units.inferred[i] == "True" | data.adult.10$total.length.units.inferred[i] == "CONVERTED" & data.adult.10$mass.units.inferred[i] == "TRUE" | data.adult.10$mass.units.inferred[i] == "True" | data.adult.10$mass.units.inferred[i] == "CONVERTED")){
#     data.adult.10$infer.type[i] <- "both"
#   }
#   else if(isTRUE(data.adult.10$total.length.units.inferred[i] ==" TRUE" | data.adult.10$total.length.units.inferred[i] == "True" | data.adult.10$total.length.units.inferred[i] == "CONVERTED")){
#     data.adult.10$infer.type[i] <- "length"
#   }
#   else if(isTRUE(data.adult.10$mass.units.inferred[i] == "TRUE" | data.adult.10$mass.units.inferred[i] == "True" | data.adult.10$mass.units.inferred[i] == "CONVERTED")){
#     data.adult.10$infer.type[i] <- "mass"
#   }
#   else{
#     data.adult.10$infer.type[i] <- "none"
#   }
# }
# 
# ggplot(data = subset(data.adult.10, scientificName  == "Artibeus jamaicensis")) + 
#     geom_point(aes(x = log10(mass), y = log10(total.length), color = infer.type)) +
#     geom_smooth(aes(x = log10(mass), y = log10(total.length)),
#                 method = "lm", color = "slateblue4")+
#     scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
#     scale_y_log10(name = expression(log[10]~Total~Length~(mm)))

#WEIRD ONES: 
#Akodon mimus
#Aplodontia rufa
#Artibeus lituratus, jamaicensis

#family, order
#link species to higher taxonomy
#how do confidence in these to compare?
#how many individuals or spp do you need for it to be sensible
#compare to Scotty Dog book

na.taxon <- pan.data.adult.10[is.na(pan.data.adult.10$MSW05_Order),]
unique(na.taxon$MSW05_Binomial)
#"Akodon tucumanensis"        "Artibeus planirostris"      "Clethrionomys gapperi"     
#"Geomys lutescens"           "Handleyomys melanotis"      "Heteromys catopterius"     
#"Ictidomys tridecemlineatus" "Lutra canadensis"           "Martes caurina"            
#"Mustela vison"              "Myotis aurascens"           "Oryzomys mexicanus"        
#"Oryzomys oryzomys"          "Peromyscus nudipes"         "Plecotus townsendii"       
#"Sturnira sturnira"          "Urocitellus elegans"        "Urocitellus parryii"  

# "Abrothrix xanthorhinus"            "Akodon alterus"                   
# "Akodon viridescens"                "Artibeus intermedius" 
# "Alopex lagopus"                    "Artibeus aequatorialis"           
# "Artibeus schwartzi"                "Artibeus tolteca"                 
# "Arvicola terrestris"               "Auliscomys micropus"              
# "Carollia carollia"                 "Carollia sowelii"                 
# "Cerradomys subflavus"              "Chaerephon pumila"                
# "Clethrionomys californicus"                    
# "Clethrionomys glareolus"           "Clethrionomys rufocanus"          
# "Clethrionomys rutilus"             "Dicrostonyx kilangmiutak"         
# "Eligmodontia bolsonensis"          "Euryoryzomys legatus"             
# "Euryoryzomys nitidus"              "Galea leucoblephara"              
# "Gerbilliscus (Taterona)"          
# "Glossophaga glossophaga"           "Hsunycteris thomasi"               
# "Hylaeamys megacephalus"            "Ictidomys mexicanus"               
# "Lagurus curtatus"                  "Macroscelides flavicaudatus"       
# "Macroscelides micus"                                  
# "Mastomys couchi"                   "Microdipodops megalocephalus"      
# "Micronycteris buriri"              "Microtus hyperboreus"              
# "Microtus mogollonensis"            "Miniopterus schreibersi"           
# "Molossops abrasus"                 "Molossops planirostris"            
# "Molossus ater"                     "Myotis bocagei"                    
# "Myotis dinellii"                   "Myotis nyctor"                     
# "Napeozapus insignis"               "Nephelomys devius"                
# "Octodon degu"                      "Oligoryzomys costaricensis"       
# "Oryzomys texensis"                 "Perimyotis subflavus"             
# "Peromyscus carletoni"                           
# "Peromyscus peromyscus"             "Pipistrellus nanus"                
# "Pitymys pinetorum"                               
# "Rattus flavipectus"                "Reithrodontomys megalotus"
# "Reithrodontomys reithrodontomys"   "Sorex rohweri"
# "Spermophilus parvidens"                            
# "Sylvilagus auduboni"               "Tatera brantsii"                  
# "Tatera leucogaster"                "Tonatia silvicola"                 
# "Transandinomys talamancae"         "Urocitellus armatus"              
# "Urocitellus beldingi"              "Urocitellus canus"                 
# "Urocitellus columbianus"                       
# "Urocitellus mollis"                              
# "Urocitellus richardsonii"          "Urocitellus undulatus"

pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Akodon tucumanensis"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Akodon tucumanensis"] <- "Cricetidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Akodon tucumanensis"] <- "Akodon"

pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Artibeus planirostris"] <- "Chiroptera"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Artibeus planirostris"] <- "Phyllostomidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Artibeus planirostris"] <- "Artibeus"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Clethrionomys gapperi"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Clethrionomys gapperi"] <- "Cricetidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Clethrionomys gapperi"] <- "Myodes"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Geomys lutescens"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Geomys lutescens"] <- "Geomyidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Geomys lutescens"] <- "Geomys"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Handleyomys melanotis"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Handleyomys melanotis"] <- "Cricetidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Handleyomys melanotis"] <- "Handleyomys"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Heteromys catopterius"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Heteromys catopterius"] <- "Heteromyidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Heteromys catopterius"] <- "Heteromys"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Ictidomys tridecemlineatus"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Ictidomys tridecemlineatus"] <- "Sciuridae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Ictidomys tridecemlineatus"] <- "Ictidomys"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Lutra canadensis"] <- "Carnivora"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Lutra canadensis"] <- "Mustelidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Lutra canadensis"] <- "Lontra"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Martes caurina"] <- "Carnivora"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Martes caurina"] <- "Mustelidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Martes caurina"] <- "Martes"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Mustela vison"] <- "Carnivora"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Mustela vison"] <- "Mustelidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Mustela vison"] <- "Neovison"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Myotis aurascens"] <- "Chiroptera"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Myotis aurascens"] <- "Vespertilionidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Myotis aurascens"] <- "Myotis"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Oryzomys mexicanus"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Oryzomys mexicanus"] <- "Cricetidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Oryzomys mexicanus"] <- "Oryzomys"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Oryzomys oryzomys"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Oryzomys oryzomys"] <- "Cricetidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Oryzomys oryzomys"] <- "Oryzomys"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Peromyscus nudipes"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Peromyscus nudipes"] <- "Cricetidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Peromyscus nudipes"] <- "Peromyscus"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Plecotus townsendii"] <- "Chiroptera"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Plecotus townsendii"] <- "Vespertilionidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Plecotus townsendii"] <- "Corynorhinus"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Sturnira sturnira"] <- "Chiroptera"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Sturnira sturnira"] <- "Phyllostomidae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Sturnira sturnira"] <- "Sturnira"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Urocitellus elegans"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Urocitellus elegans"] <- "Sciuridae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Urocitellus elegans"] <- "Urocitellus"
  
pan.data.adult.10$MSW05_Order[pan.data.adult.10$MSW05_Binomial == "Urocitellus parryii"] <- "Rodentia"
pan.data.adult.10$MSW05_Family[pan.data.adult.10$MSW05_Binomial == "Urocitellus parryii"] <- "Sciuridae"
pan.data.adult.10$MSW05_Genus[pan.data.adult.10$MSW05_Binomial == "Urocitellus parryii"] <- "Urocitellus"

#recount for sample size to 10 for genus, family, and order
#genus
data.genus_stats <-pan.data.adult.10 %>%
  group_by(MSW05_Genus) %>%
  dplyr::summarise(counts = n())
#49 genera

keep.genus <- data.genus_stats$MSW05_Genus[data.genus_stats$counts >= 10] #29 genera
data.genus.10 <- pan.data.adult.10[pan.data.adult.10$MSW05_Genus %in% keep.genus,]

#family
data.family_stats <- pan.data.adult.10 %>%
  group_by(MSW05_Family) %>%
  dplyr::summarise(counts = n()) #19 families

keep.family <- data.family_stats$MSW05_Family[data.family_stats$counts >= 10] #12 families
data.family.10 <- pan.data.adult.10[pan.data.adult.10$MSW05_Family %in% keep.family,]

#order
data.order_stats <- pan.data.adult.10 %>%
  group_by(MSW05_Order) %>%
  dplyr::summarise(counts = n()) #7 orders

keep.order <- data.order_stats$MSW05_Order[data.order_stats$counts >= 10] #4 orders
data.order.10 <- pan.data.adult.10[pan.data.adult.10$MSW05_Order %in% keep.order,]

#genus
gn.models <- unique(data.genus.10$MSW05_Genus)
model.results.genus <- data.frame()
for(i in 1:length(gn.models)){
  sub.data <- as.data.frame(data.genus.10[data.genus.10$MSW05_Genus == gn.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$total.length), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = sub.data$MSW05_Binomial[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(sub.data$mass))
  model.results.genus <- rbind(sub, model.results.genus)
}

#write.csv(model.results.genus, "model.results.genus.csv")

#family
fm.models <- unique(data.family.10$MSW05_Family)
model.results.family <- data.frame()
for(i in 1:length(fm.models)){
  sub.data <- as.data.frame(data.family.10[data.family.10$MSW05_Family == fm.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$total.length), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = sub.data$MSW05_Binomial[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(sub.data$mass))
  model.results.family <- rbind(sub, model.results.family)
}

#write.csv(model.results.family, "model.results.family.csv")

#order
or.models <- unique(data.order.10$MSW05_Order)
model.results.order <- data.frame()
for(i in 1:length(or.models)){
  sub.data <- as.data.frame(data.order.10[data.order.10$MSW05_Order == or.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$total.length), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = sub.data$MSW05_Binomial[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(sub.data$mass))
  model.results.order <- rbind(sub, model.results.order)
}

#write.csv(model.results.order, "model.results.order.csv")


#show skeletal v vertnet

##THOUGHT: exclude juvs, keep everything else,and then do three sigma

#plots!
#1. show confidence in line as function of sample size
con.species.sample <- lm(model.results.species$std.err.slope ~ model.results.species$sample.size)
con.genus.sample <- lm(model.results.genus$std.err.slope ~ model.results.genus$sample.size)
con.family.sample <- lm(model.results.family$std.err.slope ~ model.results.family$sample.size)
con.order.sample <- lm(model.results.order$std.err.slope ~ model.results.order$sample.size)
sum.con.sample.species <- summary(con.species.sample)
sum.con.sample.genus <- summary(con.genus.sample)
sum.con.sample.family <- summary(con.family.sample)
sum.con.sample.order <- summary(con.order.sample)

con.samples.models <- list(model.results.species, model.results.genus, model.results.family, model.results.order)
model.names <- c("model.results.species", "model.results.genus", "model.results.family", "model.results.order")

con.sample.results <- data.frame()
for(i in 1:length(model.names)){
  model <- lm(con.samples.models$slope[[i]][6] ~ con.samples.models$sample.size[[i]][9])
  sum.model <- summary(model)
  sub <- data.frame(binomial = model.names[i],
                                   intercept = model$coefficients[[1]],
                                   slope = model$coefficients[[2]],
                                   resid.std.err = sum.model$sigma,
                                   df = max(sum.model$df),
                                   std.err.slope =  sum.model$coefficients[4],
                                   std.err.intercept = sum.model$coefficients[3],
                                   r.squared = sum.model$r.squared)
  con.sample.results <- rbind(con.sample.results, sub)
}



plot(model.results$std.err.slope ~ model.results$df) #make nicer


##Q3
deer <- subset(pan.data.adult, pan.data.adult$MSW05_Binomial == "Odocoileus virginianus")
bats <- subset(pan.data.adult, pan.data.adult$MSW05_Order == "Chiroptera")
bats.clean.mass <- subset(bats, !is.na(bats$mass) & bats$mass != 0)
bats.clean.length <- subset(bats.clean.mass, !is.na(bats.clean.mass$total.length) & bats.clean.mass$total.length != 0)
bats.clean <- bats.clean.length


# transfer function
# log linear model; log10 or natural log
clean.length.mass$total.length <- as.numeric(clean.length.mass$total.length)
clean.length.mass$mass <- as.numeric(clean.length.mass$mass)
model <- lm(log10(clean.length.mass$total.length) ~ log10(clean.length.mass$mass))
#use fitted models for the prediction
#how uncertainty in line is distributed with use many individuals v. many individuals to make this prediction
#within and across species regression; able to get precise line (average for that group of species)
#can get prediction interval (and predicted values)
#because in log-log space, not symmetrical around the line
#take some case studies in scotty dog book and compare (look at sampling and compare)
#estimate and the std err or; residual error on df

#std.err of slope
#uncertainty in intercept (really, at center of the line; y mean)
#s(y.x) = error uncertainty in residuals; amount of variance in y that is leftover after the regression

#mean and std of slope and intecept and resids
#reconstruct regression uncertainty

# case 1: vertnet; have diff species and individ; can run regression with a large sample size = Best Practice
# also do best practices for other ones (tell parameters about how they'll be constrained)
# case 2: individ representing each species; can take vertnet data and run random replicates and run regression and compare dist of slopes to actual slope and look at uncertainty in lines to see how much it varies (how much less info do you have?)
# case 3: avg. all individ in species and run regression across averages; can do this with vertnet (avg. mass and length) and tell us what that line looks like compared to when have all individ data; and apparent uncertainty
# in the ms from Ray & Juha: they say that this approach is the best (#3); they claim you'd get too much noise because body mass varies so much throughout the year
# in discussion: plant a flag that we're going to look at the effect of intra and inter-annual cyclicity on body masses
# also call to action to get measurements of modern specimens that have mass

#for this paper just show how; and that it can be applied to other things; leave case 2 & 3 for another paper
#include Ray & Juha stuff in discussion; use reported masses adn see which regressions fit best; and that we need big mammals masses
#show whether or not our masses line up to masses in panthera (fall w/in 2 sigma of means of vertnet data); if unbiased random sample then 95% of them should fall within 2 sigmas

#discussion header: alternative approaches (Ray's data); applications (case 2 & 3; juha study w/ cougar data)
#subset cougar to intact masses and see which it's along

#vertnet: everything; family; how different are the slopes of the lines from the different families
# can do an anova (all slopes come from the same parent distribution)

# ggplot(clean.length.mass) + 
#   facet_wrap(~ scientificName,  ncol = 2) +
#   geom_point(aes(x = log10(clean.length.mass$mass), y = clean.length.mass$total.length)) + 
#   geom_smooth(aes(x = log10(clean.length.mass$mass), y = clean.length.mass$total.length), method = "lm") + 
#   ggtitle(~ scientificName) +
#   scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
#   scale_y_continuous(name = "Total Length (cm)")


##Q3: transfer function----
#code for regressions of limb data vs bodymass
#Odocoileus virginianus: hind foot, mass, ankle measurement
#Spermophilus beecheyi: mass, toothrow


##Spermophilus beecheyi----

Sbeecheyi <- data[data$scientificName == "Spermophilus beecheyi",]

Sbeecheyi.clean <- Sbeecheyi[Sbeecheyi$mass.status == "GOOD" & Sbeecheyi$total.length.status == "GOOD" & Sbeecheyi$mass.units == "g" & Sbeecheyi$total.length.units == "mm",
                             select = c("scientificName","mass", "head.body.length", "tooth.row", "hindfoot.length")]
Sbeecheyi <- Sbeecheyi.cleaner

#mass vs toothrow length
model1 <- lm(log10(Sbeecheyi$mass) ~ log10(Sbeecheyi$tooth.row), na.action=na.exclude)
sum.model1 <- summary(model1)
sub1 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Mass/toothrow"),
                   intercept = model1$coefficients[[1]],
                   slope = model1$coefficients[[2]],
                   resid.std.err = sum.model1$sigma,
                   df = max(sum.model1$df),
                   std.err.slope =  sum.model1$coefficients[4],
                   std.err.intercept = sum.model1$coefficients[3],
                   r.squared = sum.model1$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$tooth.row),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(mass))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(mass)),
              method = "lm", color = "slateblue4") +
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Toothrow~Length~(mm)))
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow.png"), width = 14, height = 10, units = "cm")

#mass vs hindfoot
model2 <- lm(log10(Sbeecheyi$mass) ~ log10(Sbeecheyi$hindfoot.length), na.action=na.exclude)
sum.model2 <- summary(model2)
sub2 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Mass/hinfoot"),
                   intercept = model2$coefficients[[1]],
                   slope = model2$coefficients[[2]],
                   resid.std.err = sum.model2$sigma,
                   df = max(sum.model2$df),
                   std.err.slope =  sum.model2$coefficients[4],
                   std.err.intercept = sum.model2$coefficients[3],
                   r.squared = sum.model2$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$hindfoot.length) & !is.na(Sbeecheyi$mass),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(hindfoot.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(hindfoot.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Hindfoot~Length~(mm))) 
ggsave(p, file=paste0("plot_Spermophilus beecheyi_hingfoot.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(sub1, sub2)  

#tooth row vs hindfoot  

model3 <- lm(log10(Sbeecheyi$hindfoot.length) ~ log10(Sbeecheyi$tooth.row), na.action=na.exclude)
sum.model3 <- summary(model3)
sub3 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("hindfoot length/toothrow length"),
                   intercept = model3$coefficients[[1]],
                   slope = model3$coefficients[[2]],
                   resid.std.err = sum.model3$sigma,
                   df = max(sum.model3$df),
                   std.err.slope =  sum.model3$coefficients[4],
                   std.err.intercept = sum.model3$coefficients[3],
                   r.squared = sum.model3$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$hindfoot.length) & !is.na(Sbeecheyi$tooth.row),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(hindfoot.length))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(hindfoot.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Toothrow~Length~(mm))) +
  ylab(expression(log[10]~Hindfoot~Length~(mm))) 
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_hingfoot.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, sub3)  

#mass versus total length
model4 <- lm(log10(Sbeecheyi$mass) ~ log10(Sbeecheyi$head.body.length), na.action=na.exclude)
sum.model4 <- summary(model4)
sub4 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Mass/Total length"),
                   intercept = model4$coefficients[[1]],
                   slope = model4$coefficients[[2]],
                   resid.std.err = sum.model4$sigma,
                   df = max(sum.model4$df),
                   std.err.slope =  sum.model4$coefficients[4],
                   std.err.intercept = sum.model4$coefficients[3],
                   r.squared = sum.model4$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$mass) & !is.na(Sbeecheyi$total.length),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(head.body.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(head.body.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Total~Length~(mm)))

ggsave(p, file=paste0("plot_Spermophilus beecheyi_totallength.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, sub4)  

#toothrow versus total length
model5 <- lm(log10(Sbeecheyi$head.body.length) ~ log10(Sbeecheyi$tooth.row), na.action=na.exclude)
sum.model5 <- summary(model5)
sub5 <- data.frame(binomial = Sbeecheyi$MSW05_Binomial[1],
                   comparison = ("Total length/toothrow length"),
                   intercept = model5$coefficients[[1]],
                   slope = model5$coefficients[[2]],
                   resid.std.err = sum.model5$sigma,
                   df = max(sum.model5$df),
                   std.err.slope =  sum.model5$coefficients[4],
                   std.err.intercept = sum.model5$coefficients[3],
                   r.squared = sum.model5$r.squared,
                   sample.size = nrow(Sbeecheyi[!is.na(Sbeecheyi$total.length) & !is.na(Sbeecheyi$tooth.row),]))

p = ggplot(data = Sbeecheyi) + 
  geom_point(aes(x = log10(tooth.row), y = log10(head.body.length))) +
  geom_smooth(aes(x = log10(tooth.row), y = log10(head.body.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Spermophilus beecheyi") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Toothrow~Length~(mm))) +
  ylab(expression(log[10]~Total~Length~(mm))) 
ggsave(p, file=paste0("plot_Spermophilus beecheyi_toothrow_totallength.png"), width = 14, height = 10, units = "cm")

model.Spermophilus.beecheyi <- rbind(model.Spermophilus.beecheyi, sub5)  
#write.csv(model.Spermophilus.beecheyi, file= "model.results.Spermophilus.beecheyi.csv")

#Strangely Futres does not have the hindfoot data for Odocoileus virginianus so for now I am going to pull it from a different dataset?

## Odocoileus virginianus----
## add paleo deer data----
old.deer <- read.csv(ArchaeoDeerAstragalusCalcaneus.csv, header = TRUE)

Ovirginianus <- pan.data[pan.data$MSW05_Binomial == "Odocoileus virginianus",]

Ovirginianus.clean <- Ovirginianus[Ovirginianus$lifeStage != "Juvenile" & Ovirginianus$mass.status != "outlier" & Ovirginianus$total.length.status != "outlier",]
Ovirginianus <- Ovirginianus.clean

#mass vs hindfoot length
model1 <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$hindfoot.length), na.action=na.exclude)
sum.model1 <- summary(model1)
sub1 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("Mass/hindfoot"),
                   intercept = model$coefficients[[1]],
                  slope = model$coefficients[[2]],
                  resid.std.err = sum.model$sigma,
                  df = max(sum.model$df),
                  std.err.slope =  sum.model$coefficients[4],
                  std.err.intercept = sum.model$coefficients[3],
                  r.squared = sum.model$r.squared,
                  sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$hindfoot.length),]))

#ggtitle doesn't seem to be doing anything because it is not connected to p. When I do connect it scale_x and scale y takes away the axis tick marks. 
p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(mass), y = log10(hindfoot.length))) +
  geom_smooth(aes(x = log10(mass), y = log10(hindfoot.length)),
              method = "lm", color = "slateblue4") +
ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Body~Mass~(g))) +
  ylab(expression(log[10]~Hindfoot~Length~(mm))) + 
  ggsave(p, file=paste0("plot_Odocoileus virginianus_mass_hindfoot.png"), width = 14, height = 10, units = "cm")

#mass vs astragalus
model2 <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$astragalus.length), na.action=na.exclude)
sum.model2 <- summary(model2)
sub2 <- data.frame(binomial = Ovirginianusi$MSW05_Binomial[1],
                   comparison = ("Mass/astragalus"),
                   intercept = model2$coefficients[[1]],
                   slope = model2$coefficients[[2]],
                   resid.std.err = sum.model2$sigma,
                   df = max(sum.model2$df),
                   std.err.slope =  sum.model2$coefficients[4],
                   std.err.intercept = sum.model2$coefficients[3],
                   r.squared = sum.model2$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$mass),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Astragalus~Length~(mm))) 
ggsave(p, file=paste0("plot_Odocoileus virginianus_mass_astragalus.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(sub1, sub2)  

#astragalus vs hindfoot  

model3 <- lm(log10(Ovirginianus$hindfoot.length) ~ log10(Ovirginianus$astragalus.length), na.action=na.exclude)
sum.model3 <- summary(model3)
sub3 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("hindfoot length/astraglus length"),
                   intercept = model3$coefficients[[1]],
                   slope = model3$coefficients[[2]],
                   resid.std.err = sum.model3$sigma,
                   df = max(sum.model3$df),
                   std.err.slope =  sum.model3$coefficients[4],
                   std.err.intercept = sum.model3$coefficients[3],
                   r.squared = sum.model3$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$astragalus.length) & !is.na(Ovirginianus$hindfoot.length),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(hindfoot.length))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(hindfoot.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Astragalus~Length~(mm))) +
  ylab(expression(log[10]~Hindfoot~Length~(mm))) 
ggsave(p, file=paste0("plot_Odocoileus virginianus_astragalus_hingfoot.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, sub3)  

#mass versus total length
model4 <- lm(log10(Ovirginianus$mass) ~ log10(Ovirginianus$total.length), na.action=na.exclude)
sum.model4 <- summary(model4)
sub4 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("Mass/Total length"),
                   intercept = model4$coefficients[[1]],
                   slope = model4$coefficients[[2]],
                   resid.std.err = sum.model4$sigma,
                   df = max(sum.model4$df),
                   std.err.slope =  sum.model4$coefficients[4],
                   std.err.intercept = sum.model4$coefficients[3],
                   r.squared = sum.model4$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$mass) & !is.na(Ovirginianus$total.length),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(total.length), y = log10(mass))) +
  geom_smooth(aes(x = log10(total.length), y = log10(mass)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  ylab(expression(log[10]~Body~Mass~(g))) +
  xlab(expression(log[10]~Total~Length~(mm)))

ggsave(p, file=paste0("plot_Odocoileus virginianus_mass_totallength.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, sub4)  

#astragalus versus total length
model5 <- lm(log10(Ovirginianus$total.length) ~ log10(Ovirginianus$astragalus.length), na.action=na.exclude)
sum.model5 <- summary(model5)
sub5 <- data.frame(binomial = Ovirginianus$MSW05_Binomial[1],
                   comparison = ("Total length/astragalus length"),
                   intercept = model5$coefficients[[1]],
                   slope = model5$coefficients[[2]],
                   resid.std.err = sum.model5$sigma,
                   df = max(sum.model5$df),
                   std.err.slope =  sum.model5$coefficients[4],
                   std.err.intercept = sum.model5$coefficients[3],
                   r.squared = sum.model5$r.squared,
                   sample.size = nrow(Ovirginianus[!is.na(Ovirginianus$total.length) & !is.na(Ovirginianus$astragalus.length),]))

p = ggplot(data = Ovirginianus) + 
  geom_point(aes(x = log10(astragalus.length), y = log10(total.length))) +
  geom_smooth(aes(x = log10(astragalus.length), y = log10(head.body.length)),
              method = "lm", color = "slateblue4")+
  ggtitle("Odocoileus virginianus") +
  theme(plot.title = element_text(face = "italic"))+
  xlab(expression(log[10]~Astragalus~Length~(mm))) +
  ylab(expression(log[10]~Total~Length~(mm))) 
ggsave(p, file=paste0("plot_Odocoileus virginanus_astragalus_totallength.png"), width = 14, height = 10, units = "cm")

model.Odocoileus.virginianus <- rbind(model.Odocoileus.virginianus, sub5)  
#write.csv(model.Spermophilus.beecheyi, file= "model.results.Spermophilus.beecheyi.csv")



#the below code is for plotting many measurments.
#can switch out species name for whichever is the target
#test <- subset(data.limb, scientificName== "Aepyceros melampus", select = c("X","occurrenceID", "scientificName", "lifeStage", "sex", "catalogNumber", "mass", "astragalus.length", "astragalus.width", "calcaneus.GB", "calcaneus.GL", "femur.length", "humerus.length", "forearm.length", "tooth.row"))

test_reshape <- gather(data = test, 
                       key = Measurement, 
                       value = value, 
                       8:15)

sp.models.lim <- unique(test_reshape$Measurement)
model.results.species.limb <- data.frame()
for(i in 1:length(sp.models.lim)){
  sub.data <- as.data.frame(test_reshape[test_reshape$Measurement == sp.models[i],])
  model <- lm(log10(sub.data$mass) ~ log10(sub.data$value), na.action=na.exclude)
  sum.model <- summary(model)
  sub <- data.frame(binomial = sub.data$scientificName[1],
                    intercept = model$coefficients[[1]],
                    slope = model$coefficients[[2]],
                    resid.std.err = sum.model$sigma,
                    df = max(sum.model$df),
                    std.err.slope =  sum.model$coefficients[4],
                    std.err.intercept = sum.model$coefficients[3],
                    r.squared = sum.model$r.squared,
                    sample.size = length(sub.data$mass))
  model.results.species.limb <- rbind(sub, model.results.species.limb)
}

#plotting, I need to still fix this
for (i in sp.models.lim) {
  p = ggplot(data = subset(test_reshape, Measurement  == i)) + 
    geom_point(aes(x = log10(mass), y = log10(value))) +
    geom_smooth(aes(x = log10(mass), y = log10(value)),
                method = "lm", color = "slateblue4")
  ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_log10(name = expression(log[10]~Total~Length~(mm))) + 
    ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}









