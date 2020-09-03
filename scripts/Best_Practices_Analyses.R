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
require(ape)
require(caper)
require(phytools)

##Upload data----
pan <- read.csv("https://de.cyverse.org/dl/d/88B409B3-8626-471C-BC8E-1925EBE2A6C5/pantheria.csv", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("https://de.cyverse.org/dl/d/9A1CB483-4ABF-49F2-9F4C-1201EFA774E6/labeled.clean.data.csv", header = TRUE)
mamm.tree <- read.tree("https://de.cyverse.org/dl/d/DD53DD75-07A0-4609-A321-F3819E72AE5D/Mammal2.tre")

##combine with pantheria----
sp.data <- unique(data$scientificName) #605 spp
length(pan[pan$MSW05_Binomial %in% sp.data,]) #538 spp

pan.data <- merge(pan, data, by.x = "MSW05_Binomial", by.y = "scientificName", all.y = TRUE, all.x = FALSE)
length(unique(pan.data$MSW05_Binomial))

###head-body-length----
pan.data$head.body.length <- pan.data$total.length - pan.data$tail.length
# need to get bunnies and deer
for(i in 1:nrow(pan.data)){
  if(is.na(pan.data$head.body.length[pan.data$MSW05_Family == "Cervidae"][i])){
    pan.data$head.body.length[i] <- pan.data$total.length[i]
  }
  else{
    next
  }
}

for(i in 1:nrow(pan.data)){
  if(is.na(pan.data$head.body.length[pan.data$MSW05_Family == "Leporidae"][i])){
    pan.data$head.body.length[i] <- pan.data$total.length[i]
  }
  else{
    next
  }
}

#write.csv(pan.data, "data.taxonomy.csv")

##Q1 compare to pantheria----
pan.clean <- pan[!is.na(pan$X5.1_AdultBodyMass_g),] #474 sp

pan.data.clean <- pan.data[!is.na(pan.data.adult$mass) & pan.data.adult$mass.status != "outlier",] #489 sp
length(unique(pan.data.adult.clean$MSW05_Binomial)) #344

pan.data.adult_stats <- pan.data.adult.clean %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(sample.size = n(), 
                   min.mass = min(mass, na.rm = TRUE),
                   max.mass  = max(mass, na.rm = TRUE),
                   avg.mass = mean(mass, na.rm = TRUE),
                   sd.err.mass = sd(mass, na.rm = TRUE)/sqrt(sample.size),
                   pan.mass = X5.1_AdultBodyMass_g[1],
                   mass.diff = abs((pan.mass - avg.mass) / sd.err.mass))
pan.data.adult_stats.10 <- pan.data.adult_stats %>%
  filter(sample.size >= 10) #298 spp
#write.csv(pan.data.adult_stats.10, "pan.results.csv")

length(pan.data.adult_stats.10$MSW05_Binomial[pan.data.adult_stats.10$mass.diff <= 2]) #67
length(pan.data.adult_stats.10$MSW05_Binomial[pan.data.adult_stats.10$mass.diff > 2]) #231
#22.5% are ok; 77.5% are not

##FIGURE: body mass distributions w/ line from PanTHERIA----

pan.data.adult.clean.10 <- pan.data.adult.clean[pan.data.adult.clean$MSW05_Binomial %in% pan.data.adult_stats.10$MSW05_Binomial,]

uniq_species <- unique(pan.data.adult.clean.10$MSW05_Binomial)
for (i in uniq_species) {
  p = ggplot(data = subset(pan.data.adult.clean.10, MSW05_Binomial == i)) + 
    geom_density(aes(log10(mass)), fill = "darkslateblue") +
    ggtitle(i) +
    scale_x_log10(name = expression(log[10]~Body~Mass~(g))) +
    scale_y_continuous(name = 'Probability') + 
    geom_vline(xintercept = log10(pan.data.adult.clean.10$X5.1_AdultBodyMass_g[pan.data.adult.clean.10$MSW05_Binomial == i][1]))
  ggsave(p, file=paste0("dist_", i,".png"), width = 14, height = 10, units = "cm")
}

##Q2: length v. mass----
#clean data
pan.data.mass <- pan.data[!is.na(pan.data$mass) & pan.data$mass.status != "outlier",]
pan.data.mass.length <- pan.data.mass[!is.na(pan.data.mass$total.length) & pan.data.mass$total.length.status != "outlier",]
pan.data.adult <- pan.data.mass.length[pan.data.mass.length$lifeStage != "Juvenile",]
length(unique(pan.data.adult$MSW05_Binomial)) #338sp

##some measurements are zero, oy vey
pan.data.adult.clean <- pan.data.adult[pan.data.adult$mass > 0 & pan.data.adult$head.body.length > 0,]

#recount sample sizes
pan.data.adult_stats <- pan.data.adult.clean %>%
  group_by(MSW05_Binomial) %>%
  dplyr::summarise(counts = n())

keep.10 <- pan.data.adult_stats$MSW05_Binomial[pan.data.adult_stats$counts >= 10] #286 sp
pan.data.adult.10 <- pan.data.adult.clean[pan.data.adult.clean$MSW05_Binomial %in% keep.10,]
length(unique(pan.data.adult.10$MSW05_Binomial)) #286 sp

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

na.taxon <- pan.data.adult.10[is.na(pan.data.adult.10$MSW05_Order),]
unique(na.taxon$MSW05_Binomial)
#"Akodon tucumanensis"        "Artibeus planirostris"      "Clethrionomys gapperi"     
#"Geomys lutescens"           "Handleyomys melanotis"      "Heteromys catopterius"     
#"Ictidomys tridecemlineatus" "Lutra canadensis"           "Martes caurina"            
#"Mustela vison"              "Myotis aurascens"           "Oryzomys mexicanus"        
#"Oryzomys oryzomys"          "Peromyscus nudipes"         "Plecotus townsendii"       
#"Sorex (Otisorex)"           "Sturnira sturnira"          "Urocitellus elegans"       
#"Urocitellus parryii"       

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

##Q2a. Drivers of allometry----
pan.models <- merge(pan, model.results.species, by.x = "MSW05_Binomial", by.y = "binomial", all.x = FALSE, all.y = TRUE)
pan.models$cent.lat <- pan.models$X26.2_GR_MaxLat_dd - pan.models$X26.3_GR_MinLat_dd
reg.results <- lm(pan.models$slope ~ pan.models$X5.1_AdultBodyMass_g + pan.models$X26.1_GR_Area_km2 + pan.models$X12.2_Terrestriality + pan.models$cent.lat)
summary(reg.results)

#Q2b. Phylogenetic signal----
plot(mamm.tree)

#filter species that don't match
mamm.tree$tip.label #give indices for each mamm
#use tip labels to filter the results
model.results.species$binomial <- gsub(" ", "_", model.results.species$binomial)
sp.results_sub <- subset(model.results.species, binomial %in% mamm.tree$tip.label) #214

#creates list of species and value
slope <- setNames(sp.results_sub$slope, sp.results_sub$binomial)
std.err <- setNames(sp.results_sub$std.err.slope, sp.results_sub$binomial)
r.squared <- setNames(sp.results_sub$r.squared, sp.results_sub$binomial)
#Blomberg's K
phylosig(mamm.tree, slope, method="lambda", test = TRUE, nsim = 1000, se = NULL, start = NULL, control = list()) #39.2275 
phylosig(mamm.tree, slope, method="K", test = TRUE, nsim = 1000, se = NULL, start = NULL, control = list()) #0.0883418

#merge panthera summary stats
#examine non-phylo model (slope~avg.mass)
#avg bs phylo controlled, but not a predictor of slope

#mass and slope highly constrained, but slope and mass are not correlated
#get centroid lat of sp; trop v polar and how these relationships change; geogr range
#use pan for those metrics; non-phylo controlled models

##Q3: or other paper compare to Scotty Dog Book----



##Q4
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








