## Korrelation mellem PP og distance metoden

list.files()

PP.data <- read_excel("PP_data_version2.xlsx")

PP.data <- data.frame(PP.data)

head(PP.data)

length(unique(PP.data$Plot))

PP.data <- PP.data[order(PP.data$Plot),]



dist.data <- read.csv("Distance data - behandlet.csv",
         header = T,
         sep = ";")

length(unique(dist.data$Plot))

dist.data <- dist.data[order(dist.data$Plot),]

###################################

PP.plot.test <- data.frame(Plot = unique(PP.data$Plot))

head(PP.plot.test)

length(PP.plot.test$Plot)


dist.plot.test <- data.frame(Plot = unique(dist.data$Plot))

head(dist.plot.test)

length(hab.plot.test$Plot)


ifelse(PP.plot.test$Plot == dist.plot.test$Plot, "Yes", "No")

dist.plot.test[62,1] <- "8575E060"

#############################

## der udtrækkes 10 tilfældige arter

Arter <- unique(PP.data$Dansk.navn)

sample(Arter, 10)

## [1] "Rød svingel"          "Lyng-snerre"          "Almindelig mjødurt"  
## [4] "Rød-gran"             "Almindelig syre"      "Almindelig star"     
## [7] "Hedelyng"             "Katteskæg"            "Almindelig hvene"   
## [10] "Sump-kællingetand"

################################

names(PP.data)

PP.data.sort <- data.frame(Plot = PP.data$Plot,
                             Dansk.navn = PP.data$Dansk.navn,
                             Tæthed = PP.data$Tæthed)


##############################
                             
rod.svingel.PP <- subset(PP.data.sort, Dansk.navn == "Rød svingel")

rod.svingel.PP


rod.svingel.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Rød.svingel_v,
                               dist.b = dist.data$Rød.svingel_b)

rod.svingel.dist$dist <- ifelse(rod.svingel.dist$dist.v < rod.svingel.dist$dist.b,
                                rod.svingel.dist$dist.v,
                                rod.svingel.dist$dist.b)

rod.svingel.dist <- subset(rod.svingel.dist, dist < 500)

rod.svingel.dist <- subset(rod.svingel.dist, Plot != "73C13D2C")

rod.svingel.dist

hist(rod.svingel.PP$Tæthed)

hist(rod.svingel.dist$dist)

rod.svingel.test <- cor.test(rod.svingel.PP$Tæthed, rod.svingel.dist$dist, method = "spearman")

test.est <- rod.svingel.test$estimate

test.est[1,]

## rød svingel
## testen er sigifikant (9.228e-05) og de er korreleret
#rho = -0,89

p.val <- rod.svingel.test$p.value
est <- rod.svingel.test$estimate

rod.svingel.data <- data.frame(Art = "Rød svingel",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(rod.svingel.PP$Plot))

################################################

lyng.snerre.PP <- subset(PP.data.sort, Dansk.navn == "Lyng-snerre")

lyng.snerre.PP

lyng.snerre.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Lyng.snerre_v,
                               dist.b = dist.data$Lyng.snerre_b)

lyng.snerre.dist$dist <- ifelse(lyng.snerre.dist$dist.v < lyng.snerre.dist$dist.b,
                                lyng.snerre.dist$dist.v,
                                lyng.snerre.dist$dist.b)

lyng.snerre.dist <- subset(lyng.snerre.dist, dist < 500)

hist(lyng.snerre.PP$Tæthed)

hist(lyng.snerre.dist$dist)

ifelse(lyng.snerre.PP$Plot == lyng.snerre.dist$Plot, "Yes", "No") 

lyng.snerre.test <- cor.test(lyng.snerre.PP$Tæthed, lyng.snerre.dist$dist, 
         method = "spearman")

## lyng-snerre
## testen er sigifikant (1.09e-07) og de er korreleret
#rho = -0,73

p.val <- lyng.snerre.test$p.value
est <- lyng.snerre.test$estimate

lyng.snerre.data <- data.frame(Art = "Lyng-snerre",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(lyng.snerre.PP$Plot))

test.data <- rbind(rod.svingel.data, lyng.snerre.data)

##################################

alm.mjødurt.PP <- subset(PP.data.sort, Dansk.navn == "Almindelig mjødurt")

alm.mjødurt.PP

alm.mjødurt.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Alm.mjødurt_v,
                               dist.b = dist.data$Alm.mjødurt_b)

alm.mjødurt.dist$dist <- ifelse(alm.mjødurt.dist$dist.v < alm.mjødurt.dist$dist.b,
                                alm.mjødurt.dist$dist.v,
                                alm.mjødurt.dist$dist.b)

alm.mjødurt.dist <- subset(alm.mjødurt.dist, dist < 500)

hist(alm.mjødurt.PP$Tæthed)

hist(alm.mjødurt.dist$dist)

ifelse(alm.mjødurt.PP$Plot == alm.mjødurt.dist$Plot, "Yes", "No") 

alm.mjødurt.test <- cor.test(alm.mjødurt.PP$Tæthed, alm.mjødurt.dist$dist, 
         method = "spearman")

## almindelig mjødurt
## testen er IKKE signikikant (0,15) og de er korreleret
#rho = -0,66

p.val <- alm.mjødurt.test$p.value
est <- alm.mjødurt.test$estimate

alm.mjødurt.data <- data.frame(Art = "Almindelig mjødurt",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(alm.mjødurt.PP$Plot))

test.data <- rbind(test.data, alm.mjødurt.data)

test.data

#################################################

rød.gran.PP <- subset(PP.data.sort, Dansk.navn == "Rød-gran")

rød.gran.PP

rød.gran.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Rødgran_v,
                               dist.b = dist.data$Rødgran_b)

rød.gran.dist$dist <- ifelse(rød.gran.dist$dist.v < rød.gran.dist$dist.b,
                                rød.gran.dist$dist.v,
                                rød.gran.dist$dist.b)

rød.gran.dist <- subset(rød.gran.dist, dist < 500)

hist(rød.gran.PP$Tæthed)

hist(rød.gran.dist$dist)

ifelse(rød.gran.PP$Plot == rød.gran.dist$Plot, "Yes", "No") 

rød.gran.test <- cor.test(rød.gran.PP$Tæthed, rød.gran.dist$dist, 
         method = "spearman")

## rød-gran
## testen er sigifikant (0.0034) og de er korreleret
#rho = -0,58

p.val <- rød.gran.test$p.value
est <- rød.gran.test$estimate

rød.gran.data <- data.frame(Art = "Rød-gran",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(rød.gran.PP$Plot))

test.data <- rbind(test.data, rød.gran.data)

test.data

################################

alm.syre.PP <- subset(PP.data.sort, Dansk.navn == "Almindelig syre")

alm.syre.PP

alm.syre.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Alm.syre_v,
                               dist.b = dist.data$Alm.syre_b)

alm.syre.dist$dist <- ifelse(alm.syre.dist$dist.v < alm.syre.dist$dist.b,
                                alm.syre.dist$dist.v,
                                alm.syre.dist$dist.b)

alm.syre.dist <- subset(alm.syre.dist, dist < 500)


hist(alm.syre.PP$Tæthed)

hist(alm.syre.dist$dist)

ifelse(alm.syre.PP$Plot == alm.syre.dist$Plot, "Yes", "No") 

alm.syre.test <- cor.test(alm.syre.PP$Tæthed, alm.syre.dist$dist, 
         method = "spearman")

# almindelig syre
## testen er sigifikant (0.000975) og de er korreleret
#rho = -0,76

p.val <- alm.syre.test$p.value
est <- alm.syre.test$estimate

alm.syre.data <- data.frame(Art = "Almindelig syre",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(alm.syre.PP$Plot))

test.data <- rbind(test.data, alm.syre.data)

test.data

#########################################

alm.star.PP <- subset(PP.data.sort, Dansk.navn == "Almindelig star")

alm.star.PP

alm.star.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Alm.star_v,
                               dist.b = dist.data$Alm.star_b)

alm.star.dist$dist <- ifelse(alm.star.dist$dist.v < alm.star.dist$dist.b,
                                alm.star.dist$dist.v,
                                alm.star.dist$dist.b)

alm.star.dist <- subset(alm.star.dist, dist < 500)

hist(alm.star.PP$Tæthed)

hist(alm.star.dist$dist)

ifelse(alm.star.PP$Plot == alm.star.dist$Plot, "Yes", "No") 

alm.star.test <- cor.test(alm.star.PP$Tæthed, alm.star.dist$dist, 
         method = "spearman")

# almindelig star
## testen er sigifikant (1.685e-09) og de er korreleret
#rho = -0,90

p.val <- alm.star.test$p.value
est <- alm.star.test$estimate

alm.star.data <- data.frame(Art = "Almindelig star",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(alm.star.PP$Plot))

test.data <- rbind(test.data, alm.star.data)

test.data

###########################################

hedelyng.PP <- subset(PP.data.sort, Dansk.navn == "Hedelyng")

hedelyng.PP

hedelyng.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Hedelyng_v,
                               dist.b = dist.data$Hedelyng_b)

hedelyng.dist$dist <- ifelse(hedelyng.dist$dist.v < hedelyng.dist$dist.b,
                                hedelyng.dist$dist.v,
                                hedelyng.dist$dist.b)

hedelyng.dist <- subset(hedelyng.dist, dist < 500)

hist(hedelyng.PP$Tæthed)

hist(hedelyng.dist$dist)

ifelse(hedelyng.PP$Plot == hedelyng.dist$Plot, "Yes", "No") 

hedelyng.test <- cor.test(hedelyng.PP$Tæthed, hedelyng.dist$dist, 
         method = "spearman")

length(hedelyng.PP$Plot)

## Hedelyng
## testen er sigifikant (2.2e-16) og de er korreleret
#rho = -0.81
## 86 observationer

p.val <- hedelyng.test$p.value
est <- hedelyng.test$estimate

hedelyng.data <- data.frame(Art = "Hedelyng",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(hedelyng.PP$Plot))

test.data <- rbind(test.data, hedelyng.data)

test.data


##########################################################

katteskæg.PP <- subset(PP.data.sort, Dansk.navn == "Katteskæg")

katteskæg.PP

katteskæg.dist <- data.frame(Plot = dist.data$Plot,
                                dist.v = dist.data$Katteskæg_v,
                                dist.b = dist.data$Katteskæg_b)

katteskæg.dist$dist <- ifelse(katteskæg.dist$dist.v < katteskæg.dist$dist.b,
                                 katteskæg.dist$dist.v,
                                 katteskæg.dist$dist.b)

katteskæg.dist <- subset(katteskæg.dist, dist < 500)

hist(katteskæg.PP$Tæthed)

hist(katteskæg.dist$dist)

ifelse(katteskæg.PP$Plot == katteskæg.dist$Plot, "Yes", "No")

katteskæg.test <- cor.test(katteskæg.PP$Tæthed, katteskæg.dist$dist, 
         method = "spearman")

## Katteskæg
## testen er signifikant (0,02247) og korreleret
## rho = -0,68

p.val <- katteskæg.test$p.value
est <- katteskæg.test$estimate

katteskæg.data <- data.frame(Art = "Katteskæg",
                            korrelation = est,
                            p.val = p.val,
                            observationer = length(katteskæg.PP$Plot))

test.data <- rbind(test.data, katteskæg.data)

test.data

###################################################

alm.hvene.PP <- subset(PP.data.sort, Dansk.navn == "Almindelig hvene")

alm.hvene.PP

alm.hvene.dist <- data.frame(Plot = dist.data$Plot,
                                dist.v = dist.data$Alm.hvene_v,
                                dist.b = dist.data$Alm.hvene_b)

alm.hvene.dist$dist <- ifelse(alm.hvene.dist$dist.v < alm.hvene.dist$dist.b,
                                 alm.hvene.dist$dist.v,
                                 alm.hvene.dist$dist.b)

alm.hvene.dist <- subset(alm.hvene.dist, dist < 500)

hist(alm.hvene.PP$Tæthed)

hist(alm.hvene.dist$dist)

ifelse(alm.hvene.PP$Plot == alm.hvene.dist$Plot, "Yes", "No") 

alm.hvene.test <- cor.test(alm.hvene.PP$Tæthed, alm.hvene.dist$dist, 
         method = "spearman")

length(alm.hvene.PP$Plot)

## Almindelig hvene
## testen er sigifikant (3.213e-07) og de er korreleret
#rho = -0.85
## 23 observationer

p.val <- alm.hvene.test$p.value
est <- alm.hvene.test$estimate

alm.hvene.data <- data.frame(Art = "Almindelig hvene",
                            korrelation = est,
                            p.val = p.val,
                            observationer = length(alm.hvene.PP$Plot))

test.data <- rbind(test.data, alm.hvene.data)

test.data

#######################################

sump.kæl.PP <- subset(PP.data.sort, Dansk.navn == "Sump-kællingetand")

sump.kæl.PP

sump.kæl.dist <- data.frame(Plot = dist.data$Plot,
                                dist.v = dist.data$Sump.kællingetand_v,
                                dist.b = dist.data$Sump.kællingetand_b)

sump.kæl.dist$dist <- ifelse(sump.kæl.dist$dist.v < sump.kæl.dist$dist.b,
                                 sump.kæl.dist$dist.v,
                                 sump.kæl.dist$dist.b)

sump.kæl.dist <- subset(sump.kæl.dist, dist < 500)

hist(sump.kæl.PP$Tæthed)

hist(sump.kæl.dist$dist)

ifelse(sump.kæl.PP$Plot == sump.kæl.dist$Plot, "Yes", "No") 

sump.kæl.test <- cor.test(sump.kæl.PP$Tæthed, sump.kæl.dist$dist, 
         method = "spearman")

length(sump.kæl.PP$Plot)

## Sump-kællingetand
## testen er ikke sigifikant (0.07221) og de er korreleret
#rho = -0.77
## 6 observationer

p.val <- sump.kæl.test$p.value
est <- sump.kæl.test$estimate

sump.kæl.data <- data.frame(Art = "Sump-kællingetand",
                            korrelation = est,
                            p.val = p.val,
                            observationer = length(sump.kæl.PP$Plot))

test.data <- rbind(test.data, sump.kæl.data)

test.data

write.csv(test.data, "korrelationstest.csv")



######################################################

std.error <- function(x) sd(x)/sqrt(length(x))

mean.kor <- mean(test.data$korrelation)
mean.kor

SE.kor <- std.error(test.data$korrelation)
SE.kor

mean.p <- mean(test.data$p.val)
mean.p

SE.p <- std.error(test.data$p.val)
SE.p







