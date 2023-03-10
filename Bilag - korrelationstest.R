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

## der udtr?kkes 10 tilf?ldige arter

Arter <- unique(PP.data$Dansk.navn)

sample(Arter, 10)

## [1] "R?d svingel"          "Lyng-snerre"          "Almindelig mj?durt"  
## [4] "R?d-gran"             "Almindelig syre"      "Almindelig star"     
## [7] "Hedelyng"             "Kattesk?g"            "Almindelig hvene"   
## [10] "Sump-k?llingetand"

################################

names(PP.data)

PP.data.sort <- data.frame(Plot = PP.data$Plot,
                             Dansk.navn = PP.data$Dansk.navn,
                             T?thed = PP.data$T?thed)


##############################
                             
rod.svingel.PP <- subset(PP.data.sort, Dansk.navn == "R?d svingel")

rod.svingel.PP


rod.svingel.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$R?d.svingel_v,
                               dist.b = dist.data$R?d.svingel_b)

rod.svingel.dist$dist <- ifelse(rod.svingel.dist$dist.v < rod.svingel.dist$dist.b,
                                rod.svingel.dist$dist.v,
                                rod.svingel.dist$dist.b)

rod.svingel.dist <- subset(rod.svingel.dist, dist < 500)

rod.svingel.dist <- subset(rod.svingel.dist, Plot != "73C13D2C")

rod.svingel.dist

hist(rod.svingel.PP$T?thed)

hist(rod.svingel.dist$dist)

rod.svingel.test <- cor.test(rod.svingel.PP$T?thed, rod.svingel.dist$dist, method = "spearman")

test.est <- rod.svingel.test$estimate

test.est[1,]

## r?d svingel
## testen er sigifikant (9.228e-05) og de er korreleret
#rho = -0,89

p.val <- rod.svingel.test$p.value
est <- rod.svingel.test$estimate

rod.svingel.data <- data.frame(Art = "R?d svingel",
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

hist(lyng.snerre.PP$T?thed)

hist(lyng.snerre.dist$dist)

ifelse(lyng.snerre.PP$Plot == lyng.snerre.dist$Plot, "Yes", "No") 

lyng.snerre.test <- cor.test(lyng.snerre.PP$T?thed, lyng.snerre.dist$dist, 
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

alm.mj?durt.PP <- subset(PP.data.sort, Dansk.navn == "Almindelig mj?durt")

alm.mj?durt.PP

alm.mj?durt.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$Alm.mj?durt_v,
                               dist.b = dist.data$Alm.mj?durt_b)

alm.mj?durt.dist$dist <- ifelse(alm.mj?durt.dist$dist.v < alm.mj?durt.dist$dist.b,
                                alm.mj?durt.dist$dist.v,
                                alm.mj?durt.dist$dist.b)

alm.mj?durt.dist <- subset(alm.mj?durt.dist, dist < 500)

hist(alm.mj?durt.PP$T?thed)

hist(alm.mj?durt.dist$dist)

ifelse(alm.mj?durt.PP$Plot == alm.mj?durt.dist$Plot, "Yes", "No") 

alm.mj?durt.test <- cor.test(alm.mj?durt.PP$T?thed, alm.mj?durt.dist$dist, 
         method = "spearman")

## almindelig mj?durt
## testen er IKKE signikikant (0,15) og de er korreleret
#rho = -0,66

p.val <- alm.mj?durt.test$p.value
est <- alm.mj?durt.test$estimate

alm.mj?durt.data <- data.frame(Art = "Almindelig mj?durt",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(alm.mj?durt.PP$Plot))

test.data <- rbind(test.data, alm.mj?durt.data)

test.data

#################################################

r?d.gran.PP <- subset(PP.data.sort, Dansk.navn == "R?d-gran")

r?d.gran.PP

r?d.gran.dist <- data.frame(Plot = dist.data$Plot,
                               dist.v = dist.data$R?dgran_v,
                               dist.b = dist.data$R?dgran_b)

r?d.gran.dist$dist <- ifelse(r?d.gran.dist$dist.v < r?d.gran.dist$dist.b,
                                r?d.gran.dist$dist.v,
                                r?d.gran.dist$dist.b)

r?d.gran.dist <- subset(r?d.gran.dist, dist < 500)

hist(r?d.gran.PP$T?thed)

hist(r?d.gran.dist$dist)

ifelse(r?d.gran.PP$Plot == r?d.gran.dist$Plot, "Yes", "No") 

r?d.gran.test <- cor.test(r?d.gran.PP$T?thed, r?d.gran.dist$dist, 
         method = "spearman")

## r?d-gran
## testen er sigifikant (0.0034) og de er korreleret
#rho = -0,58

p.val <- r?d.gran.test$p.value
est <- r?d.gran.test$estimate

r?d.gran.data <- data.frame(Art = "R?d-gran",
                               korrelation = est,
                               p.val = p.val,
                               observationer = length(r?d.gran.PP$Plot))

test.data <- rbind(test.data, r?d.gran.data)

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


hist(alm.syre.PP$T?thed)

hist(alm.syre.dist$dist)

ifelse(alm.syre.PP$Plot == alm.syre.dist$Plot, "Yes", "No") 

alm.syre.test <- cor.test(alm.syre.PP$T?thed, alm.syre.dist$dist, 
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

hist(alm.star.PP$T?thed)

hist(alm.star.dist$dist)

ifelse(alm.star.PP$Plot == alm.star.dist$Plot, "Yes", "No") 

alm.star.test <- cor.test(alm.star.PP$T?thed, alm.star.dist$dist, 
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

hist(hedelyng.PP$T?thed)

hist(hedelyng.dist$dist)

ifelse(hedelyng.PP$Plot == hedelyng.dist$Plot, "Yes", "No") 

hedelyng.test <- cor.test(hedelyng.PP$T?thed, hedelyng.dist$dist, 
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

kattesk?g.PP <- subset(PP.data.sort, Dansk.navn == "Kattesk?g")

kattesk?g.PP

kattesk?g.dist <- data.frame(Plot = dist.data$Plot,
                                dist.v = dist.data$Kattesk?g_v,
                                dist.b = dist.data$Kattesk?g_b)

kattesk?g.dist$dist <- ifelse(kattesk?g.dist$dist.v < kattesk?g.dist$dist.b,
                                 kattesk?g.dist$dist.v,
                                 kattesk?g.dist$dist.b)

kattesk?g.dist <- subset(kattesk?g.dist, dist < 500)

hist(kattesk?g.PP$T?thed)

hist(kattesk?g.dist$dist)

ifelse(kattesk?g.PP$Plot == kattesk?g.dist$Plot, "Yes", "No")

kattesk?g.test <- cor.test(kattesk?g.PP$T?thed, kattesk?g.dist$dist, 
         method = "spearman")

## Kattesk?g
## testen er signifikant (0,02247) og korreleret
## rho = -0,68

p.val <- kattesk?g.test$p.value
est <- kattesk?g.test$estimate

kattesk?g.data <- data.frame(Art = "Kattesk?g",
                            korrelation = est,
                            p.val = p.val,
                            observationer = length(kattesk?g.PP$Plot))

test.data <- rbind(test.data, kattesk?g.data)

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

hist(alm.hvene.PP$T?thed)

hist(alm.hvene.dist$dist)

ifelse(alm.hvene.PP$Plot == alm.hvene.dist$Plot, "Yes", "No") 

alm.hvene.test <- cor.test(alm.hvene.PP$T?thed, alm.hvene.dist$dist, 
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

sump.k?l.PP <- subset(PP.data.sort, Dansk.navn == "Sump-k?llingetand")

sump.k?l.PP

sump.k?l.dist <- data.frame(Plot = dist.data$Plot,
                                dist.v = dist.data$Sump.k?llingetand_v,
                                dist.b = dist.data$Sump.k?llingetand_b)

sump.k?l.dist$dist <- ifelse(sump.k?l.dist$dist.v < sump.k?l.dist$dist.b,
                                 sump.k?l.dist$dist.v,
                                 sump.k?l.dist$dist.b)

sump.k?l.dist <- subset(sump.k?l.dist, dist < 500)

hist(sump.k?l.PP$T?thed)

hist(sump.k?l.dist$dist)

ifelse(sump.k?l.PP$Plot == sump.k?l.dist$Plot, "Yes", "No") 

sump.k?l.test <- cor.test(sump.k?l.PP$T?thed, sump.k?l.dist$dist, 
         method = "spearman")

length(sump.k?l.PP$Plot)

## Sump-k?llingetand
## testen er ikke sigifikant (0.07221) og de er korreleret
#rho = -0.77
## 6 observationer

p.val <- sump.k?l.test$p.value
est <- sump.k?l.test$estimate

sump.k?l.data <- data.frame(Art = "Sump-k?llingetand",
                            korrelation = est,
                            p.val = p.val,
                            observationer = length(sump.k?l.PP$Plot))

test.data <- rbind(test.data, sump.k?l.data)

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







