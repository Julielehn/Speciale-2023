library(readxl)
library(dplyr)
library(tidyr)

##################################################

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale")

list.files()

##################################################

# datafil, som indholder distance data indlæses
data <- read.csv("Distance data - Behandlet.csv",
         header = TRUE,
         sep = ";")

head(data)

length(data)


# Deler data op - plot og dato i et dataframe og distancemål i en anden dataframe. 
Plot <- data[,1]
Dist.data <- data[,3:449]

# Distance data konverteres
Dist.data <- 1-(Dist.data)^2/(500)^2

# Distance data splittes op igen - Så distance data relateret til planter findes
# i en data frame, og distance data relateret til andre ressourcer findes i en 
# anden dataframe

Plante.data <- Dist.data[,c(24:ncol(Dist.data))]
Ressource.data <- Dist.data[,c(1:23)]

Ressource.data <- cbind(Plot, Ressource.data)

Ressource.data


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Omdan plantedata til lang format #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Blomster data og vegetetive data skildes ad og ordnes hvert for sig. Hvorefter
# de sættes sammen igen

Blomst.data <- Plante.data[,c(FALSE, TRUE)]

colnames(Blomst.data) <- gsub("_b","",colnames(Blomst.data))

Blomst.data <- cbind(Plot, Blomst.data)

Blomst.data <- gather(Blomst.data, 
                      Dansk.navn, 
                      Blomster.SS, 
                      Tormentil:ncol(Blomst.data), 
                      factor_key = TRUE)


veg.data <- Plante.data[,c(T, F)]

colnames(veg.data) <- gsub("_v","",colnames(veg.data))

veg.data <- cbind(Plot, veg.data)

veg.data <- gather(veg.data, 
                   Dansk.navn, 
                   Vegetativ.SS, 
                   Tormentil:ncol(veg.data), 
                   factor_key = TRUE)

Plante.data.long <- cbind(Blomst.data, veg.data)

Plante.data.long <- Plante.data.long[, c("Plot", 
                               "Dansk.navn",
                               "Blomster.SS",
                               "Vegetativ.SS")]

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### indsæt data om værtsplante og nektarforhold ###########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Indlæs værtsplante/nektar data, som merge det sammen med det resterende data
vært.nektar <- read.csv("plante_arter_dist.csv",
                        header = TRUE,
                        sep = ";")


Plante.data.long <- merge(Plante.data.long, 
                     vært.nektar, 
                     by = "Dansk.navn", 
                     all.x = T)

Plante.data[is.na(Plante.data)] <- 0

Plante.data.long <- Plante.data.long[, c("Plot", 
                               "Dansk.navn",
                               "Blomster.SS",
                               "Vegetativ.SS",
                               "Nektar")]


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Ny variabel - sansynlighed for nektar/pollen ##########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Vi er kun interesseret i de observationer (Rækker), hvor der er observationer
# af værtsplanter. Derfor subsetter vi for dette. For at være sikker på, at alle
# plots stadig er repræsenteret skal merges med en dataframe, som indeholder
# alle plotnavne

Plot.id <- data.frame(Plot = unique(Plante.data.long[,"Plot"]))
Nektar.SS <- subset(Plante.data.long, Nektar == 1)

Nektar.SS <- merge(Plot.id, Nektar.SS, by = "Plot", all.x = T)

length(unique(Nektar.SS$Plot))

Nektar.SS[is.na(Nektar.SS)] <- 0

#nu udregnes den gennemsnitlige sandsynlighed for hver planteart for hver 
#habitatstyper
# der startes ud med en tom dataframe
Nektar.SS.M <- data.frame()

#og laver en vektor med alle plotnavne
habitat.list <- unique(Nektar.SS$Habitatskode)
habitat.list

art.liste <- unique(Nektar.SS$Dansk.navn)
art.liste

# For loop, som finder summen af tætheden for alle værtsplanter, for hvert
#seperat plot
for (i in art.liste){
    temp.df <- subset(Nektar.SS, Dansk.navn == i)
    temp.df2 <- data.frame(Dansk.navn = i,
                           IOP.blomst = mean(temp.df$Blomster.SS),
                           IOP.veg = mean(temp.df$Vegetativ.SS))
    Nektar.SS.M <- rbind(Nektar.SS.M, temp.df2)
}

Nektar.SS.M

# ny kolonne med ratio mellem blomstrende og ikke blomsterende individer
Nektar.SS.M$Ratio <- Nektar.SS.M$IOP.blomst/Nektar.SS.M$IOP.veg

# data rundes to heholdsvis 3 og 1 decimaler
Nektar.SS.M[,2:3] <- round(Nektar.SS.M[,2:3], digits = 3)

Nektar.SS.M[,4] <- round(Nektar.SS.M[,4], digits = 1)

write.csv(Nektar.SS.M, "Nektarplante.blomst.veg.ratio.csv")






