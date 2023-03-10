#Forberedelse af strukturdata

#####################################

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar")

list.files()

struktur.data <- read.csv("strukturdata til modellering.csv",
                          sep = ";",
                          header = T)

names(struktur.data)



######## Krond?kke #########

Krond?kke.N <- struktur.data$Krond?kke.N*1.04
Krond?kke.S <- struktur.data$Krond?kke.S*1.04
Krond?kke.V <- struktur.data$Krond?kke.V*1.04
Krond?kke.? <- struktur.data$Krond?kke.?*1.04

Krond?kke.df <- data.frame(Krond?kke.N, 
                           Krond?kke.S,
                           Krond?kke.V,
                           Krond?kke.?)

Krond?kke.total <- rowMeans(Krond?kke.df)

struktur.data$Krond?kke.total <- Krond?kke.total


########## F?rnedybde #######

F?rnedybde.PP.df <- data.frame(struktur.data$F?rnedybde.PP.1,
                               struktur.data$F?rnedybde.PP.2,
                               struktur.data$F?rnedybde.PP.3,
                               struktur.data$F?rnedybde.PP.4)

F?rnedybde.5m.df <- data.frame(struktur.data$F?rnedybde.5m.1,
                               struktur.data$F?rnedybde.5m.2,
                               struktur.data$F?rnedybde.5m.3,
                               struktur.data$F?rnedybde.5m.4)



F?rnedybde.df <- data.frame(F?rnedybde.PP.total = rowMeans(F?rnedybde.PP.df),
                            F?rnedybde.5m.total = rowMeans(F?rnedybde.5m.df))


F?rnedybde.total <- rowMeans(F?rnedybde.df)

struktur.data$F?rnedybde.total <- F?rnedybde.total


########## Vegetationsh?jde #############

Vegetationsh?jde.df <- data.frame(struktur.data$Vegetationsh?jde.1,
                                  struktur.data$Vegetationsh?jde.2,
                                  struktur.data$Vegetationsh?jde.3,
                                  struktur.data$Vegetationsh?jde.4)


Vegetationsh?jde.total <- rowMeans(Vegetationsh?jde.df)

struktur.data$Vegetationsh?jde.total <- Vegetationsh?jde.total

names(struktur.data)

################################################

struktur.data[ ,c("Krond?kke.N",
                  "Krond?kke.S",
                  "Krond?kke.V",
                  "Krond?kke.?",
                  "F?rnedybde.PP.1",
                  "F?rnedybde.PP.2",
                  "F?rnedybde.PP.3",
                  "F?rnedybde.PP.4",
                  "F?rnedybde.5m.1",
                  "F?rnedybde.5m.2",
                  "F?rnedybde.5m.3",
                  "F?rnedybde.5m.4",
                  "Vegetationsh?jde.1",
                  "Vegetationsh?jde.2",
                  "Vegetationsh?jde.3",
                  "Vegetationsh?jde.4")] <- list(NULL)

names(struktur.data)

#######################################################

write.csv(struktur.data, "Strukturdata modellering - behandlet - 17.05.22.csv")

##### End of script