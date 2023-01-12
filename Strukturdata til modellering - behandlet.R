#Forberedelse af strukturdata

#####################################

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar")

list.files()

struktur.data <- read.csv("strukturdata til modellering.csv",
                          sep = ";",
                          header = T)

names(struktur.data)



######## Krondække #########

Krondække.N <- struktur.data$Krondække.N*1.04
Krondække.S <- struktur.data$Krondække.S*1.04
Krondække.V <- struktur.data$Krondække.V*1.04
Krondække.Ø <- struktur.data$Krondække.Ø*1.04

Krondække.df <- data.frame(Krondække.N, 
                           Krondække.S,
                           Krondække.V,
                           Krondække.Ø)

Krondække.total <- rowMeans(Krondække.df)

struktur.data$Krondække.total <- Krondække.total


########## Førnedybde #######

Førnedybde.PP.df <- data.frame(struktur.data$Førnedybde.PP.1,
                               struktur.data$Førnedybde.PP.2,
                               struktur.data$Førnedybde.PP.3,
                               struktur.data$Førnedybde.PP.4)

Førnedybde.5m.df <- data.frame(struktur.data$Førnedybde.5m.1,
                               struktur.data$Førnedybde.5m.2,
                               struktur.data$Førnedybde.5m.3,
                               struktur.data$Førnedybde.5m.4)



Førnedybde.df <- data.frame(Førnedybde.PP.total = rowMeans(Førnedybde.PP.df),
                            Førnedybde.5m.total = rowMeans(Førnedybde.5m.df))


Førnedybde.total <- rowMeans(Førnedybde.df)

struktur.data$Førnedybde.total <- Førnedybde.total


########## Vegetationshøjde #############

Vegetationshøjde.df <- data.frame(struktur.data$Vegetationshøjde.1,
                                  struktur.data$Vegetationshøjde.2,
                                  struktur.data$Vegetationshøjde.3,
                                  struktur.data$Vegetationshøjde.4)


Vegetationshøjde.total <- rowMeans(Vegetationshøjde.df)

struktur.data$Vegetationshøjde.total <- Vegetationshøjde.total

names(struktur.data)

################################################

struktur.data[ ,c("Krondække.N",
                  "Krondække.S",
                  "Krondække.V",
                  "Krondække.Ø",
                  "Førnedybde.PP.1",
                  "Førnedybde.PP.2",
                  "Førnedybde.PP.3",
                  "Førnedybde.PP.4",
                  "Førnedybde.5m.1",
                  "Førnedybde.5m.2",
                  "Førnedybde.5m.3",
                  "Førnedybde.5m.4",
                  "Vegetationshøjde.1",
                  "Vegetationshøjde.2",
                  "Vegetationshøjde.3",
                  "Vegetationshøjde.4")] <- list(NULL)

names(struktur.data)

#######################################################

write.csv(struktur.data, "Strukturdata modellering - behandlet - 17.05.22.csv")

##### End of script