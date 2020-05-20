


kundenEigenschaften <- read.csv("E:/OneDrive/Studium/EC Hauptseminar/Seminararbeit/Datensaetze/KundenEigenschaften.csv", header = TRUE, sep=";")
kaufRegenschirm <- read.csv("E:/OneDrive/Studium/EC Hauptseminar/Seminararbeit/Datensaetze/KaufRegenschirm.csv", header = TRUE, sep=";")



#Merge
kundenMitRegenschirm <- merge(kundenEigenschaften, kaufRegenschirm, by = "id_kunde", all.x = TRUE)


#Subset
kundenMitRegenschirm <- subset(kundenMitRegenschirm, !is.na(kundenMitRegenschirm$datum_kauf))


#Aggragation
schirmeNachGeschlecht <- aggregate(as.matrix(kundenMitRegenschirm$anzahl) ~ kundenMitRegenschirm$geschlecht, FUN = sum)




schirmeNachGeschlecht <- data.frame(geschlecht = schirmeNachGeschlecht$`kundenMitRegenschirm$geschlecht`, anzahl = schirmeNachGeschlecht$V1)
schirmeNachGeschlecht$geschlecht <- as.character(schirmeNachGeschlecht$geschlecht)
schirmeNachGeschlecht$anzahl <- as.numeric(schirmeNachGeschlecht$anzahl)


#Kuchendiagramm
slices <- schirmeNachGeschlecht$anzahl
lbls <- paste(schirmeNachGeschlecht$geschlecht,": ",schirmeNachGeschlecht$anzahl,sep="")
pie(slices, labels = lbls, main= "Anzahl verkaufter Schirme nach Geschlecht", col = rainbow(length(lbls)))