

library(rdwd)

# Datenset auswaehlen
link <- selectDWD("wuerzburg", res="daily", var= "kl", per="recent")

# Download
file <- dataDWD(link, read=FALSE)

# Auslesen
clim <- readDWD(file, varnames=TRUE)



tage <- 18



# Passende Vektoren "extrahieren"
zeitraum <- clim[c((length(clim$MESS_DATUM)-tage):length(clim$MESS_DATUM)),c(2)]
niederschlagshoehe <- clim[c((length(clim$MESS_DATUM)-tage):length(clim$MESS_DATUM)),c(7)]


# Dataframe mit benoetigten Infos erstellen
niederschlag <- data.frame(zeitraum, niederschlagshoehe)


##### Balkendiagramm Niederschlagshoehe
barplot(niederschlag$niederschlagshoehe, main = "Niederschlagshoehe", names.arg = niederschlag$zeitraum, col = "lightblue")


# "schöneres" Datum
betterDate <- format(niederschlag$zeitraum, format = "%a,%d.%m")
niederschlag <- cbind(niederschlag, betterDate)


##### Balkendiagramm Niederschlagshoehe
barplot(niederschlag$niederschlagshoehe, main = "Niederschlagshoehe", names.arg = niederschlag$betterDate, col = "lightblue")




####################################################################################################

library(rdwd)

regenbericht <- function(ort, zeitraum){
  
  # Datenset auswaehlen
  link <- selectDWD(ort, res="daily", var= "kl", per="recent")
  
  # Actually download that dataset, returning the local storage file name:
  file <- dataDWD(link, read=FALSE)
  
  # Read the file from the zip folder:
  clim <- readDWD(file, varnames=TRUE)
  
  
  tage <- zeitraum
  
  
  
  zeitraum <- clim[c((length(clim$MESS_DATUM)-tage):length(clim$MESS_DATUM)),c(2)]
  niederschlagshoehe <- clim[c((length(clim$MESS_DATUM)-tage):length(clim$MESS_DATUM)),c(7)]
  
  
  niederschlag <- data.frame(zeitraum, niederschlagshoehe)
  
  
  betterDate <- format(niederschlag$zeitraum, format = "%a,%d.%m")
  niederschlag <- cbind(niederschlag, betterDate)
  
  ##### Niederschlagshoehe
  barplot(niederschlag$niederschlagshoehe, main = "Niederschlagshoehe", names.arg = niederschlag$betterDate, col = "lightgreen")
  
}
