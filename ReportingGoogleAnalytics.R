
# Importieren von Google Analytics Daten im Format .csv
quelldaten <- read.csv("E:/OneDrive/Studium/EC Hauptseminar/Seminararbeit/ga_data.csv", header = TRUE, sep=";")

# Bildung eines Dataframes aus den relevanten Spalten
schoener <- data.frame(quelle = quelldaten$source,datum = quelldaten$date,
                       sessionDuration = quelldaten$avgSessionDuration)

# Bildung eines Subsets: Dataframe wird aus Zeilen gebildet, deren sessionDuration-Wert > 0 ist
nochSchoener <- subset(schoener, schoener$sessionDuration != "0")




# Funktion, die die durchschnittliche sessionDuration pro Monat über den ganzen Datensatz berechnet
# und ein entsprechendes Diagramm erstellt
durationTimeUebersicht <- function(){

  # Bildung eines Dataframes aus den relevanten Spalten
  allData <- data.frame(datum = nochSchoener$datum, sessionDuration = nochSchoener$sessionDuration)
  
  # Datum sortieren
  allData$datum <- lubridate::dmy(allData$datum)
  dplyr::arrange(allData, datum)
  
  # Jedes Datum mit dem gleichen Jahres- und Monatswert auf den 15. Tag des Monats setzen
  # => Vorbereitung für Datenaggregation
  allData$datum <- format(allData$datum,"%Y.%m.15")
  
  # vom Datentyp factor zu date konvertieren 
  allData$datum <- as.Date(allData$datum, format = "%Y.%m.%d")
  
  # vom Datentyp factor zu numeric konvertieren
  sessionDuration <- as.numeric(allData$sessionDuration)
  
  # Bildung eines Dataframes aus den relevanten Spalten
  allData <- data.frame(datum = allData$datum, sessionDuration)
  
  # Aggregation der Daten: Basierend auf dem Datum wird die durchschnittliche sessionDuration berechnet
  allData <- aggregate(as.matrix(allData$sessionDuration) ~ allData$datum, FUN = mean)
  
  # Erzeugen eines Diagramms und einer Linie, die die durchschnittliche sessionDuration insgesamt zeigt
  plot(allData$`allData$datum` , allData$V1, type = "l", xlab = "Monat", ylab = "avgDurationTime",
      ylim = c(0,400), main = "DurationTime Uebersicht ", col= "black")
  
  # Anzeige von Gitterlinien im Diagramm
  grid()
  
  # Erzeugung einer Legende für das Diagramm
  legend("bottom", c("insgesamt","Instagram","direct","Jimdo","Google","IGShoping"),
         col = c("black","purple","green","blue","red","orange"), pch=15,
         cex = 0.8, bty = "n", horiz = TRUE)
}


# Funktion zum Hinzufuegen einer Linie im Diagramm, diedie durchschnittliche sessionDuration pro Monat
# einer bestimmten Quelle darstellt
addLine <-function(eingabe){

  # Bei Funktionsaufruf "addLine("Instagram")" oder "addLine("instagram")"
  # wird source auf den Wert "l.instagram.com" und color den Wert "purple" gesetzt
  if(eingabe == "Instagram" | eingabe == "instagram"){
    source <- "l.instagram.com"
    color <- "purple"}
  
  if(eingabe == "Direct" | eingabe == "direct"){
    source <- "(direct)"
    color <- "green"}
  
  if(eingabe == "Jimbo" | eingabe == "jimdo"){
    source <- "cms.e.jimdo.com"
    color <- "blue"}
  
  if(eingabe == "Google" | eingabe == "google"){
    source <- "google"
    color <- "red"}
  
  if(eingabe == "IGShoping" | eingabe == "igshoping"){
    source <- "IGShopping"
    color <- "orange"}
  
  # Dataframe wird aus Zeilen gebildet, deren Wert in nochSchoener$quelle
  # mit dem String-Wert von source uebereinstimmen
  subset <- subset(nochSchoener, nochSchoener$quelle  == source)
  subset <- data.frame(datum = subset$datum, sessionDuration = subset$sessionDuration)
  
  # Datum sortieren
  subset$datum <- lubridate::dmy(subset$datum)
  dplyr::arrange(subset, datum)
  
  # Jedes Datum mit dem gleichen Jahres- und Monatswert auf den 15. Tag des Monats setzen
  # => Vorbereitung für Datenaggregation
  subset$datum <- format(subset$datum,"%Y.%m.15")
  
  # vom Datentyp factor zu date konvertieren
  subset$datum <- as.Date(subset$datum, format = "%Y.%m.%d")
  
  # vom Datentyp factor zu numeric konvertieren
  sessionDuration <- as.numeric(subset$sessionDuration)
  
  # Bildung eines Dataframes aus den relevanten Spalten
  subset <- data.frame(datum = subset$datum, sessionDuration)
  
  # Aggregation der Daten: Basierend auf dem Datum wird die durchschnittliche sessionDuration berechnet
  subset <- aggregate(as.matrix(subset$sessionDuration) ~ subset$datum, FUN = mean)

  # Hinzufuegen der Linie
  points(subset$`subset$datum`,subset$V1, type = "l", col = color)
}
