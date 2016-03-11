# Solvency2 Abschlussprojekt
# Daten einlesen

# set workspace
setwd("/home/fred/Documents/workspace/Solvency2/projekt/")

# xls- in csv-dateien konvertieren
# daten einlesen
Bestand <- read.csv("BestandTest.csv", header = TRUE)
Tafeln <- read.csv("Tafeln.csv", header = TRUE)
#Zinsstrukturkurven <- read.csv("Zinsstrukturkurven.csv", header = TRUE)