# Solvency2 Abschlussprojekt

# set workspace
setwd("/home/fred/Documents/workspace/Solvency2/projekt/")

# zuerst xls in csv-format abspeichern
# dann daten einlesen
Bestand <- read.csv("Bestand.csv", header = TRUE)
VorlesungTafeln <- read.csv("VorlesungTafeln.csv", header = TRUE)
Zinsstrukturkurven <- read.csv("Zinsstrukturkurven.csv", header = TRUE)

# sortierung nach alter
selalter<-Bestand[which(Bestand$x==30 & Bestand$t==4),]
selEW<-VorlesungTafeln[which(VorlesungTafeln$Alter.x==30),]

# kx kopfschaden
# qx sterberate
# wx stornorate
# px eintrittswahrscheinlichkeit pflegestufe