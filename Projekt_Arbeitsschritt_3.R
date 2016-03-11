# Solvency2 Abschlussprojekt
# 3.2, Arbeitsschritt III

# set workspace
setwd("/home/fred/Documents/workspace/Solvency2/projekt/")

# lade Tabellen
source("/home/fred/Documents/workspace/Solvency2/projekt/Projekt_DatenEinlesen.R")

# definiere benötigte Variablen
AgeMax = length(Tafeln$x)

# baue Eintrittstabelle mit Werten für Eintritt in die jeweilige Pflegestufe und Sterbealter
Eintrittstabelle <- matrix(nrow = 4,ncol=length(Bestand$x),byrow=TRUE)
rownames(Eintrittstabelle) <- c('clientDiesAt','clientInPST3At','clientInPST2At','clientInPST1At')

# Algoritmus zum befüllen der Eintrittstabelle
for (i in 1:ncol(Eintrittstabelle)) {
  Eintrittstabelle[1,i] = AgeMax
  Eintrittstabelle[2,i] = AgeMax+1
  Eintrittstabelle[3,i] = AgeMax+1
  Eintrittstabelle[4,i] = AgeMax+1
  for (j in (Bestand$x[i] + Bestand$t[i]):AgeMax) {
    #cat(j)
    #cat('\n')
    if (runif(1,0,1) <= Tafeln$qx_m[j]) {
      Eintrittstabelle[1,i] = j
      break
    }
    if (runif(1,0,1) <= Tafeln$Px_III[j]) {
      Eintrittstabelle[2,i] = j
    }
    if (runif(1,0,1) <= Tafeln$Px_II[j] && j < Eintrittstabelle[2,i]) {
      Eintrittstabelle[3,i] = j
    }
    if (runif(1,0,1) <= Tafeln$Px_I[j] && j < Eintrittstabelle[2,i] && j < Eintrittstabelle[3,i]) {
      Eintrittstabelle[4,i] = j
    }
    
  }
}