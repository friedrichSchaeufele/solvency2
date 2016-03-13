#######################################################################################
#######################################################################################
####     Solvency2 Abschlussprojekt              ######################################
####     plug together                           ######################################
#######################################################################################
#######################################################################################

#######################################################################################
### set workspace
#######################################################################################
setwd("C:/Users/maja/Downloads/SolvencyProject/solvency2")

#######################################################################################
### daten einlesen
#######################################################################################
Bestand <- read.csv("BestandTest.csv", header = TRUE)
#Bestand <- read.csv("Bestand.csv", header = TRUE)
Tafeln <- read.csv("Tafeln.csv", header = TRUE)
Zinsstrukturkurven <- read.csv("Zinsstrukturkurven.csv", header = TRUE)

#######################################################################################
### Variablen
#######################################################################################

# maximal erreichbares Alter
AgeMax = length(Tafeln$x)

# Variable zur wahl der Zinsstrukturkurve fuer die Abzinsung
# ZinsSK = 1 = RFnoVA
# ZinsSK = 2 = RFVA
ZinsSK = 1

# Lamdas fuer tPx
# to do

#######################################################################################
### erstelle und lese Abzinsungstabelle ein
#######################################################################################

source('Projekt_abzinsungstabellen.R',local = TRUE)

#######################################################################################
### lese Arbeitsschritt 1 ein - Funktionen und Ausgabe fuer tPx
#######################################################################################

source('Projekt_Arbeitsschritt_1.R',local = TRUE)
#create tPx Matrix with specific lambdas
tPx_gesamt_Matrix <- create_tPx_gesamt_Matrix(1,1)

#######################################################################################
### lese Arbeitsschritt 2 ein - Funktionen und Ausgabe fuer tVx
#######################################################################################

source('Projekt_Arbeitsschritt_2.R',local = TRUE)

#######################################################################################
### Simulation - Vorbereitungen und Start
### Arbeitsschritt 3 und 4
#######################################################################################

# Anzahl Simulationen
n = 1

# EWR-Matrix zum abspeichern der berrechneten EWR-Werte
EWRMatrix = matrix(nrow = 1,ncol=n,byrow=TRUE)

# starte berechnungen
for (p in 1:n) {
  
# baue Eintrittstabelle mit Werten für Eintritt in die jeweilige Pflegestufe und Sterbealter
  # Eintrittstabelle <- matrix(nrow = 4,ncol=length(Bestand$x),byrow=TRUE)
Eintrittstabelle <- matrix(nrow = 5,ncol=length(Bestand$x),byrow=TRUE)
  # rownames(Eintrittstabelle) <- c('clientDiesAt','clientInPST3At','clientInPST2At','clientInPST1At')
rownames(Eintrittstabelle) <- c('clientDiesAt','clientInPST3At','clientInPST2At','clientInPST1At','StornoAt')

# Algoritmus für das Befüllen der Eintrittstabelle
for (i in 1:ncol(Eintrittstabelle)) {
  Eintrittstabelle[1,i] = AgeMax
  Eintrittstabelle[2,i] = AgeMax+1
  Eintrittstabelle[3,i] = AgeMax+1
  Eintrittstabelle[4,i] = AgeMax+1
  Eintrittstabelle[5,i] = AgeMax+1
  for (j in (Bestand$x[i] + Bestand$t[i]):AgeMax) {
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
    if (runif(1,0,1) <= Tafeln$wx_m[j] && j < Eintrittstabelle[2,i] && j < Eintrittstabelle[3,i] && j < Eintrittstabelle[4,i]) {
      Eintrittstabelle[5,i] = j
    }
  }
}

#######################################################################################
### Erwartungswert berechnen
#######################################################################################
# EWR = E[SUM_{j=0}^{w}(Ausgaben - Einnahmen)*(Abzinsungsfaktor)]
# Ausgaben = Sum(q*L)
# q = 1, 0.7, 0.4 je nach Pflegestufe
# w = todesalter oder storno
# berechne Ausgaben/Einnahmen pro Klienten und fasse am Schluss zusammen

EWR = 0
for (i in 1:ncol(Eintrittstabelle)) {
  
  # aktuelles alter 
  actAge = Bestand$x[i]+Bestand$t[i]
    #cat('actage = ')
    #cat(actAge)
    #cat('\n')
  
  # buffer für storno
  storno = FALSE
  
  # berechne w = Alter bei dem Ein- und Auszahlungen enden
  endAge = Eintrittstabelle[1,i]
  if (Eintrittstabelle[5,i] < Eintrittstabelle[1,i]) {
    endAge = Eintrittstabelle[5,i]
    storno = TRUE
  }
    #cat('endAge = ')
    #cat(endAge)
    #cat('\n')

  # years to maturity
  w = endAge - actAge

    #cat('w = ')
    #cat(w)
    #cat('\n')
  
  
  # erstelle vektor q für schadenklasse
  if (w>0) {
    q <- rep(0, w)
    #cat('q = ')
    #cat(q)
    #cat('\n')
  }
  else{
    q = 0
  }
  
  # years to pstI
  ytPST1 = Eintrittstabelle[4,i] - actAge
  if (ytPST1 < w && storno == FALSE) {
    for (k in 1:(w-ytPST1)) {
      q[ytPST1+k] = 0.4
    }
  }
    #cat('ytPST1 = ')
    #cat(ytPST1)
    #cat('\n')
  
  
  # years to pstII
  ytPST2 = Eintrittstabelle[3,i] - actAge
  if (ytPST2 < w && storno == FALSE) {
    for (k in 1:(w-ytPST2)) {
      q[ytPST2+k] = 0.7
    }
  }
    #cat('ytPST2 = ')
    #cat(ytPST2)
    #cat('\n')
  
  
  # years to pst3
  ytPST3 = Eintrittstabelle[2,i] - actAge
  if (ytPST3 < w && storno == FALSE) {
    for (k in 1:(w-ytPST3)) {
      q[ytPST3+k] = 1
    }
  }
    #cat('ytPST3 = ')
    #cat(ytPST3)
    #cat('\n')
    
    #cat('q before = ')
    #cat(q)
    #cat('\n')
  
  # Ausgaben = sum(L*q*abzinsungsfaktor)
  # q * abzVek
  for (k in 1:length(q)) {
    if (k <= ncol(Abzinsungstabelle)) {
      q[k] = q[k]*Abzinsungstabelle[ZinsSK,k]
    }
    if (k > ncol(Abzinsungstabelle)) {
      q[k] = q[k] * Abzinsungstabelle[ZinsSK,ncol(Abzinsungstabelle)]
    }
  }
    #cat('q = ')
    #cat(q)
    #cat('\n')
  
    #cat(sum(Bestand$L[i]*q))
    #cat('\n')
  

  EWR = EWR+12*sum(Bestand$L[i]*q)
}
EWRMatrix[p]=EWR

}
