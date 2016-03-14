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
#Bestand <- read.csv("BestandTest.csv", header = TRUE)
Bestand <- read.csv("Bestand.csv", header = TRUE)
#Tafeln <- read.csv("Tafeln.csv", header = TRUE)
Tafeln <- read.csv("TafelnAktualisiert.csv", header = TRUE)
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

# Anzahl Simulationen
n = 15

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

Deckungsrueckstellung = sum(t_V_x_einzeln_vec)

Deckungsrueckstellung_einnahmen_seit_Versicherungsbeginn = P_x_PST_einzeln_vec
for (i in 1:length(Deckungsrueckstellung_einnahmen_seit_Versicherungsbeginn)) {
  Deckungsrueckstellung_einnahmen_seit_Versicherungsbeginn[i] = Deckungsrueckstellung_einnahmen_seit_Versicherungsbeginn[i]*Bestand$t[i]
}
sum(Deckungsrueckstellung_einnahmen_seit_Versicherungsbeginn)
#######################################################################################
### Simulation - Vorbereitungen und Start
### Arbeitsschritt 3 und 4
#######################################################################################

# EWR-Matrix zum abspeichern der berrechneten EWR-Werte
# EWR_vec = matrix(nrow = 1,ncol=n,byrow=TRUE)
EWR_vec = rep(0,n)

system.time(
# starte berechnungen
for (p in 1:n) {
  # baue Eintrittstabelle mit Werten für Eintritt in die jeweilige Pflegestufe und Sterbealter
  # Eintrittstabelle <- matrix(nrow = 4,ncol=length(Bestand$x),byrow=TRUE)
  Eintrittstabelle <-
    matrix(nrow = 5,
           ncol = length(Bestand$x),
           byrow = TRUE)
  # rownames(Eintrittstabelle) <- c('clientDiesAt','clientInPST3At','clientInPST2At','clientInPST1At')
  rownames(Eintrittstabelle) <-
    c('clientDiesAt',
      'clientInPST3At',
      'clientInPST2At',
      'clientInPST1At',
      'StornoAt')
  
  # Algoritmus für das Befüllen der Eintrittstabelle
  for (i in 1:ncol(Eintrittstabelle)) {
    Eintrittstabelle[1, i] = AgeMax
    Eintrittstabelle[2, i] = AgeMax + 1
    Eintrittstabelle[3, i] = AgeMax + 1
    Eintrittstabelle[4, i] = AgeMax + 1
    Eintrittstabelle[5, i] = AgeMax + 1
    for (j in (Bestand$x[i] + Bestand$t[i]):AgeMax) {
      if (runif(1, 0, 1) <= Tafeln$qx_m[j]) {
        Eintrittstabelle[1, i] = j
        break
      }
      if (runif(1, 0, 1) <= Tafeln$Px_III[j]) {
        Eintrittstabelle[2, i] = j
      }
      if (runif(1, 0, 1) <= Tafeln$Px_II[j] &&
          j < Eintrittstabelle[2, i]) {
        Eintrittstabelle[3, i] = j
      }
      if (runif(1, 0, 1) <= Tafeln$Px_I[j] &&
          j < Eintrittstabelle[2, i] && j < Eintrittstabelle[3, i]) {
        Eintrittstabelle[4, i] = j
      }
      if (runif(1, 0, 1) <= Tafeln$wx_m[j] &&
          j < Eintrittstabelle[2, i] &&
          j < Eintrittstabelle[3, i] && j < Eintrittstabelle[4, i]) {
        Eintrittstabelle[5, i] = j
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
    actAge = Bestand$x[i] + Bestand$t[i]
    #cat('actage = ')
    #cat(actAge)
    #cat('\n')
    
    # buffer für storno
    storno = FALSE
    
    # berechne w = Alter bei dem Ein- und Auszahlungen enden
    endAge = Eintrittstabelle[1, i]
    if (Eintrittstabelle[5, i] < Eintrittstabelle[1, i]) {
      endAge = Eintrittstabelle[5, i]
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
    if (w > 0) {
      q_vec <- rep(0,w)
      p_vec <- rep(1,w)
      # cat('q_vec = ')
      # cat(q_vec)
      # cat('\n')
      # cat('p_vec = ')
      # cat(p_vec)
      # cat('\n')
    }
    else{
      q_vec = 0
      p_vec = 0
    }
    
    # years to pstI
    ytPST1 = Eintrittstabelle[4, i] - actAge
    if (ytPST1 < w && storno == FALSE) {
      for (k in 1:(w - ytPST1)) {
        q_vec[ytPST1 + k] = 0.4
        p_vec[ytPST1 + k] = 0.0
      }
    }
    #cat('ytPST1 = ')
    #cat(ytPST1)
    #cat('\n')
    
    
    # years to pstII
    ytPST2 = Eintrittstabelle[3, i] - actAge
    if (ytPST2 < w && storno == FALSE) {
      for (k in 1:(w - ytPST2)) {
        q_vec[ytPST2 + k] = 0.7
        p_vec[ytPST2 + k] = 0.0
      }
    }
    #cat('ytPST2 = ')
    #cat(ytPST2)
    #cat('\n')
    
    
    # years to pst3
    ytPST3 = Eintrittstabelle[2, i] - actAge
    if (ytPST3 < w && storno == FALSE) {
      for (k in 1:(w - ytPST3)) {
        q_vec[ytPST3 + k] = 1
        p_vec[ytPST3 + k] = 0.0
      }
    }
    #cat('ytPST3 = ')
    #cat(ytPST3)
    #cat('\n')
    
    #cat('q_vec before = ')
    #cat(q_vec)
    #cat('\n')
    
    # Ausgaben = sum(L*q_vec*abzinsungsfaktor)
    # q_vec * abzVek
    for (k in 1:length(q_vec)) {
      if (k <= ncol(DiskontZinsTabelle)) {
        q_vec[k] = q_vec[k] * DiskontZinsTabelle[ZinsSK, k]
        p_vec[k] = p_vec[k] * P_x_PST_einzeln_vec[i]*DiskontZinsTabelle[ZinsSK,k]
      }
      if (k > ncol(DiskontZinsTabelle)) {
        q_vec[k] = q_vec[k] * DiskontZinsTabelle[ZinsSK, ncol(DiskontZinsTabelle)]
        p_vec[k] = P_x_PST_einzeln_vec[i]*DiskontZinsTabelle[ZinsSK,ncol(DiskontZinsTabelle)]
      }
    }
    # cat('q_vec = ')
    # cat(q_vec)
    # cat('\n')
    # cat('p_vec = ')
    # cat(p_vec)
    # cat('\n')
    # 
    #cat(sum(Bestand$L[i]*q_vec))
    #cat('\n')
    
    
    EWR = EWR + 12 * Bestand$L[i] * sum(q_vec) - sum(p_vec)
  }
  EWR_vec[p] = EWR
})

# > EWR_vec
# [1] 1177936731 1152709344 1184291407 1149240466 1201506911 1156066798 1186641412 1166289355
# [9] 1195579997 1214046198
# > mean(EWR_vec)
# [1] 1178430862