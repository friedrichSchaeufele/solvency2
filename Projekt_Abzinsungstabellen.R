# Solvency2 Abschlussprojekt
# Erstelle Abzinsungstabellen
# fuer Jahre 1:101

#erstelle Rechnungszinsvektor
rZins = rep(0.02,length(Tafeln$x))
for (i in 6:length(Tafeln$x)) {
  rZins[i]=0.015
}
diskontRZins <- rep(1,length(rZins))
diskontRZins[1] = (1/(1+rZins[1]))
for (i in 2:length(diskontRZins)) {
  diskontRZins[i] = diskontRZins[i-1]*(1/(1+rZins[i]))
}

# erstelle Tabelle mit Zinswerten fuer AgeMax Jahre
Zinstabelle <- matrix(nrow = 6,ncol=AgeMax,byrow=TRUE)
rownames(Zinstabelle) <- c('RFNoVA','RFVA','3','4','5','6')
for (i in 1:(ncol(Zinsstrukturkurven)-1)) {
  for (j in 1:AgeMax) {
    if (j<=nrow(Zinsstrukturkurven)) {
      Zinstabelle[i,j] = Zinsstrukturkurven[j,i+1]
    }
    if (j > nrow(Zinsstrukturkurven)) {
      Zinstabelle[i,j] = Zinsstrukturkurven[nrow(Zinsstrukturkurven),i+1]
    }
  }
}

# Erstelle Diskontierungstabelle
DiskontZinsTabelle <- matrix(nrow = 6,ncol=AgeMax,byrow=TRUE)
rownames(DiskontZinsTabelle) <- c('DiskRFNoVA','DiskRFVA','Disk3','Disk4','Disk5','Disk6')
for (i in 1:nrow(DiskontZinsTabelle)) {
  for (j in 1:ncol(DiskontZinsTabelle)) {
    DiskontZinsTabelle[i,j] = (1/(1+Zinstabelle[i,j]))
    if (j>1) {
      for (k in 2:j) {
        DiskontZinsTabelle[i,j] = DiskontZinsTabelle[i,j]*(1/(1+Zinstabelle[i,j]))
      }
    }

  }
}

