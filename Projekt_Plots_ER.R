grosseSimPlot <- read.csv("results_vec_gr_Sim.csv", header = FALSE)
llPlot <- read.csv("results_vec_ll2.csv", header = FALSE)
stornoplot <- read.csv("results_vec_storno_50ProzentNach5Jahren_akt.csv", header = FALSE)

xbuffer = rep(0,(length(grosseSimPlot[1,])+length(llPlot[1,])+length(stornoplot[1,])))
ybuffer = rep(0,(length(grosseSimPlot[1,])+length(llPlot[1,])+length(stornoplot[1,])))
for (i in 1:length(grosseSimPlot[1,])) {
  xbuffer[i] = grosseSimPlot[1,i]
  ybuffer[i] = grosseSimPlot[2,i]
}
for (i in 1:length(llPlot[1,])) {
  xbuffer[length(grosseSimPlot[1,])+i] = llPlot[1,i]
  ybuffer[length(grosseSimPlot[1,])+i] = 7
}
for (i in 1:length(stornoplot[1,])) {
  xbuffer[length(grosseSimPlot[1,])+ length(llPlot[1,]) +i] = stornoplot[1,i]
  ybuffer[length(grosseSimPlot[1,])+ length(llPlot[1,]) +i] = 8
}

stripchart(xbuffer/1000000 ~ ybuffer,vertical = TRUE, pch=19 , method = 'jitter',group.names=c('RFNoVA','RFVA','NShockUP','NShockDown','VShockUp','VShockDown','Langlebigkeit','Storno'),main='Erbebnissübersicht der jeweiligen Szenarien',ylab='in Mio. Euro',xlab = 'Gruppe',cex.lab=1, cex.axis=0.4)
abline(h = mean(xbuffer[1:23])/1000000,col='blue')
abline(h = SCR_Zins[1]/1000000,col='orange')
text(x=3 , y = mean(xbuffer[1:23])/1000000, 'Mean Basisszenario',pos = 4,col = 'blue')
text(x=3 , y = SCR_Zins[1]/1000000, 'SCR Basisszenario',pos = 4,col = 'Orange')
dev.print(device=pdf,file="Gesamtplot.pdf")

mean_vec = rep(0,8)
sd_vec = rep(0,8)
for (i in 0:5) {
  mean_vec[i+1] = mean(xbuffer[(23*i+1):(23*i+23)])
  sd_vec[i+1] = sd(xbuffer[(23*i+1):(23*i+23)])
}
mean_vec[7]=mean(xbuffer[(23*6+1):(23*6+10)])
mean_vec[8]=mean(xbuffer[(23*6+11):(23*6+20)])
sd_vec[7]=sd(xbuffer[(23*6+1):(23*6+10)])
sd_vec[8]=sd(xbuffer[(23*6+11):(23*6+20)])

# student 99.5% Qauntil bei n = 23 ist 2,807
SCR_Zins <- rep(0,6)
for (i in 1:6) {
  SCR_Zins[i]=mean_vec[i]+sd_vec[i]*2.807
}

# student 99.5% Qauntil bei n = 10 ist 3,169
SCR_LL = mean_vec[7]+sd_vec[7]*3.169
SCR_Storno = mean_vec[8]+sd_vec[8]*3.169

#Berechnung des VaR
VaR_gesamt = sqrt(2*0.25*SCR_LL*SCR_Storno +  SCR_LL*SCR_LL + SCR_Storno*SCR_Storno + SCR_Zins[1]*SCR_Zins[1])