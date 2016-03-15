grosseSimPlot <- read.csv("results_vec_gr_Sim.csv", header = FALSE)
llPlot <- read.csv("results_vec_langesLeben.csv", header = FALSE)
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

stripchart(xbuffer ~ ybuffer, pch=19 , method = 'jitter' )
