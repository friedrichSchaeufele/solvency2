groﬂeSimPlot <- read.csv("results_vec_gr_Sim.csv", header = FALSE)

stripchart(EWR_vec ~ EWR_group_vec, pch=19 , method = 'jitter' )
