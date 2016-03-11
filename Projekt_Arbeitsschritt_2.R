# Solvency2 Abschlussprojekt
# 3.2, Arbeitsschritt II

# vektor der pflegestufen-anteile
q_PST = c(0.4,0.7,1)

# 1) tVx_gesamt = Sum_{PST} Sum_{Bestand} tVx^{PST}
# 2) tVx^{PST} = A_{x+t}^{PST} - P_{x}^{PST} * a_{x+t}
# 3) A_{x+t}^{PST} = Sum_{j=0}^{w-x} (L * 12 * q * k_{x+j}^{PST} * v_j)

# berechne Vektor mit allen Werten fuer A_x / L
# A_x_Vector = Sum_{j=0}^{Agemax-x} q^{PST} * 12 * v^j * k_{x+j}^{PST} * j_P_x 
A_x_Vector <- matrix(nrow = length(Tafeln$x),ncol=length(q_PST),byrow=TRUE)
for (i in 1:nrow(A_x_Vector)) {
  for (j in 1:ncol(A_x_Vector)) {
    A_x_Vector[i,j] = 0
    for (k in 0:(nrow(A_x_Vector)-i)) {
      A_x_Vector[i,j] = A_x_Vector[i,j] + q_PST[j]*12*diskontRZins[k+1]*Tafeln$Kx_Pst_I[i+k] *tPx_gesamt_Matrix[k+1,i] #*tPx_gesamt_Matrix[k+1,Bestand$x[i]]
    }
  }
}





# starte mit 
# 3)  




# A_x_PST = 0
# for (i in 1:length(Bestand$x)) {
#   buffer = 0
#   for (j in 0:(AgeMax-Bestand$x[i]-1)) {
#     buffer = buffer + (12*Bestand$L[i]*diskontRZins[j+1]*tPx_q_m_Matrix[j+1,Bestand$x[i]])*c(Tafeln$Kx_Pst_I[Bestand$x[i]+j],Tafeln$Kx_Pst_II[Bestand$x[i]+j],Tafeln$Kx_Pst_III[Bestand$x[i]+j])
#   }
#   A_x_PST = A_x_PST + sum(buffer*q_PST)
# }
# 
# # berechne a_x = Sum_(j=0)^(w-x) diskontRzins_j * jPx
# 
# a_x = 0
# for (i in 1:length(Bestand$x)) {
#   buffer = 0
#   for (j in 0:(AgeMax-Bestand$x[i]-1)) {
#     buffer = buffer + diskontRZins[j+1]*tPx_q_m_Matrix[j+1,Bestand$x[i]]
#   }
#   a_x = a_x + sum(buffer*q_PST)
# }
# 
# A_x_PST
# 
# a_x
# 
# P_x = A_x_PST/a_x
# berechne fuer alle Vertraege der jeweiligen Pflegestufen PSt=1,2,3
# mit Leistungsbeitrag L und q=1,0.7,0.4 : 
# tVx = A_{x+t}^(PSt) - P_{x}^(PSt) * a_{x+t}
#     = q * L ()
# func_tVx <- function(t,x,w,qPSt,L){
#   buffer = qPSt*L
#   sumK = 0
#   sumP = 0 
#   for (i in 0:(w-t-x)) {
#     sumK = sumK + Tafeln$Kx_Pst_III[x+t+i]*diskontKurve_RZ[i+1]*remainProb(i,(x+t))
#     sumP = sumP + diskontKurve_RZ[i+1]*remainProb(i,(x+t))
#   }
#   sump = sumP * Tafeln$Px_III[x]
#   buffer = buffer*(sumK-sumP)
#   return(buffer)
# }
# 
# 
# Bestand$t[2]
# Bestand$x[2]
# func_tVx(Bestand$t[2],Bestand$x[2],101,1,Bestand$L[2])