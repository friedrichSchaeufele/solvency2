# Solvency2 Abschlussprojekt
# 3.2, Arbeitsschritt II

# vektor der pflegestufen-anteile
q_PST = c(0.4,0.7,1)

# Kopfschaden_Matrix <- matrix(nrow = length(Tafeln$Kx_Pst_I),ncol = 3)
# for (i in 1:nrow(Kopfschaden_Matrix)) {
#   Kopfschaden_Matrix[i,1] =  Tafeln$Kx_Pst_I[i]
#   Kopfschaden_Matrix[i,2] =  Tafeln$Kx_Pst_II[i]
#   Kopfschaden_Matrix[i,3] =  Tafeln$Kx_Pst_III[i]
# }

# 1) tVx_gesamt = Sum_{PST} Sum_{Bestand} tVx^{PST}
# 2) tVx^{PST} = A_{x+t}^{PST} - P_{x}^{PST} * a_{x+t}
# 3) A_{x+t}^{PST} = Sum_{j=0}^{w-x} (L * 12 * q * k_{x+j}^{PST} * v_j)

# berechne Vektor mit allen Werten fuer A_x / L
# A_x_PST_Vector = Sum_{j=0}^{Agemax-x} q^{PST} * 12 * v^j * k_{x+j}^{PST} * j_P_x 
# A_x_Vector <- matrix(nrow = length(Bestand$x),ncol=length(q_PST),byrow=TRUE)
# for (i in 1:nrow(A_x_Vector)) {
#   for (j in 1:ncol(A_x_Vector)) {
#     A_x_Vector[i,j] = 0
#     for (k in 0:(AgeMax-Bestand$x[i])) {
#       A_x_Vector[i,j] = A_x_Vector[i,j] + diskontRZins[k+1]*Kopfschaden_Matrix[Bestand$x[i]+k,j]*tPx_gesamt_Matrix[k+1,Bestand$x[i]]
#     }
#     A_x_Vector[i,j] = A_x_Vector[i,j] * 12 * Bestand$L[i] * q_PST[j]
#   }
# }
# 
# a_pp_vector <- matrix(nrow = length(Bestand$x),ncol = 1)
# for (i in 1:nrow(a_pp_vector)) {
#   a_pp_vector[i,1] = 0
#   for (k in 0:(AgeMax-Bestand$x[i])) {
#     a_pp_vector[i,1] = a_pp_vector[i,1] + diskontRZins[k+1]*tPx_gesamt_Matrix[k+1,Bestand$x[i]]
#   }
# }
# 
# P_x_Vector <- matrix(nrow = length(Bestand$x),ncol=length(q_PST),byrow=TRUE)
# for (i in 1:nrow(P_x_Vector)) {
#   for (j in 1:ncol(P_x_Vector)) {
#     P_x_Vector[i,j] = A_x_Vector[i,j]/a_pp_vector[i,1]
#   }
# }

# vektoren für A_x_PST
A_x_I_vec <- rep(0,AgeMax)
for (i in 1:AgeMax) {
  for (k in 0:(AgeMax-i)) {
    A_x_I_vec[i] = A_x_I_vec[i] + 12*q_PST[1]*diskontRZins[k+1]*Tafeln$Kx_Pst_I[i+k]*tPx_gesamt_Matrix[k+1,i]
  }
}

A_x_II_vec <- rep(0,AgeMax)
for (i in 1:AgeMax) {
  for (k in 0:(AgeMax-i)) {
    A_x_II_vec[i] = A_x_II_vec[i] + 12*q_PST[2]*diskontRZins[k+1]*Tafeln$Kx_Pst_II[i+k]*tPx_gesamt_Matrix[k+1,i]
  }
}

A_x_III_vec <- rep(0,AgeMax)
for (i in 1:AgeMax) {
  for (k in 0:(AgeMax-i)) {
    A_x_III_vec[i] = A_x_III_vec[i] + 12*q_PST[3]*diskontRZins[k+1]*Tafeln$Kx_Pst_III[i+k]*tPx_gesamt_Matrix[k+1,i]
  }
}

# vektor fuer a..
a_pp_vec = rep(0,AgeMax)
for (i in 1:AgeMax) {
  for (k in 0:(AgeMax-i)) {
    a_pp_vec[i] = a_pp_vec[i] + diskontRZins[k+1]*tPx_gesamt_Matrix[k+1,i]
  }
}

# funktionen zum berechnen der prämien
func_P_x_I_value <- function(x){
  return(A_x_I_vec[x]/a_pp_vec[x])
}
# beispiel
# func_P_x_I_value(Bestand$x[1])*Bestand$L[1]

func_P_x_II_value <- function(x){
  return(A_x_II_vec[x]/a_pp_vec[x])
}

func_P_x_III_value <- function(x){
  return(A_x_III_vec[x]/a_pp_vec[x])
}


# jetzt Berechnung der Deckungsrueckstellung t_V_x = Leistung * (A_x+t^PST - P_x^PST * a_x+t)

t_V_x_I_ohne_Leistung <- function(t,x){
  return(A_x_I_vec[x+t]-func_P_x_I_value(x)*a_pp_vec[x+t])
}

t_V_x_II_ohne_Leistung <- function(t,x){
  return(A_x_II_vec[x+t]-func_P_x_II_value(x)*a_pp_vec[x+t])
}

t_V_x_III_ohne_Leistung <- function(t,x){
  return(A_x_III_vec[x+t]-func_P_x_III_value(x)*a_pp_vec[x+t])
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