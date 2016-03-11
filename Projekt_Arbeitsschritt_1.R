# Solvency2 Abschlussprojekt
# 3.2, Arbeitsschritt I

#########################################################################
#implementiere zuerst funktionen für t_p_x für sterbeWK q und StornoWK w

func_tPx_q_m <- function(t,x,lambda=1){
  buffer = 1
  if (t>0) {
    for(i in 1:t){
      buffer = buffer * (1 - (lambda*Tafeln$qx_m[x+i]))
    }
  }
  return(buffer)
}

func_tPx_q_w <- function(t,x,lambda=1){
  buffer = 1
  if (t>0) {
    for(i in 1:t){
      buffer = buffer * (1 - (lambda*Tafeln$qx_w[x+i]))
    }
  }
  return(buffer)
}

func_tPx_w_m <- function(t,x,lambda=1){
  buffer = 1
  if (t>0) {
    for(i in 1:t){
      buffer = buffer * (1 - (lambda*Tafeln$wx_m[x+i]))
    }
  }
  return(buffer)
}

func_tPx_w_w <- function(t,x,lambda=1){
  buffer = 1
  if (t>0) {
    for(i in 1:t){
      buffer = buffer * (1 - (lambda*Tafeln$wx_w[x+i]))
    }
  }
  return(buffer)
}

##########################################################################
# berechne unisex-werte fuer q und w

# berechne Vektor x_p_0 fuer gegebene Formel
x_p_0_m = x_p_0_w = rep(1,AgeMax)
x_w_0_m = x_w_0_w = rep(1,AgeMax)
for (i in 0:(AgeMax-1)) {
  x_p_0_m[i+1] = func_tPx_q_m(i,0)
  x_p_0_w[i+1] = func_tPx_q_w(i,0)
}

# calc q_x_unisex and w_x_unisex
# vorerst mittelwert da formel nicht zu stimmen scheint
q_x_unisex = w_x_unisex = rep(1,AgeMax)
for (i in 1:AgeMax) {
  # folgende formal passt nicht
  # q_x_unisex[i] = (Tafeln$qx_m[i]*x_p_0_m[i]+Tafeln$qx_w[i]*x_p_0_w[i])/(x_p_0_m[i]*x_p_0_w[i])
  # deswegen
  q_x_unisex[i]=(Tafeln$qx_m[i]+Tafeln$qx_w[i])/2
  w_x_unisex[i]=(Tafeln$wx_m[i]+Tafeln$wx_w[i])/2
}

######################################################################
# erstelle funktion zum berechnen der tPx_gesamt(t,x) = Prod_(j=1)^(t) (1-q_unisex_(x+j))

func_tPx_gesamt <- function(t,x,lambda_1=1,lambda_2=1){
  buffer = 1
  if (t>0) {
    for(i in 1:t){
      buffer = buffer * (1 - (lambda_1*q_x_unisex[x+i]) - (lambda_2*w_x_unisex[x+i]))
    }
  }
  return(buffer)
}

#################################################################
# Funktion zum erstellen der tPx_gesamt_Matrix mit variablem lambda
create_tPx_gesamt_Matrix <- function(lambda_1=1,lambda_2=1){
  bufferMatrix <- matrix(nrow = AgeMax,ncol=AgeMax,byrow=TRUE)
  for (i in 1:AgeMax) {
    for (j in 1:AgeMax) {
      if(i<=(length(Tafeln$x)-j)){
        bufferMatrix[i,j] = func_tPx_gesamt(i-1,j-1)
      }
    }
  }
  return(bufferMatrix)
}

tPx_gesamt_Matrix <- create_tPx_gesamt_Matrix()
