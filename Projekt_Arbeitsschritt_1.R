# Solvency2 Abschlussprojekt
# 3.2, Arbeitsschritt I

# Funktion fuer t_P_x(q) maenlich, =  Produkt_{j=0}^{t-1} (1-q_m{x+j})
func_tPx_q_m <- function(t,x,lambda=1){
  buffer = 1
  if (t>1) {
    for(i in 0:(t-1)){
      buffer = buffer * (1 - (lambda*Tafeln$qx_m[x+i]))
    }
  }
  return(buffer)
}

# Funktion zum erstellen der tPx_q_m_Matrix mit variablem lambda
create_tPx_q_m_Matrix <- function(lambda=1){
  bufferMatrix <- matrix(nrow = length(Tafeln$x),ncol=length(Tafeln$x),byrow=TRUE)
  for (i in 1:length(Tafeln$x)) {
    for (j in 1:length(Tafeln$x)) {
      if(i<=(length(Tafeln$x)-j)){
        bufferMatrix[i,j] = func_tPx_q_m(i,j,lambda)
      }
    }
  }
  return(bufferMatrix)
}

# berechne gesamt tPx Matrix als kombination der einzelnen Matrizen
create_tPx_gesamt_Matrix <- function(lambda_q_m=1,lambda_q_w=1,lambda_w_m=1,lambda_w_w=1){
  # berechne matrizen für jeweilige q,w fuer maennlich/weiblich
  # lamdas koennen hier veraendert werden
  tPx_q_m_Matrix <- create_tPx_q_m_Matrix(lambda_q_m)
  #tPx_q_w_Matrix <- create_tPx_q_w_Matrix(lambda_q_w)
  #tPx_w_m_Matrix <- create_tPx_w_m_Matrix(lambda_w_m)
  #tPx_w_w_Matrix <- create_tPx_w_w_Matrix(lambda_w_w)
  # jetzt algorithmus zum berechnen 
  
  #vorerst
  return(tPx_q_m_Matrix)
}

#tPx_gesamt_Matrix <- create_tPx_gesamt_Matrix(1,1,1,1)
