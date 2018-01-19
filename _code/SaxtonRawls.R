# ************************************
# Functions to calculate soil hydraulic parameters
# Ref. Saxton & Rawls, 2006
# ************************************

# Function
# ______________________________________
# Input: {{pSand / pClay, decimal % Sand / Clay in Volume; pOM, % OM in weight}}
# Output: {{a list of BD, LL15, DUL, SAT}}
# ______________________________________

SaxtonRawls <-function(pSand, pClay, pOM){
  
  pSand <- pSand/100
  pClay <- pClay/100
  pOM <- pOM/100
  
  # calc LL15 (theta_1500)
  theta_1500t = -0.024*pSand + 0.487*pClay + 0.006*pOM +
    0.005*pSand*pOM - 0.013*pClay*pOM + 0.068*pSand*pClay + 0.031
  LL15 = theta_1500t + (0.14*theta_1500t - 0.02)
  LL15 = round(pmax(0.01, pmin(0.99, LL15)),3)
  
  # calc DUL (theta_33)
  theta_33t = -0.251*pSand + 0.195*pClay + 0.11*pOM +
    0.006*pSand*pOM - 0.027*pClay*pOM + 0.452*pSand*pClay + 0.299
  #DUL = theta_33t + (1.283*theta_33t^2 - 0.374*theta_33t - 0.015)
  DUL = theta_33t + (1.283*theta_33t^2 - 0.374*theta_33t - 0.015)
  DUL = round(pmax(0.01, pmin(0.99, DUL)),3)
  
  # calc SAT-33 KPa moisture
  theta_sat33t = 0.278*pSand + 0.034*pClay + 0.022*pOM -
    0.018*pSand*pOM - 0.027*pClay*pOM - 0.584*pSand*pClay + 0.078
  theta_sat33 = theta_sat33t + (0.636*theta_sat33t - 0.107)
  
  # calc SAT
  SAT = DUL + theta_sat33 - 0.097*pSand + 0.043
  SAT = round(pmax(0.01, pmin(0.99, SAT)),3)
  
  # calc BD
  BD = (1 - SAT)*2.65
  BD = round(pmax(1.0, pmin(2.1, BD)),3)
  
  # calc ksat (saturated water conductivity)
  lambda = (log(DUL)-log(LL15)) / (log(1500)-log(33))
  ksat = 1930*((SAT-DUL)^(3-lambda))
  SWCON = round(0.15 + pmin(ksat,75)/100, 3)
  
  res <- list(BD, LL15*100, DUL*100, SAT*100, SWCON*100,ksat)
  names(res) <- c("BD", "LL15", "DUL", "SAT", "SWCON","KSAT")
  
  return(res)
}

#SaxtonRawls(pSand = c(2,2),pClay = c(30,30), pOM=c(4,3))

