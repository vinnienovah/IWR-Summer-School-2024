install.packages("grDevices")
library( grDevices)
library(tidyverse)


RHS = function(t, y, parms){
  # state variables
  E <- y[1]
  J <- y[2]
  M <- y[3]
  # right-hand side of ODE
  list(c(
    # eggs
    - delta_E*E - mu_E*E, #'*Complete line: (egg production missing)*
    # juveniles
    delta_E*E - alpha*Jˆ2 - , #'*Complete line: (juvenile development and natural mortality missing)*
    # female adults
    - mu_M*M #'*Complete line: (emergence rate missing)*
  ))
}