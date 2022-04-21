#' Atmospheric conductance
#'
#' This function estimates atmospheric conductance
#' (how easily water diffuses into the air)
#’ 
#’
#' @param v_m wind speed (cm/s)
#' @param h vegetation height (cm)
#' @param k_d coefficient default is 0.7
#' @param k_0 coefficient default is 0.1
#' @authors Charles Hendrickson, Ryan Munnikhuis, Alex Vand
#' @examples atmospheric_conductance(v_m = 250, h = 1000)
#' @return atmospheric_conductance


atmospheric_conductance <- function(v_m, h, k_d = 0.7, k_0 = 0.1){
  z_m <- h + 200 #height at which windspeed is measured (cm)
  z_d <- k_d * h
  z_0 <- k_0 * h
  C_at <- v_m / (6.25 * log((z_m - z_d)/z_0)^2)
  
  return(C_at)
}


