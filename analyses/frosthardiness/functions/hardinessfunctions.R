
# from geosphere R package
nightlength <- function(doy, lat){
  
  doy <- (doy-1) %% 365 + 1

  # Forsythe et al., Ecological Modeling, 1995
  P <- asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.00860*(doy-186)))))
  a <-  (sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(P)) / (cos(lat * pi/180) * cos(P))
  a <- pmin(pmax(a, -1), 1)
  DL <- 24 - (24/pi) * acos(a)
  return(24-DL)
  
}

# frost hardiness functions, from PHENOFIT model 
dFHt <- function(t, temp1, temp2, max) {
  if(t > temp1) {
    return(0)
  }else if (t < temp2) {
    return(max)
  }else {
    return(max - (max / (temp1 - temp2)) * (t - temp2))
  }
}

dFHp <- function(nl, nl1, nl2, max) {
  if (nl < nl1) {
    return(0);
  } else if (nl > nl2) {
    return(max);
  } else {
    return(max / (nl2 - nl1) * (nl - nl1))
  }
}