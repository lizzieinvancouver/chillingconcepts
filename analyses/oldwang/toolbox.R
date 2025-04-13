
threshold_inferior <- function(x, Vb){
  return(as.numeric(x < Vb))
}

sigmoid_simp <- function(x, d, e){
  return(1/(1+exp(d*(x-e))))
}

wang <- function(x, tmin, topt, tmax){
  if((x <= tmax) & (tmax > (tmin + 1)) & (topt > (tmin + 0.5)) & (topt < (tmax - 0.5)) & (x > tmin)){
    alpha <-  log(2) / log((tmax - tmin) / (topt - tmin))
    num <- 2 * (x - tmin)^alpha * (topt - tmin)^alpha - (x - tmin)^(2 * alpha)
    den <- (topt - tmin)^(2*alpha)
    return(num/den)
  }else{
    return(0)
  }
}

chillme <- function(data, d0 = as.Date("1999-10-01"), dmax = as.Date("2021-12-31"), C, tmin, topt, tmax){
  dchill <- sapply(data[data$date >= d0 & data$date <= dmax, "temp"], wang, tmin, topt, tmax)
  sumchill <- cumsum(dchill)
  breakday <- which.max(sumchill >= C)
  
  return(list(dchill = data.frame(value = dchill, sum = sumchill, date = data[data$date >= d0 & data$date <= dmax, "date"]), breakday = as.Date(d0) %m+% days(breakday)))
}

chillopt <- function(x, param, obs){
  print(param)
  chill <- chillme(data = x, d0 = as.Date("1999-01-01") %m+% days(as.integer(param[1])), dmax = as.Date("2000-08-01"), C = param[2], tmin = param[3], topt = (param[3]+param[4])/2, tmax = param[4])
  return(abs(as.numeric(obs-chill$breakday)))
}

chillopt_wostart <- function(x, param, obs, d0){
  chill <- chillme(data = x, d0 = d0, dmax = as.Date("2021-12-31"), C = param[1], tmin = param[2], topt = (param[2]+param[3])/2, tmax = param[3])
  return(abs(as.numeric(obs-chill$breakday)))
}
