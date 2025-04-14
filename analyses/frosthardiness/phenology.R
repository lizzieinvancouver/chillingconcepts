# phenology simulation, from PHENOFIT model

funits = 0
cunits = 0
dormancy.break.date <- leafout.date <- NA

# Start phenology simulations
day = params$t0 - 1

# Chilling loop
while(cunits < params$Ccrit & day < nbdays){
  day <- day + 1
  tmean <- clim[clim$doy == day, 'tmean'] 
  cunits <- cunits + thresholdinf(tmean, params)
  pheno[pheno$doy == day, 'phase'] <- 1
  pheno[pheno$doy == day, 'state'] <-  cunits / params$Ccrit
}
dormancy.break.date <- pheno[pheno$doy == day, 'date']

# Forcing loop
while(funits < params$Fcrit & day < nbdays){
  day <- day + 1
  tmean <- clim[clim$doy == day, 'tmean'] 
  funits <- funits + sigmoid(tmean, params)
  pheno[pheno$doy == day, 'phase'] <- 2
  pheno[pheno$doy == day, 'state'] <-  funits / params$Fcrit
}

# Y1 = FU - FU1;
# Y2 = Fcrit - FU1;
# if (Y2 < 0) {
#   pheno.setDate2(year, day - 1);
# } else if (Y2 != Y1 && Y1 != 0) {
#   pheno.setDate2(year, day - 1 + (Y2 / Y1));
# } else {
#   pheno.setDate2(year, day);
# }

leafout.date <- pheno[pheno$doy == day, 'date']




