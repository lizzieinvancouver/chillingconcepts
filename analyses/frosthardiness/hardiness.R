# hardiness simulation, from PHENOFIT model

wintersolstice <- -10 # 21 December
summersolstice <- 172 # 21 June

FH = params$FHminfe
CR = 0

for(day in params$t0:lubridate::yday(leafout.date)){
  
  leafphase = pheno[pheno$doy == day, 'phase']
  leafstate = pheno[pheno$doy == day, 'state']
  
  if (leafphase <= 1) {
    CR = 1
  } else if (leafphase == 2) {
    CR = max(0, 1 - leafstate);
  }
  
  if (clim[pheno$doy == day, 'tmin'] > params$Te1) {
    dFHti = 0;
  } else if (clim[pheno$doy == day, 'tmin'] < params$Te2) {
    dFHti = params$FHtfemax;
  } else {
    dFHti = dFHt(clim[pheno$doy == day, 'tmin'], params$Te1, params$Te2, params$FHtfemax);
  }
  
  NL <- nightlength(day, lat = 48.864716)
  
  if (CR == 0) {
    dFHpi = 0;
  } else if (day > wintersolstice && day < summersolstice) { # just discovered this: decreasing night length has no effect, in Leinonen 96
    dFHpi = params$FHpfemax;
  } else if (NL > params$NL2) {
    dFHpi = FHpfemax;
  } else if (NL < params$NL1) {
    dFHpi = 0;
  } else {
    dFHpi = dFHp(NL, params$NL1, params$NL2, params$FHpfemax);
  }
  
  FH = (4 / 5) * FH + (1 / 5) * (params$FHminfe + CR * (dFHti + dFHpi))
  
  hard[hard$doy == day, 'cr'] <-  CR
  hard[hard$doy == day, 'fh'] <-  FH
  
}
