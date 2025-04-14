
# modified from Phenofit code
WINTER_SOLSTICE = -10; # 21 December
SUMMER_SOLSTICE = 173; # 22 June

leinonen_hardiness <- function(clim, pheno, params){
  
  NL = 0; # night length, hour fractions
  FD = 0; # frost damage, unitless, [0,1]
  FH = FHminfe; # frost hardiness, celsius degrees
  dFHti = 0; # temperature component of frost hardiness,
  dFHpi = 0; # photoperiod component of frost hardiness,
  CR = 0; # hardening competence
  day = date0;
  
  for(d in 1:nrow(clim)){
    
    leafDevelopmentPhase = pheno[pheno$day == d, 'phase']
    leafDevelopmentState = pheno[pheno$day == d, 'state']
    
    if (leafDevelopmentPhase == 1) {
      CR = 1;
    } else if (leafDevelopmentPhase == 2) {
      CR = max(0, 1 - leafDevelopmentState);
    }
    
    if (clim[d, 'tmin'] > Te1) {
      dFHti = 0;
    } else if (clim[pheno$day == d, 'tmin'] < params$Te2) {
      dFHti = params$FHtfemax;
    } else {
      dFHti = dFHt(clim[pheno$day == d, 'tmin'], params$Te1, params$Te2, params$FHtfemax);
    }
    
    if (CR == 0) {
      dFHpi = 0;
    } else if (day > WINTER_SOLSTICE && day < SUMMER_SOLSTICE) { # just discovered this: decreasing night length has no effect, in Leinonen 96
      dFHpi = params$FHpfemax;
    } else if (NL > params$NL2) {
      dFHpi = FHpfemax;
    } else if (NL < params$NL1) {
      dFHpi = 0;
    } else {
      dFHpi = dFHp(NL, params$NL1, params$NL2, params$FHpfemax);
    }
    
    FH = (4 / 5) * FH + (1 / 5) * (FHminfe + CR * (dFHti + dFHpi))
    
  }
                                  
}