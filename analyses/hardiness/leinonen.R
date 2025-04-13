
# modified from Phenofit code

leinonen_hardiness <- function(clim, pheno, params){
  
  NL = 0; # night length, hour fractions
  FD = 0; # frost damage, unitless, [0,1]
  FH = FHminfe; # frost hardiness, celsius degrees
  dFHti = 0; # temperature component of frost hardiness,
  dFHpi = 0; # photoperiod component of frost hardiness,
  CR = 0; # hardening competence
  day = date0;
  
  for(d in 1:nrow(clim)){
    
    leafDevelopmentPhase = pheno[d, 'phase']
    leafDevelopmentState = pheno[d, 'state']
    
    if (leafDevelopmentPhase == 1) {
      CR = 1;
    } else if (leafDevelopmentPhase == 2) {
      CR = Math.max(0, 1 - leafDevelopmentState);
    }
    
    if (clim[d, 'tmin'] > Te1) {
      dFHti = 0;
    } else if (clim[d, 'tmin'] < Te2) {
      dFHti = FHtfemax;
    } else {
      dFHti = dFHt(clim[d, 'tmin'], Te1, Te2, FHtfemax);
    }
    
    if (CR == 0) {
      dFHpi = 0;
    } else if (day > WINTER_SOLSTICE && day < SUMMER_SOLSTICE) { # just discovered this: decreasing night length has no effect, in Leinonen 96
      dFHpi = FHpfemax;
    } else if (NL > NL2) {
      dFHpi = FHpfemax;
    } else if (NL < NL1) {
      dFHpi = 0;
    } else {
      dFHpi = dFHp(NL, NL1, NL2, FHpfemax);
    }
    
    FH = (4 / 5) * FH + (1 / 5) * (FHminfe + CR * (dFHti + dFHpi))
    
  }
                                  
}