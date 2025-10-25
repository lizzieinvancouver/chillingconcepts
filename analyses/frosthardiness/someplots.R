
temp <- seq(-20,20, 0.1)
chilling <- sapply(temp, function(t) thresholdinf(t, params))

par(mar=c(4,4,1,0))
plot(chilling~temp, type = "l", col = "#60B5FF", lty = 1, lwd = 2, axes=F)
axis(side=1, at=seq(-20,20, 10))
axis(side=2, at=c(0,0.5,1))

temp <- seq(-10,30, 0.1)
forcing <- sapply(temp, function(t) sigmoid(t, params))

par(mar=c(4,4,1,0))
plot(forcing~temp, type = "l", col = "#FF9149", lty = 1, lwd = 2, axes=F,  ylim=c(0,1))
axis(side=1, at=seq(-10,30, 10))
axis(side=2, at=c(0,0.5,1))


par(mar=c(2,2,1,1))
plot(na.omit(hard)$cr~na.omit(hard)$date, type = "l", col = "#b597ff", lty = 1, lwd = 2, axes=F,  ylim=c(0,1))
lines(na.omit(pheno[pheno$phase ==2,])$state~na.omit(pheno[pheno$phase ==2,])$date,  col = "#FF9149", lty = 2)
axis(1, as.Date(c("2014-11-01", "2015-01-01", "2015-03-01", "2015-05-01")), format(as.Date(c("2014-11-01", "2015-01-01", "2015-03-01", "2015-05-01")), "%b %d"))
axis(side=2, at=c(0,0.5,1))


par(mar=c(4,4,1,0))
temp <- seq(-20,20, 0.1)
dFHtemp <- sapply(temp, function(t) dFHt(t, params$Te1, params$Te2, params$FHtfemax))
CR <- 0.5
plot(dFHtemp ~ temp, type = "l", col = "#b597ff", lty = 1, lwd = 2, axes=F, ylim=c(-8,0), xlab="Min. temp.", ylab = "Temp. comp.")
lines(CR*dFHtemp ~ temp,  col = "#b597ff", lty = 2, lwd = 2)
axis(side=1, at=seq(-20,20, 10))
axis(side=2, at=seq(-8,0,2))

