rm(list = ls())
wd <- "/home/victor/projects/chillingconcepts/analyses"

library(ggplot2)
library(patchwork)
source(file.path(wd, "frosthardiness", 'functions', "phenofunctions.R"))
source(file.path(wd, "frosthardiness", 'functions', "hardinessfunctions.R"))

# get some climate
# library(terra)
# paris <- vect(data.frame(lat = 48.864716, lon =2.34901))
# yr <- 2015
# datadir <-"/home/victor/projects/bcook_wine"
# era5_tmax <-rast(file.path(datadir, "era5land", "era5_tmax_daily.nc"))
# era5_tmin <- rast(file.path(datadir, "era5land", "era5_tmin_daily.nc"))
# era5_tmax_s <- subset(era5_tmax, which(time(era5_tmax, format = "years") %in% c((yr-1):(yr))))
# era5_tmin_s <- subset(era5_tmin, which(time(era5_tmin, format = "years") %in% c((yr-1):(yr))))
# tmin <- extract(era5_tmin_s, paris, ID=FALSE)-273.15
# tmax <- extract(era5_tmax_s, paris, ID=FALSE)-273.15
# 
# clim <- data.frame(
#   date = time(era5_tmax_s),
#   tmax = t(tmax),
#   tmin = t(tmin)
# )
# clim$tmean = (clim$tmax + clim$tmin)/2
# clim$doy = lubridate::yday(clim$date)
# clim[lubridate::year(clim$date) == yr-1,"doy"] <- clim[lubridate::year(clim$date) == yr-1,"doy"]-365
# saveRDS(clim, file = file.path(wd, "data", "clim.rds"))

clim <- readRDS(file = file.path(wd, "frosthardiness", "data", "clim.rds"))

nbdays <- max(clim$doy)

params <- list(
  t0 = -62, Vb = 12, Ccrit = 73.7, # chilling (threshold inferior)
  d = -0.08, e = 13.0, Fcrit = 37.3, # forcing (sigmoid)
  FHminfe = -5.3, # min frost hardiness
  Te1 = 10, Te2 = -16, FHtfemax = -8,  # temperature component of frost hardines
  NL1 = 10,  NL2 = 16, FHpfemax = -12 # photoperiod component of frost hardiness
)

pheno <- data.frame(
  date = clim$date,
  doy = clim$doy,
  phase = NA,
  state = NA
)

source(file.path(wd, "frosthardiness", "phenology.R"))

hard <- data.frame(
  date = clim$date,
  doy = clim$doy,
  cr = NA,
  fh = NA
)

source(file.path(wd, "frosthardiness", "hardiness.R"))


# Plot things
breaks <- c("2014-11-01", "2015-01-01", "2015-03-01", "2015-05-01")

phenophases <- ggplot() +
  geom_vline(aes(xintercept = dormancy.break.date), linetype = "dashed", linewidth = 0.3, color = "grey30") +
  geom_line(data = na.omit(pheno[pheno$phase == 1,]), aes(x = date, y = state, color = as.character(phase))) +
  geom_line(data = na.omit(pheno[pheno$phase == 2,]), aes(x = date, y = state, color = as.character(phase))) +
  scale_color_manual(values = c("1" =  "#60B5FF", "2" = "#FF9149")) +
  scale_x_date(position = "top", breaks =  as.Date(breaks), labels = format(as.Date(breaks), '%d %b')) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey10", size = 10),
        panel.grid = element_blank()) +
  labs(y = "Phenophase\nprogression")

startdate <- min(na.omit(pheno)$date)
phenoline <- ggplot() +
  geom_segment(aes(x = startdate, y = 0, xend = dormancy.break.date, yend = 0), linewidth = 2, color = "#60B5FF") +
  geom_segment(aes(x = dormancy.break.date, y = 0, xend = leafout.date, yend = 0), linewidth = 2, color = "#FF9149") +
  theme_void() +
  geom_text(aes(y = -0.008, x = median(c(startdate,dormancy.break.date)), label = "Endodormancy"), color = "grey10", size = 3.5) +
  geom_text(aes(y = -0.008, x = median(c(dormancy.break.date,leafout.date)), label = "Ecodormancy"),  color = "grey10", size = 3.5) +
  ylim(-0.015,0.005)

hardiness <-ggplot() +
  geom_vline(aes(xintercept = dormancy.break.date), linetype = "dashed", linewidth = 0.3, color = "grey30") +
  geom_line(data = na.omit(hard), aes(x = date, y = fh), color = "#b597ff") +
  scale_x_date(position = "bottom", breaks =  as.Date(breaks), labels = format(as.Date(breaks), '%d %b')) +
  scale_y_continuous(position = 'right') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey10", size = 10),
        panel.grid = element_blank()) +
  labs(y = "Frost hardiness (°C)")


assemble <- phenophases + phenoline + hardiness + plot_layout(heights = c(1,0.40,1.5), nrow = 3)
ggsave(assemble, filename = file.path(wd, "figures", "processbased_pheno_hardiness.pdf"), height = 4)


