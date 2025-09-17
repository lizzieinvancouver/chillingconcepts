## Started 8 May 2025 ##
## Updated 17 September 2025: with plotting code from walde.R ##

library(data.table)
walde <- fread("~/Documents/git/projects/treegarden/chilling/analyses/walde_etal/Experimental_Data.txt")

unique(walde[["Target Temperature"]])
unique(walde[["Measured Temperature"]])
unique(walde$Chilling)
unique(walde$Species)


library(tidyverse)

walde$forcing  <- walde[["Measured Temperature"]]
walde$bbdoy  <- walde[["DOY of Budburst"]]
walde$chill  <- walde[["Chilling"]]
walde$photo  <- walde[["Photoperiod"]]
walde$species  <- walde[["Species"]]

waldesm <- subset(walde, select=c("forcing", "bbdoy", "chill", "photo", "species"))

 dat <- 
  waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  filter(species == "Quercus robur")

# Stratify and look at each
# Assume intercepts (start dates) are different ...
# And the GDD required changes ....
summary(lm(bbdoy ~ I(1/forcing), data = dat, subset = chill == "high",
           weight = forcing^3)) # cube variance because it's a f(x) of the mean

summary(lm(bbdoy ~ I(1/forcing), data = dat, subset = chill == "low",
           weight = forcing^3))

# If you want to know which is more plausible, you could fit a fixed slope and a fixed intercept model and see which has larger likelihood.

# Below, saturate model and see what changes ...
# Here you can see that different start dates explain more than different GDD
logLik(lm(bbdoy ~ I(1/forcing) + chill, data = dat,
           weight = forcing^3))

logLik(lm(bbdoy ~ I(1/forcing):chill, data = dat,
          weight = forcing^3))

# You can force the GDD to be the same and try to get the intercept to fit it... 

# I'm getting the first is larger so that it's more likely chill changes the start date than GDD. 
# Technically, the second logLik is wrong because the variance depends on GDD so you need to do something like

library("nmle")
logLik(
gls(
  bbdoy ~ I(1/forcing):chill,
  data    = dat,
  weights = varComb(varFixed(~I(forcing^(-3))), 
                    varIdent(form = ~1|chill)),
  method = "ML"
  )
)

## Plotting!
# Fitting separate intercepts 
# Chilling changes start dates but not amount of GDD needed
plotchangestartdates  <- waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  filter(species == "Quercus robur") %>%
  ggplot() +
  aes(forcing, bbdoy, weight = forcing^3, color = chill, group = chill) +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(linewidth = 0.5, colour = "darkgray")) +
  labs(x = "Temperature (°C)",
       y =  "Mean Time until Budburst (days)",
       title = "Chilling treatment changes start date of forcing \n (not GDD required)") +
  #facet_grid(chill ~ ., scales = "free") +
  geom_smooth(method = "lm", formula = y ~ offset(I(850/x))) 

# Fitting separate intercepts 
# Chilling changes the GDD required but not the start date (common theory)
plotchangegdd <- waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  filter(species == "Quercus robur") %>%
  ggplot() +
  aes(forcing, bbdoy) +
  geom_point(aes(color = chill, group = chill)) +
  theme_bw() +
  theme(axis.line = element_line(linewidth = 0.5, colour = "darkgray")) +
  labs(x = "Temperature (°C)",
       y =  "Mean Time until Budburst (days)", 
       title = "Chilling treatment changes GDD required \n (not start date of forcing)") +
  #facet_grid(chill ~ ., scales = "free") +
  geom_function(fun = function(x) 1176.355/x -2.456) +
  geom_function(fun = function(x) 518.548  /x -2.456)

library(cowplot)
pdf("~/Documents/git/projects/treegarden/chilling/analyses/walde_etal/figures/quickcompare.pdf", height=5, width=9)
plot_grid(plotchangegdd, plotchangestartdates)
dev.off()