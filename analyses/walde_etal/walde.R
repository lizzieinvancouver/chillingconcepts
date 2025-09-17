## Started 13 Dec 2024 ##
## At GMU with Jonathan Auerbach ##
## Looking at the Walde data with different models ##

# Following Auerbach code from Professional/presentations/2024/Mo12_ESAstats/presentation.html #
# Full code here: https://github.com/eco4cast/Statistical-Methods-Seminar-Series/tree/main/auerbach-randomwalks


library(data.table)
walde <- fread("~/Documents/git/projects/treegarden/cherries/data/walde_etal/Experimental_Data.txt")

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

waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  ggplot() +
  aes(forcing, bbdoy, color=photo) +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(linewidth = 0.5, colour = "darkgray")) +
  labs(x = "Temperature (°C)",
       y =  "Mean Time until Budburst (days)") +
  facet_grid(species ~ chill, scales = "free") +
  geom_smooth(aes(weight = forcing^3), 
              method = "lm", formula = y ~ I(1/x), color = "red") 

waldesm %>%
  select(forcing, bbdoy, chill, species) %>%
  lm(bbdoy ~ I(1/forcing), data = ., weight = forcing^3) %>%
  summary()


waldesm %>%
  group_by(round(forcing/10, 2), chill, species) %>%
  summarize(mean_bbdoy = mean(bbdoy))

## Side project ... 
# Showing photoperiod for high chill
# waldehighchillphoto.pdf
waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  filter(chill == "high") %>%
  ggplot() +
  aes(forcing, bbdoy, color = as.factor(photo), group = as.factor(photo), weight = forcing^3) +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(linewidth = 0.5, colour = "darkgray")) +
  labs(x = "Temperature (°C)",
       y =  "Mean Time until Budburst (days)") +
  facet_wrap(species ~ ., scales = "free") +
  geom_smooth(method = "lm", formula = y ~ I(1/x)) 


## Back to chilling! 
# Fitting both intercept and forcing 
# Chilling changes start dates and amount of GDD needed
 waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  filter(species == "Quercus robur") %>%
  ggplot() +
  aes(forcing, bbdoy, color = chill, group = chill, weight = forcing^3) +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(linewidth = 0.5, colour = "darkgray")) +
  labs(x = "Temperature (°C)",
       y =  "Mean Time until Budburst (days)") +
  #facet_grid(chill ~ ., scales = "free") +
  geom_smooth(method = "lm", formula = y ~ I(1/x)) 

# Fitting separate intercepts 
# Chilling changes start dates but not amount of GDD needed
waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  filter(species == "Quercus robur") %>%
  ggplot() +
  aes(forcing, bbdoy, weight = forcing^3, color = chill, group = chill) +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(linewidth = 0.5, colour = "darkgray")) +
  labs(x = "Temperature (°C)",
       y =  "Mean Time until Budburst (days)") +
  #facet_grid(chill ~ ., scales = "free") +
  geom_smooth(method = "lm", formula = y ~ offset(I(850/x))) 

# Fitting separate intercepts 
# Chilling changes the GDD required but not the start date (common theory)
waldesm %>%
  select(forcing, bbdoy, chill, photo, species) %>%
  filter(species == "Quercus robur") %>%
  ggplot() +
  aes(forcing, bbdoy) +
  geom_point(aes(color = chill, group = chill)) +
  theme_bw() +
  theme(axis.line = element_line(linewidth = 0.5, colour = "darkgray")) +
  labs(x = "Temperature (°C)",
       y =  "Mean Time until Budburst (days)") +
  #facet_grid(chill ~ ., scales = "free") +
  geom_function(fun = function(x) 1176.355/x -2.456) +
  geom_function(fun = function(x) 518.548  /x -2.456)

 # We still have a problem: the regression stats support an interaction
 # Could it be survivor bias? Longer to budbreak, more likely to fail
 # If it is low-temp survivor bias ... then fit with the highest 3 temperatures and it should show no interaction 
waldesm %>%
  select(forcing, bbdoy, chill, species) %>%
  lm(bbdoy - 1 ~ I(1/forcing) * chill, data = ., weight = forcing^3, subset = species == "Quercus robur") %>%
  summary()

 # Next steps:
 # Remove photoperiod treatments 
 # Group by forcing temperature (treatment)
 # Try the model with top 3 temperatures

# Predictions: 
# If Charrier had two chill treatments and still started from endodormancy break date...
# Then the model should show no effect of chilling!
