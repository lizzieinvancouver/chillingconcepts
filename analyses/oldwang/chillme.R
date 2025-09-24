
rm(list=ls());gc()
library(terra)
library(lubridate)
library(ggplot2)
library(patchwork)

wd <- "~/projects/chillingconcepts/analyses/oldwang"
source(file.path(wd, "toolbox.R"))

era5_tmin <- rast(file.path("/home/victor/projects/bcook_wine", "era5land", "era5_tmin_daily.nc"))
era5_tmin <- subset(era5_tmin, which(time(era5_tmin, format = "years") %in%c(1999:2000)))

site <- vect(data.frame(x = 10, y = 50), geom = c("x", "y"), crs = "EPSG:4326")
plot(subset(era5_tmin,1))
points(site)
temp <- extract(era5_tmin, site, ID = FALSE)
temp_df <- data.frame(temp = t(temp)-273.15, date=time(era5_tmin))

ggplot(data = temp_df ) +
  geom_line(aes(x = date, y = temp)) +
  theme_minimal()


obs <- as.Date("2000-01-30")

## Optimizing 4 parameters: start date, critical sum and temperature range
# Case 1
optm <- optim(par=c(170,70,0,10), fn=chillopt, x=temp_df, obs = as.Date("2000-01-30"))
optm$par
chill1 <- chillme(data = temp_df, d0 = as.Date("1999-01-01") %m+% days(as.integer(optm$par[1])), 
                 C = optm$par[2], tmin = optm$par[3], topt = (optm$par[3]+optm$par[4])/2, tmax = optm$par[4])
chillsim1 <- data.frame(chill1$dchill, case = 1)
wangsim1 <- data.frame(temp = -30:30, value = sapply(-30:30, wang, tmin = optm$par[3], topt = (optm$par[3]+optm$par[4])/2, tmax = optm$par[4]),
                       case = 1)



# Case 2
optm <- optim(par=c(220,150,-20,10), fn=chillopt, x=temp_df, obs = as.Date("2000-01-30"))
optm$par
chill2 <- chillme(data = temp_df, d0 = as.Date("1999-01-01") %m+% days(as.integer(optm$par[1])), 
                 C = optm$par[2], tmin = optm$par[3], topt = (optm$par[3]+optm$par[4])/2, tmax = optm$par[4])
chillsim2 <- data.frame(chill2$dchill, case = 2)
wangsim2 <- data.frame(temp = -30:30, value = sapply(-30:30, wang, tmin = optm$par[3], topt = (optm$par[3]+optm$par[4])/2, tmax = optm$par[4]),
                       case = 2)


ggplot(data = rbind(wangsim1, wangsim2)) +
  geom_line(aes(x = temp, y = value, col = case, group = case)) +
  theme_minimal()
ggplot(data = rbind(chillsim1, chillsim2)) +
  geom_line(aes(x = date, y = sum, col = case, group = case)) +
  theme_minimal()


## Optimizing 3 parameters: critical sum and temperature range
d0 <- as.Date("1999-09-01") # fixed
# Case 1
optm1 <- optim(par=c(70,0,10), fn=chillopt_wostart, x=temp_df, obs = as.Date("2000-01-30"), d0 = d0)
optm1$par
chill1 <- chillme(data = temp_df, d0 = d0, 
                  C = optm1$par[1], tmin = optm1$par[2], topt = (optm1$par[2]+optm1$par[3])/2, tmax = optm1$par[3])
chillsim1 <- data.frame(chill1$dchill, case = "1")
wangsim1 <- data.frame(temp = seq(-20,30,0.5), value = sapply(seq(-20,30,0.5), wang, tmin = optm1$par[2], topt = (optm1$par[2]+optm1$par[3])/2, tmax = optm1$par[3]),
                       case = "1")

# Case 2
optm2 <- optim(par=c(130,-20,10), fn=chillopt_wostart, x=temp_df, obs = as.Date("2000-01-30"), d0 = d0)
optm2$par
chill2 <- chillme(data = temp_df, d0 = d0, 
                  C = optm2$par[1], tmin = optm2$par[2], topt = (optm2$par[2]+optm2$par[3])/2, tmax = optm2$par[3])
chillsim2 <- data.frame(chill2$dchill, case = "2")
wangsim2 <- data.frame(temp = seq(-20,30,0.5), value = sapply(seq(-20,30,0.5), wang, tmin = optm2$par[2], topt = (optm2$par[2]+optm2$par[3])/2, tmax = optm2$par[3]),
                       case = "2")

dacc <- ggplot(data = rbind(wangsim1, wangsim2)) +
  geom_line(
    aes(x = temp, y = value, col = case, group = case), linewidth = 1.5) +
  geom_line(
    aes(x = temp, y = value, group = case), linewidth = .5, col = "white") +
  scale_color_manual(values = c("#789DBC", "#DDA853")) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.05, 1.05), expand = FALSE) +
  theme(panel.grid = element_blank(), legend.position = 'none',
        plot.margin = margin(r = 20)) +
  labs(x = "Temperature", y = "Daily chilling")

acc <- ggplot() +
  geom_segment(aes(x = obs, xend = obs, y = -5, yend = max(optm1$par[1], optm2$par[1])),
               linetype = "dashed", alpha = 0.5) +
  geom_segment(aes(x = d0, xend = obs, y = optm1$par[1], yend = optm1$par[1]),
               linetype = "dashed", col = "#789DBC") +
  geom_segment(aes(x = d0, xend = obs, y = optm2$par[1], yend = optm2$par[1]),
               linetype = "dashed", col = "#DDA853")  +
  geom_line(
    data = rbind(chillsim1, chillsim2) %>% dplyr::filter(date <= obs),
    aes(x = date, y = sum, col = case, group = case), linewidth = 1.5) +
  geom_line(
    data = rbind(chillsim1, chillsim2) %>% dplyr::filter(date <= obs),
    aes(x = date, y = sum, group = case), linewidth = .5, col = "white") +
  geom_line(
    data = rbind(chillsim1, chillsim2) %>% dplyr::filter(date > obs & date < obs %m+% days(15)),
    aes(x = date, y = sum, col = case, group = case), linetype = "dotted", linewidth = 0.8) +
  scale_color_manual(values = c("#789DBC", "#DDA853")) +
  theme_bw() +
  coord_cartesian(ylim = c(-2, 150), expand = FALSE) +
  scale_y_continuous(position = "right") +
  theme(panel.grid = element_blank(), legend.position = 'none',
        plot.margin = margin(l = 20)) +
  labs(x = "", y = "Chilling accumulation")

assemble <- dacc + acc

cowplot::ggsave2(filename = file.path(wd, "non_identifiability.pdf"),
                 plot = assemble, 
                 device = cairo_pdf, width =  180, height = 60, unit = "mm")





## Optimizing two parameters: start date and threshold
range_temp <- c(-5, 12)
optm1 <- optim(par=c(0,70), fn=chillopt_worange, x=temp_df, obs = as.Date("2000-01-30"), range = range_temp)
optm1$par
chill1 <- chillme(data = temp_df, d0 = d0 %m+% days(as.integer(optm1$par[1])), 
                  C = optm1$par[2], tmin = range_temp[1], topt = (range_temp[1]+range_temp[2])/2, tmax = range_temp[2])
chillsim1 <- data.frame(chill1$dchill, case = "1")
wangsim1 <- data.frame(temp = seq(-20,30,0.5), value = sapply(seq(-20,30,0.5), wang, tmin = range_temp[1], topt = (range_temp[1]+range_temp[2])/2, tmax = range_temp[2]),
                       case = "1")

optm2 <- optim(par=c(60,120), fn=chillopt_worange, x=temp_df, obs = as.Date("2000-01-30"), range = range_temp)
optm2$par
chill2 <- chillme(data = temp_df, d0 = d0 %m+% days(as.integer(optm2$par[1])), 
                  C = optm2$par[2], tmin = range_temp[1], topt = (range_temp[1]+range_temp[2])/2, tmax = range_temp[2])
chillsim2 <- data.frame(chill2$dchill, case = "2")
wangsim2 <- data.frame(temp = seq(-20,30,0.5), value = sapply(seq(-20,30,0.5), wang, tmin = range_temp[1], topt = (range_temp[1]+range_temp[2])/2, tmax = range_temp[2]),
                       case = "2")

dacc <- ggplot(data = rbind(wangsim1, wangsim2)) +
  geom_line(
    aes(x = temp, y = value, col = case, group = case, linewidth = case, linetype = case)) +
  geom_line(
    aes(x = temp, y = value, group = case), col = "white", linewidth = 0.3) +
  scale_color_manual(values = c("#789DBC", "#DDA853")) +
  scale_linewidth_manual(values = c(2.5, 1.5)) +
  scale_linetype_manual(values = c('solid', 'solid')) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.05, 1.05), expand = FALSE) +
  theme(panel.grid = element_blank(), legend.position = 'none',
        plot.margin = margin(r = 20)) +
  labs(x = "Temperature", y = "Daily chilling")

acc <- ggplot() +
  geom_segment(aes(x = obs, xend = obs, y = -5, yend = max(optm1$par[2], optm2$par[2])),
               linetype = "dashed", alpha = 0.5) +
  geom_segment(aes(x = d0, xend = obs, y = optm1$par[2], yend = optm1$par[2]),
               linetype = "dashed", col = "#789DBC") +
  geom_segment(aes(x = d0, xend = obs, y = optm2$par[2], yend = optm2$par[2]),
               linetype = "dashed", col = "#DDA853")  +
  geom_line(
    data = rbind(chillsim1, chillsim2) %>% dplyr::filter(date <= obs),
    aes(x = date, y = sum, col = case, group = case), linewidth = 1.5) +
  geom_line(
    data = rbind(chillsim1, chillsim2) %>% dplyr::filter(date <= obs),
    aes(x = date, y = sum, group = case), linewidth = .5, col = "white") +
  geom_line(
    data = rbind(chillsim1, chillsim2) %>% dplyr::filter(date > obs & date < obs %m+% days(15)),
    aes(x = date, y = sum, col = case, group = case), linetype = "dotted", linewidth = 0.8) +
  scale_color_manual(values = c("#789DBC", "#DDA853")) +
  theme_bw() +
  coord_cartesian(ylim = c(-2, 150), expand = FALSE) +
  scale_y_continuous(position = "right") +
  theme(panel.grid = element_blank(), legend.position = 'none',
        plot.margin = margin(l = 20)) +
  labs(x = "", y = "Chilling accumulation")

dacc + acc
