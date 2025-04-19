##############################################################
#Load Packages
##############################################################

# install.packages("dlnm")
install.packages(c("dlnm", "mvmeta","splines", "rgdal", "sf", "spdep" ))
install.packages("INLA", repos = c(getOption("repos"), INLA =
                                     "https://inla.r-inla-download.org/R/stable"), dep = TRUE) # only the first time

#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

# if (!requireNamespace("BiocManager", quietly = TRUE))
  # install.packages("BiocManager")
# BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)


# modelling
library(dlnm)
library(INLA)
library(mvmeta)
library(splines)

# data management and visualization
library(dplyr)
library(ggplot2)
library(tidyr)

# spatial data
library(rgdal)
library(sf)
library(spdep)

setwd("C:/Users/Administrator/Desktop/IWR 2024/documents-export-2024-9-23/Statistical methods/student_materials/student_materials")

dat <- read.csv("C:/Users/Administrator/Desktop/IWR 2024/documents-export-2024-9-23/Statistical methods/student_materials/student_materials/dengue_data_mod.csv", header = TRUE)
map <- st_read("RDHS_Kalutara_v2.shp")

head(dat)

tail(dat)

#####################################
# Part 2: Exploratory Data Analysis
######################################

seasonal_summary <- dat %>%
  group_by(season) %>%
  summarize(
    cases = mean(cases, na.rm = TRUE),
    BI = mean(BI, na.rm = TRUE)
  )
seasonal_summary

#############################################
par(mfrow = c(2, 2))
plot(seasonal_summary$BI, type = "l",
     xlab = "Time in months",
     ylab = "Average BI",
     main = "Breteau Index")
abline(v = 6, lty = 2, col = "gray")
boxplot(BI ~ season,
        data = dat,
        main = "Month specific variability 
across all regions",
        xlab = "Time in Months",
        ylab = expression(paste("Breteau Index")),
        col = "lightblue",
        border = "darkblue")
plot(seasonal_summary$cases, type = "l",
     xlab = "Time in months",
     ylab = "Average Cases",
     main = "Dengue Cases")
abline(v = 7, lty = 2, col = "gray")
boxplot(cases ~ season,
        data = dat,
        main = "Month specific variability 
across all regions",
        xlab = "Time in Months",
        ylab = expression(paste("Case number")),
        col = "pink",
        border = "brown")

##################################################

dat_agg <- aggregate(dat[, c("cases", "pop")], by = list(dat$regnames), FUN =
                       sum)
dat_agg$rate <- dat_agg$cases * 10000 / dat_agg$pop
map2 <- merge(map, dat_agg, by.x = "reg_names", by.y = "Group.1")
ggplot(map2) +
  geom_sf(aes(fill = cases)) +
  scale_fill_gradient(low = "beige", high = "brown") +
  ggtitle("number of cases")

#####################
ggplot(map2) +
  geom_sf(aes(fill = rate)) +
  scale_fill_gradient(low = "beige", high = "brown") +
  ggtitle("incidence rate per 10,000 person-years")



# inla.upgrade()
# remove.packages("INLA")
library(INLA)

#######################################################
# Part 3: Fixed and Mixed Effect Models in INLA
# Part 3a: Model Fitting and Likelihood Families
#######################################################
# formula <- outcome ~ variable1 + variable2 + ... + f(random_effect, model =
#                                                        "functionname")
# model <- inla(formula,
#               data = dat_agg, # define the data
#               family = "familyname", # set the likelihood family
#               # Default model family is "gaussian"
#               # Other families in epidemiology include "binomial", "poisson", "nbinomial", "zeroinflated..."
#               # Depending on the model you might need to add other arguments such as:
#                 # Ntrials = n for a binomial model
#                 # offset = n for count data models with a population
#               control.predictor = list(compute = TRUE), # computes fitted values (predictions)
#               control.compute = list(dic = TRUE) # computes DIC as the modelfit parameter
#               # Other model fit parameter options are waic and cpo
# )


formula1 <- cases ~ as.factor(season)
m.inla.1 <- inla(formula1,
                 offset = log(pop),
                 data = dat,
                 family = "poisson",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
summary(m.inla.1)

formula2 <- cases ~ as.factor(season)
m.inla.2 <- inla(formula2,
                 offset = log(pop),
                 data = dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
summary(m.inla.2)



m.inla.1$dic$dic
## [1] 10766.71

m.inla.2$dic$dic
## [1] 8199.496

####################################33
x1 <- as.data.frame(exp(m.inla.1$summary.fixed))
x1$season <- seq(1:12)
x1$model <- "poisson"
x2 <- as.data.frame(exp(m.inla.2$summary.fixed))
x2$season <- seq(1:12)
x2$model <- "nbinomial"
x <- rbind(x1, x2)
ggplot(x[x$season != 1,], aes(x = as.factor(season), y = `0.5quant`, ymin = `0.025quant`, ymax = `0.975quant`)) +
  geom_pointrange(aes(col = model)) +
  geom_errorbar(width = 0.5, aes(col = model)) +
  geom_line(aes(col = model)) +
  geom_hline(aes(yintercept = 1))

# Part 3c: Fitted vs. Predicted Values

dat$pred1 <- round(m.inla.1$summary.fitted.values$`0.5quant`, 0)
dat$pred2 <- round(m.inla.2$summary.fitted.values$`0.5quant`, 0)
ggplot(dat, aes(cases, pred1)) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle(paste0("observed vs fitted from Poisson model, R^2 = ", 
                 round(cor(dat$cases, dat$pred1), 3)))

##########################
ggplot(dat, aes(cases, pred2)) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle(paste0("observed vs fitted from negative binomial model, R^2 = ", 
                 round(cor(dat$cases, dat$pred2), 3)))

########################
ggplot(dat, aes(pred1, pred2)) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle(paste0("fitted values from both models, R^2 = ", 
                 round(cor(dat$pred1, dat$pred2), 3)))

# Part 3d: Exercises
formula3 <- cases ~ as.factor(season) +
  f(regnames, model = "iid")
m.inla.3 <- inla(formula3,
                 offset = log(pop),
                 data = dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
summary(m.inla.3)

# Part 4: Temporal Correlation Structures
# Part 4a: Non-Cyclic Temporal Effect
formula4 <- cases ~ 1 +
  f(time, model = "ar1") +
  f(regnames, model = "iid")
m.inla.4 <- inla(formula4,
                 offset = log(pop),
                 data = dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
# summary(m.inla.4)


x4 <- as.data.frame(exp(m.inla.4$summary.random$time))
x4$month <- seq(1:108)
x4$model <- "ar1"
ggplot(x4, aes(x = month, y = `0.5quant`, ymin = `0.025quant`, ymax =
                 `0.975quant`)) +
  geom_point() +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  geom_hline(aes(yintercept = 1))

###############
ggplot(dat, aes(time, cases)) +
  geom_col(fill = "grey")


# Part 4c: Cyclic Temporal Effect

formula5 <- cases ~ 1 +
  f(season, model = "rw1", cyclic = TRUE) +
  f(Year, model = "iid") +
  f(time, model = "iid") +
  f(regnames, model = "iid")
m.inla.5 <- inla(formula5,
                 offset = log(pop),
                 data = dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
# summary(m.inla.5)


x3 <- as.data.frame(exp(m.inla.3$summary.fixed))
x3$season <- seq(1:12)
x3$model <- "fixed_effect"
x3[1, c(1:7)] <- NA
x5 <- as.data.frame(exp(m.inla.5$summary.random$season))
x5$season <- seq(1:12)
x5$model <- "rw1_cyclic"
x <- rbind(x3, x5[, c(2:10)])

ggplot(x, aes(x = season, y = `0.5quant`, ymin = `0.025quant`, ymax =
                `0.975quant`)) +
  geom_pointrange(aes(col = model)) +
  geom_errorbar(width = 0.5, aes(col = model)) +
  geom_line(aes(col = model)) +
  geom_hline(aes(yintercept = 1))


# Part 5: Spatial Correlation Structures


map$ID <- seq(1:nrow(map))
dat2 <- merge(dat, map[, c("reg_names","ID")], by.x = "regnames", by.y =
                "reg_names")
map.mat <- poly2nb(map, queen = TRUE)
plot(map$geometry, col = "gray", border = "blue")
xy <- st_coordinates(st_centroid(map))
plot(map.mat, xy, col = "red", lwd = 2, add = TRUE)

#################
map.mat[[2]] # polygon 2 is a neighbor of polygons 1, 3, 5, and 6 in the map.mat list
## [1] 1 3 5 6
nb2INLA("map.adj", map.mat)
map.x <- inla.read.graph(filename = "map.adj")

# Part 5b: Model Fitting
###############################################
formula6 <- cases ~ as.factor(season) +
  f(ID, model = "bym", graph = map.x)
m.inla.6 <- inla(formula6,
                 offset = log(pop),
                 data = dat2,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
# summary(m.inla.6)


x6 <- as.data.frame(exp(m.inla.6$summary.random$ID[1:10, ]))

map$median6 <- exp(m.inla.6$summary.random$ID[1:10,]$`0.5quant`)
ggplot(map) +
  geom_sf(aes(fill = median6)) +
  scale_fill_gradient2(low = "forestgreen", mid = "beige", high = "brown", 
                       midpoint = 1)

##################################

map_m <- st_transform(map, crs = 5234)
map.mat2 <- poly2nb(map_m, snap = 5000, queen = TRUE)
plot(map_m$geometry, col = "gray", border = "blue")
xy <- st_coordinates(st_centroid(map_m))
plot(map.mat2, xy, col = "red", lwd = 2, add = TRUE)
########################
map.mat2[[2]] # polygon 9 is added to the neighbors of polygon 2 in the new map.mat2 list compared to map.mat
## [1] 1 3 5 6 9
nb2INLA("map.adj", map.mat2)
map.x2 <- inla.read.graph(filename = "map.adj")


############################################3
# Part 6: Spatio-Temporal Interactions
#############################################
formulaI = y ~ x +
  f(spaceID, model = "bym", graph = graph.adj) +
  f(timeID, model = "rw1") +
  f(timeID2, model = "iid") +
  f(spacetimeID, model = "iid")

formulaII = y ~ x +
  f(spaceID, model = "bym", graph = graph.adj) +
  f(timeID, model = "rw1") +
  f(timeID2, model = "iid") +
  f(spaceID2, model = "iid", group = timeID3, control.group = list(model =
                                                                     "rw1"))
formulaIII = y ~ x +
  f(spaceID, model = "bym", graph = graph.adj) +
  f(timeID, model = "rw1") +
  f(timeID2, model = "iid") +
  f(timeID3, model = "iid", group = spaceID2, control.group = list(model =
                                                                     "besag", graph = graph.adj))
formulaIV = y ~ x +
  f(spaceID, model = "bym", graph = graph.adj) +
  f(timeID, model = "rw1") +
  f(timeID2, model = "iid") +
  f(spaceID2, model = "besag", graph = graph.adj,
    group = timeID3, control.group = list(model = "rw1"))

##################################################
dat2$IDspacetime <- seq(1, nrow(dat2), 1)
formula7 <- cases ~ 1 +
  f(season, model = "rw1", cyclic = TRUE) +
  f(Year, model = "rw1") +
  f(time, model = "iid") +
  f(ID, model = "bym", graph = map.x) +
  f(IDspacetime, model = "iid")
m.inla.7 <- inla(formula7,
                 offset = log(pop),
                 data = dat2,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
# summary(m.inla.7)


# Part 6d: Getting Predictions (Optional)
dat2$cases2 <- dat2$cases
dat2[dat2$Year == 2018, ]$cases2 <- NA
formula8 <- cases2 ~ 1 +
  f(season, model = "rw1", cyclic = TRUE) +
  f(Year, model = "rw1") +
  f(time, model = "iid") +
  f(ID, model = "bym", graph = map.x) +
  f(IDspacetime, model = "iid")
m.inla.8 <- inla(formula8,
                 offset = log(pop),
                 data = dat2,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE))
# summary(m.inla.8)
dat2$pred7 <- round(m.inla.7$summary.fitted.values$`0.5quant`, 0)
dat2$pred8 <- m.inla.8$summary.fitted.values$`0.5quant`
dat2[dat2$Year != 2018,]$pred8 <- round(dat2[dat2$Year != 2018,]$pred8, 0)
dat2[dat2$Year == 2018,]$pred8 <- round(exp(dat2[dat2$Year == 2018,]$pred8), 
                                        0)
dat2_agg <- aggregate(dat2[, c("cases", "pred7", "pred8")], by =
                        list(dat2$Year), FUN = sum)
dat2_long <- gather(dat2_agg, type, cases, cases:pred8)
ggplot(dat2_long, aes(Group.1, cases)) +
  geom_col(aes(fill = type), position = "dodge", col = "black")
