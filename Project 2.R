## Project 2 ##
## 30.01.2023 ##
## Mimoza Dani ##

library(ggplot2)
library(palmerpenguins)
library(gauseR)
library(vegan)
library(mapsFinland)

#Problem 1
data(package = "palmerpenguins")
View(penguins)
penguin <- matrix(data=cbind(c(penguins$flipper_length_mm),c(penguins$body_mass_g)), nrow=344, ncol=2, dimnames=list(NULL, c("flipper_length", "mass")))
penguin <- as.data.frame(penguin)
mod = lm(mass ~ flipper_length, data = penguin)
summary(mod)
mod
deviance(mod)
plot(penguin, main = "Penguin species", xlab="Flipper length(mm)", ylab="Body mass(g)")
abline(mod)
newX <- as.data.frame(c(190, 215, 230))
colnames(newX) <- "flipper_length"
predict(mod, newX)


#Problem 2
data(gause_1934_book_f22)
dat <- gause_1934_book_f22[gause_1934_book_f22$Treatment == "Pa",
]
View(dat)
spec2 <- matrix(data=cbind(c(dat$Volume_species2),c(dat$Day)), nrow=24, ncol=2, dimnames=list(NULL, c("Day", "Volume_species2")))
spec2 <- as.data.frame(spec2)
plot(x = dat$Day, y = dat$Volume_Species2)
species.nls <- nls(Volume_Species2 ~ SSlogis(Day, Asym, xmid, scal), data=dat)
species.nls
#r=1.413, K=222.364, Nm=7.250
newy <- as.data.frame(seq(0, 25, by = 0.1))
colnames(newy) <- "Day"
pred <- predict(species.nls, newy)
plot(dat$Day, dat$Volume_Species2, main = "Paramecium aurelia", xlab = "Day",
     ylab = "Volume_Species2")
lines(newy$Day, pred, lwd = 3, col = "blue")

#Problem 3
#run all the codes together
data(gause_1934_book_f22)
dat <- gause_1934_book_f22[gause_1934_book_f22$Treatment == "Pa",
]
View(dat)
time <- dat$Day
species <- data.frame(dat$Volume_Species2)
colnames(species) <- c("P_aurelia")
gause_out <- gauseR::gause_wrapper(time=time, species=species)
gause_out$parameter_intervals
#P_aurelia -> 5.290266503
#r1 -> 0.707457874
#a11 -> -0.003181534

#Problem 4
data("seutukunnat2019")
View(data)
library(sf)
ggplot(seutukunnat2019) + geom_sf() + ggtitle("Finland: Maps in R!")

data(sipoo, sipoo.map)
S <- specnumber(sipoo)
plot(S ~ area, sipoo.map, xlab = "Island Area (ha)", ylab = "Number of Bird Species",
     ylim = c(1, max(S)))
marr <- nls(S ~ SSarrhenius(area, k, z), data=sipoo.map)
marr
newX <- as.data.frame(seq(0, 200, by = 0.1))
colnames(newX) <- "area"
pred <- predict(marr, newX)
plot(sipoo.map$S, sipoo.map$area, main = "Seutukunnat", xlab = "Island Area (ha)",
     ylab = "Number of Bird Species")
lines(newX$area, pred, lwd = 3, col = "blue")
#k=3.40619, z=0.43644
summary(marr)
#run 77 again, if you can't see the curve.


