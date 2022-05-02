require(here)
dat_birds <- read.csv(here("data" , "bird.sta.csv"))
dat_birds
dat_habitat <- read.csv(here("data" , "hab.sta.csv"))
dat_habitat
pairs(iris)
pairs(iris[, c("Petal.Width", "Sepal.Width", "Sepal.Length")])
hist(dat_birds$EVGR,
     xlab = "Number of birds counted",
     main = "Histogram of Evening Grosbeak Abundance",
     breaks = 0:10 - 0.7,
     col = 
       adjustcolor ("hotpink", alpha.f = 0.1),
      border = "darkgreen",
     ylim = c(0, 1000),
     xlim = c(0,7))
pairs.panels(dat_habitat[c(6, 7, 8)])


install.packages("psych")
require("psych")

