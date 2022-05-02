install.packages("psych")
require(psych)
pairs.panels(iris)

require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

require(here)
dat_hab = data.frame(read.csv(
  here("data", "hab.sta.csv")
)
)

merge(dat_bird, dat_hab)

dat_all = merge(dat_bird, dat_hab)
plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)

my_vec = rep(1:3, 5)
my_vec == 3

my_vec > 1

as.numeric(my_vec > 1)

cewa_present_absent = 
  
directions <-c(dat_bird,dat_hab)
dat_all$CEWA >0
as.numeric(dat_all$CEWA >0)
cewa_present_absent = as.numeric(dat_all$CEWA >0)

plot(x = dat_all$elev, y = cewa_present_absent)

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}


get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent, pch=.5)

require(here)
dat_hab = read.csv(
  here("data", "hab.sta.csv")
)

install.packages("psych")
require(here)
dat_habsub = read.csv(
  here("data", "hab.sub.csv")
)
par(mfrow = c(2,1))
plot(dat_hab$basal, dat_hab$slope)
plot(dat_bird$slope, dat_bird$basal)

pairs.panels(dat_hab[c("ba.tot", "slope", "elev", "aspect")])

head(dat_all)

#Question 3
dat_all<-data.frame(dat_hab, dat_bird)

plot(x = dat_all$ba.tot, y = dat_bird$BAEA, pch=.5)
plot(dat_all$HOWR> 0 ~ dat_all$ba.tot,
     ylab= "Presence",
     xlab= "Total Basal Area",
     main= "House Wren presence/absence"
     )
curve(logistic_midpoint_slope(x, midpoint = 75, slope = -0.5), add = TRUE)

get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#Question 5

plot(dat_all$CEWA> 0 ~ dat_all$ba.tot,
     ylab= "Presence",
     xlab= "Total Basal Area",
     main= "Cedar Waxwing presence/absence"
)
curve(logistic_midpoint_slope(x, midpoint = 65, slope = -0.5), add = TRUE)

#Question 7

dat_all$GRJA
sum(as.numeric(dat_all$GRJA))
as.numeric(dat_all$GRJA)


sum(as.numeric(dat_all$GRJA>0))

names(dat_habsub)
plot(dat_hab)

View(dat_habsub)


dev.off()























