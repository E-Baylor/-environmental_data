ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x"
  )

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, .3, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

set.seed(1234567)
n_pts = 50
x_min = 2
x_max = 10

x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim
    )

y_rexp  <- y_pred + rexp(n = n_pts, rate = 1.2)
plot(x_sim, y_rexp, main = "Normally Distributed Errors\n Exponentially Distributed Errors")

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

dev.off()

y_rexp  <- y_pred + rexp(n = n_pts, rate = 1.2)
plot(x_sim, y_rexp, main = "Normally Distributed Errors\n Exponentially Distributed Errors")

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_rexp)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_rexp - y_pred, main = "sim data 3", xlab = "observed y=values")

require(here)
dat_dis <- read.csv(here("data", "dispersal.csv"))

View(dat_dis)

plot(dat_dis$dist.class, dat_dis$disp.rate.ftb)

dev.off()

# 1)

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 1.9, 3.1), add = FALSE, from = 0, to = 60, ylab = "f(x)")

curve(
  exp_fun(x, 1.9, 3.03), add = TRUE, lty = "dotted")

curve(
  exp_fun(x, 1.2, -.2), add = TRUE, col = "red")

curve(
  exp_fun(x, 1.2, -.4), add = TRUE, lty = "dotted", col = "red")

#5

ricker_fun = function(x, a, b) 
{
  return(a *x* exp(-b * x))
}

curve(
  ricker_fun(x, 25,  0.1), add = FALSE, from=0, to = 50, ylab="f(x)")
curve(
  ricker_fun(x,20, 0.2), add=TRUE, lty = "dotted")
curve(
  ricker_fun(x, 10, .2), add=TRUE, lty = "dotted")
curve(
  ricker_fun(x, 75, 0.3), add=TRUE, col = "red")
curve(
  ricker_fun(x, 50, 0.3), add=TRUE, lty = "dotted", col = "red")
curve(
  ricker_fun(x, 40, 0.3), add=TRUE, lty = "dotted", col = "red")

#8

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

install.packages("salamander_dispersal.csv")
require(here)
dat_sal = read.csv(
  here("data", "salamander_dispersal.csv")
)

plot(
  x = dat_sal$dist.class, 
  y = dat_sal$disp.rate.ftb,
  xlab = "Distance Class",
  ylab = "Dispersal Rate",
  main = "Salamander Linear Model"
)
curve(line_point_slope(x, x1 = 600, y1 = .34, slope = -0.0004), add = TRUE)

#10

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

plot(
  x = dat_sal$dist.class, 
  y = dat_sal$disp.rate.ftb,
  xlab = "Distance Class",
  ylab = "Dispersal Rate",
  main = "Salamander Exponential Model"
)

curve(
  exp_fun(x, 13, .009 ), add=TRUE
)


#12
Ricker Function

plot(
  x = dat_sal$dist.class, 
  y = dat_sal$disp.rate.ftb,
  xlab = "Distance Class",
  ylab = "Dispersal Rate",
  main = "Salamander Ricker Model",
  xlim = c(0,1600)
)

curve(
  ricker_fun(x, .009, 1/200 ), add=TRUE
)

#14

ricker_fun(dat_sal$dist.class, .009, 1/200 )

dat_sal$predicted= ricker_fun(dat_sal$dist.class, .009, 1/200)

dat_sal$resids = c(dat_sal$predicted - dat_sal$disp.rate.ftb)


dat.resids=data.frame(Resids_Linear=(line_point_slope(dat_sal$dist.class, x1 = 600, y1 = .34, slope = -0.0004) - dat_sal$disp.rate.ftb),
  Resids_Exp= (exp_fun(dat_sal$dist.class, 13, .009 ) - dat_sal$disp.rate.ftb),
  Resids_Ricker=ricker_fun(dat_sal$dist.class, .009, 1/200 ) - dat_sal$disp.rate.ftb
  )



#15

par(mfrow = c(2,2))
hist(dat.resids$Resids_Linear,
     main = "Resids Linear",
     col = "#8EE5EE")
hist(dat.resids$Resids_Exp,
     main = "Resids Exp",
     col = "#7AC5CD")
hist(dat.resids$Resids_Ricker,
     main = "Resids Ricker",
     col= "#8EE5EE")








locator(1)

View(dat_sal)

dev.off()

