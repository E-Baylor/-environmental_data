x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

na.rm = FALSE

set.seed(1)
n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)

par(mfrow = c(1, 1))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

set.seed(12)

dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)



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

n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat$x, guess_x, guess_y, guess_slope)

dat$y_predicted <- c(
  0.1012500,-0.1241179,0.2093763,-0.4074218,-0.1085965,0.3864613,0.3037460,0.2978222,-0.3424526,-0.3823739
)

dat$resids <- dat$y_predicted - dat$y_observed

sum(dat$resids)

plot(dat$resids ~ dat$y_observed)

hist(dat$resids)

vec <- c(norm_17,norm_30,norm_300,norm_3000)

set.seed(1)
pop_sd = 2.4
pop_mean = 10.4

norm_17 = rnorm(n = 17, mean = 10.4, sd = 2.4)
norm_30 = rnorm(n = 30, mean = 10.4, sd = 2.4)
norm_300 = rnorm(n = 300, mean = 10.4, sd = 2.4)
norm_3000 = rnorm(n = 3000, mean = 10.4, sd = 2.4)

par(mfrow = c(2,2))

hist(norm_17,main = "Sample Size of 17", xlab = "17 Samples", col = "hotpink")
hist(norm_30,main = "Sample Size of 30", xlab = "30 Samples", col = "darkgreen")
hist(norm_300,main = "Sample Size of 300", xlab = "300 Samples",col = "lightgreen")
hist(norm_3000,main = "Sample Size of 3000", xlab = "3000 Samples", col = "lightpink")



require(here)
png(
  filename = here("images", "lab_04_hist_01.png"),
  width = 1500, height = 1600, 
  res = 180)

dev.off()

x = seq(-6, 30, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, main = "Normal PDF: mean=10.4 & sd=2.4", type = "l", xlim = c(0, 25), ylim = c(0,0.2))
abline(h = 0)

pdf(
  file = here("images","norm_1.pdf")
  )

n_pts = 30
x_min = 3
x_max = 30
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(1)
par(mfrow = c(1, 1))

plot(dat, ylim=c(-1.5,2), main="Plot Random Data",
     col = "#CD8C95"
)

hist(dat$x,
     col = "#FFC0CB", border = "#FFE4E1", main = "Hist of Random Data"
     )

boxplot(dat,
        col = "#DEB887",border = "#8B7355", main = "Box Plot Random Data"
        )

dotchart(dat$x,
         col = "mediumpurple4", main = "Dotchart Random Data"
           )

require(here)
pdf(
  file = here("images","Random_Plot_Data.pdf")
)

require(here)
png(
  filename = here("images", "Random_Plot_Data.png"),
  width = 1500, height = 1600, 
  res = 180)

dev.off()



n_pts = 100 
x_min = 0 
x_max = 11.01 
x = runif(n = n_pts, min = x_min, max = x_max) 
dat = data.frame(x = x, y_observed = rnorm(n_pts)) 
plot(dat, ylim= c(-2.85, 3), main = "LPS of Random Data", pch = 10, cex = 1.5, col =  "#9932CC") 
line_point_slope = function(x, x1, y1, slope) 
{ 
  get_y_intercept =  
    function(x1, y1, slope)  
      return(-(x1 * slope) + y1) 
  
  linear =  
    function(x, yint, slope)  
      return(yint + x * slope) 
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope)) } 
x= dat$x 
x1= 5 
y1 = 0.4 
slope1= 0.2 

curve(
  line_point_slope(
    x,
    x1,
    y1,
    slope1
  ),
  add= TRUE
)

line_point_slope(
  dat$x,
  x1,
  y1,
  slope1
)


require(here)
pdf(
  file = here("images","Random_LPS_Data.pdf")
)

dev.off()

par(mfrow = c(1,2))

dat$y_predicted= c(line_point_slope(x,x1,y1,slope1))

dat$resids= c(dat$y_predicted-dat$y_observed)

hist(dat$resids, main = "Hist of LPS", col= "#FF6EB4")

plot(dat$resids - dat$y_predicted, main = "Plot of LPS", col="#B03060")

require(here)
pdf(
  file = here("images","LPS_RandomData_Graphs.pdf")
)

dev.off()




View(dat)
abine()