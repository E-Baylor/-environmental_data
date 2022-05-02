#Simulated Data

dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)

apply(dat, MARGIN = 2, FUN = mean)

#Moth Data

install.packages("moths.csv")
require(here)
moths = read.csv(
  here("data", "moths.csv")
  )
head(moths)

#Numeric() creates an vector of length m w/ all values initialized to zer0

m = 10000

result = numeric(m)
head(result)

#Perform the bootstrap

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

#Calculate the quantiles

mean(result)

quantile(result,c(0.025,0.975))

#Bootstrap Data

install.packages("boot")
require(boot)
boot(data, statistic, R)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)

mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.025, 0.975))

#Setting up the bootstrap

moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat)
m=100
moth_result = matrix(
  nrow = m,
  ncol = n)

#Bootstrap simulation

n = nrow(moth_dat)
m = 100
moth_result = matrix(
  nrow = m,
  ncol = n)
for(i in 1:m)
{
  for(j in 1:n)
  {
    rows_j = sample(n, size = j, replace=TRUE)
    t1 = moth_dat[rows_j, ]
    t2 = apply(t1, 2, sum)
    moth_result[i, j] = sum(t2 > 0)
  }
}
head(moth_result)

# My Function

rm(list = ls())

moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  for(i in 1:n_iterations)
  {
    for(j in 1:n_input_rows)
    {
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#Testing my function

rm(list = ls())
moths = read.csv(here("data", "moths.csv"))

rarefaction_sampler = function(input_dat, n_iterations)
{
  #ADD MY FUNCTION
}
rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)

#Building the Rarefaction Curve

moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

#Plotting the Curve

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')
legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

#1
require(palmerpenguins)
dat_gen= subset(penguins, species== "Gentoo")

is.na(dat_gen$bill_length_mm)
as.numeric(is.na(dat_gen$bill_length_mm))
sum(as.numeric(is.na(dat_gen$bill_length_mm)))
length(dat_gen$bill_length_mm)
length(dat_gen$bill_length_mm) -1
length(dat_gen$bill_length_mm) - sum(as.numeric(is.na(dat_gen$bill_length_mm)))

#2

sd(dat_gen$bill_length_mm, na.rm = TRUE)

#3

mean(dat_gen$bill_length_mm, na.rm = TRUE)
quantile(dat_gen$bill_length_mm,c(0.025,0.975), na.rm=TRUE)

#4

dev.off()


qt(c(0.025,0.975)df=n_gentoo)
