#SSE Function

sse_mean= function (x)
  {
    n= length(x) - sum(as.numeric(is.na(x)))
    return(sd(x, na.rm = TRUE)/ sqrt(n))
}

sse_mean(penguins$bill_depth_mm)

require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)

length(penguins$bill_depth_mm)
sd(penguins$bill_depth_mm)
is.na(penguins$bill_depth_mm)
as.numeric(is.na(penguins$bill_depth_mm))
sum(as.numeric(is.na(penguins$bill_depth_mm)))

boxplot(flipper_length_mm ~ species, data = penguins)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

boxplot(formula=body_mass_g ~ sex:species,
        data = penguins, 
        lwd = 2, 
        main = “Penguin Conditional Boxplot”, 
        las = 3, 
        ylab = “Body Mass (g)”, 
        names = c(“Female\nAdelie”, “Male\nAdelie”, “Female\nChinstrap”, “Male\nChinstrap”, “Female\nGentoo”, “Male\nGentoo”
        )
)

boxplot(formula=body_mass_g ~ sex:species)

# for reproducibility
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

#difference in means
print(c(observed = diff_observed, simulated = diff_simulated))

#simulation function
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)


#my function
two_group_resample = function(x, n_1, n_2) 
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
}
 
two_group_resample(dat_pen$flipper_length_mm,68,152)

#Resampling Experiment

n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

#Retrieving Named Elements

t_test = t.test(flipper_shuffled ~ dat_pen$species)

str(t_test)
t_test$estimate

#Question 1

sse_mean = function(x)
{
  n= length(x) - sum(as.numeric(is.na(x)))
  return(sd(x, na.rm = TRUE)/ sqrt(n))
}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#2

two_group_resample = function(x, n_1, n_2) 
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
  return(difference_in_means)
}

#4

two_group_resample = function(x, n_1, n_2) 
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
}

two_group_resample(dat_pen$flipper_length_mm,68,152)

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}

hist(mean_differences,
     ylim = c(0,450),
     col = "#556B2F",
     main = "Resamples Differences")

#5

abs(mean_differences>5.8)
as.numeric(abs(mean_differences>5.8))
sum(as.numeric(abs(mean_differences>5.8)))

#6

sum(as.numeric(abs(mean_differences>5.8, add= TRUE)))



?b1
?
  
b1
help()


b1?dev.off()


a(Erika)
a = erika
b1 = 45.6
b2 = "45.6"
c1 <- 1,2,3



