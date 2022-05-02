require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

#1
boxplot(dat_ade$body_mass_g ~ dat_ade$sex, 
        main = "Adelie Penguins Body Mass based on Sex", 
        xlab = "Sex", ylab = "Body Mass")

#2

dat_ad_f <- subset(dat_ade,sex == "female")
dat_ad_m <- subset(dat_ade,sex == "male")
t.test(dat_ad_f$body_mass_g, mu = 0)

#4

t.test(dat_ad_m$body_mass_g, alternative = c("greater"), mu = 4000)

#6

t.test(dat_ade$body_mass_g ~ dat_ade$sex)
t.test(dat_ad_f$body_mass_g, dat_ad_m$body_mass_g)

#8