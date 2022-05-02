require(palmerpenguins)

hist(penguins$flipper_length_mm)

image_file = "ugly_histogram.png"

png(
  here("images", image_file),
  width = 1200, height = 1000 
)

save_png_1 = function(image_file)
{
    require(here)
    png(
      here("images", image_file),
      width = 1200, height = 1000
    )
}

hist(penguins$body_mass_g)

hotpink_hist_fun = function(dat_vecmy_title, x_label)


dat_vec = penguins$body_mass_g
my_title = "Erika's Histogram"
x_label = "Erika's Data"

dev.off()
hotpink_hist_fun = function(dat_vec,my_title, x_label)


{
  hist(
    dat_vec,
    col = "hotpink",
    main = my_title,
    xlab = x_label)
}

hotpink_hist_fun(
  dat_vec = sample(x = 1:100,  size = 1000,replace = TRUE),
  my_title = "Erika's Random Numbers",
  x_label = "x-values"
)




