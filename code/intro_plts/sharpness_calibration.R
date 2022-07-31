library(ggplot2)
library(data.table)
library(dplyr)
library(gridExtra)
library(here)


###Sharpness####
xs <- seq(-3, 3, length = 10000)
sharp <- dnorm(xs, mean = 0, sd = 0.3)
nsharp <- dnorm(xs, mean = 0, sd = 0.8)

df_sharpness <- data.frame(xs = xs, 
                    sharp = sharp,
                    nsharp = nsharp) |>
  setDT() |>
  melt(measure.vars = c("sharp", "nsharp")) |>
  rename(sharpness = variable, 
         density = value) |>
  mutate(sharpness = factor(sharpness,
                            levels = c("sharp", "nsharp"),
                            labels = c("more sharp", "less sharp")))


###Calibration####
#samples for histogram
sampl <- rnorm(0, 0.3, n = 100000)
df_samples = data.frame(samples = sampl)

#distributions
xs <- seq(-3, 3, length = 10000)
cal <- dnorm(xs, mean = 0, sd = 0.3)
ncalwide <- dnorm(xs, mean = 0, sd = 0.8)
ncalshift <-  dnorm(xs, mean = -0.5, sd = 0.3)

df_calibration <- data.frame(xs = xs, 
                    cal = cal,
                    ncalwide = ncalwide,
                    ncalshift = ncalshift) |>
  setDT() |>
  melt(measure.vars = c("cal", "ncalwide", "ncalshift")) |>
  rename(calibration = variable, 
         density = value) |>
  mutate(sharpness = factor(calibration,
                            levels = c("cal", "ncalwide", "ncalshift"),
                            labels = c("well calibrated", "not well calibrated", "not well calibrated")))
  


#######Make plots####
plot_sharpness <- ggplot(df_sharpness, aes(x = xs, y = density, color = sharpness)) +
  geom_line() +
  scale_colour_brewer(palette = "Set1") +
  xlab("x") +
  ylab("Density") +
  ggtitle("Sharpness") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))


plot_calibration <- ggplot() +
  geom_histogram(data = df_samples, aes(x = samples, y = ..density..), bins = 50, 
                 color = "gray", fill = "lightgray", alpha = 0.8) +
  geom_line(data = df_calibration, aes(x = xs, y = density, color = calibration)) +
  scale_colour_brewer(palette = "Set1", labels = c("well calibrated", "not well calibrated", "not well calibrated")) +
  xlab("x") +
  ggtitle("Calibration") +
  ylab("Density") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))


pdf(file = here("plots", "sharp_calib.pdf"), height = 5, width = 12)
grid.arrange(plot_sharpness, plot_calibration, ncol =2)
dev.off()
