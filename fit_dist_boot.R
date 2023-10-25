

library(tidyverse)
library(boot)
library(fitdistrplus)



plotdist(data_to_fit_dist, histo = TRUE, demp = TRUE) 


descdist(data_to_fit_dist, boot = 1000)


fit_w  <- fitdist(data_to_fit_dist, "weibull")
fit_g  <- fitdist(data_to_fit_dist, "gamma")
fit_ln <- fitdist(data_to_fit_dist, "lnorm")

fit_exp <- fitdist(data_to_fit_dist, "exp")
fit_nbinom <- fitdist(data_to_fit_dist, "nbinom")
fit_geom <- fitdist(data_to_fit_dist, "geom")


options(
  scipen=999, 
  repr.plot.width  = 14,   # in inches (default = 7)
  repr.plot.height = 10    # in inches (default = 7)
)

par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma", "exp", "nbinom", "geom")
fit_list <- list(fit_w, fit_ln, fit_g, fit_exp, fit_nbinom, fit_geom)

denscomp(fit_list, legendtext = plot.legend) 
qqcomp(fit_list, legendtext = plot.legend)
cdfcomp(fit_list, legendtext = plot.legend)
ppcomp(fit_list, legendtext = plot.legend)



gofstat(fit_list,
         fitnames = plot.legend)


summary(fit_g)



# d <- rlnorm(10, meanlog = 1, sdlog = 0.5)

d <- data_to_fit_dist

ln_boot_mean <- function(data, ind){
    ld <- log(data[ind])
    exp(base::mean(ld)+var(ld)/2)
  }

d %>%
    boot(., statistic = ln_boot_mean, R = 100) %$% 
    print(.) %>%
    boot.ci(., conf = 0.95, type = "bca")     