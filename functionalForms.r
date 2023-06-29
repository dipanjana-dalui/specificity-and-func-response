library(ggplot2)
library(tidyverse)
f <- function(N, s, z){
  y <- N*(s*N)^z
}
library(scales)

zvals <- c(-0.5, -0.05, 0, 0.05,  0.5)
zcols <- seq_gradient_pal(low = "#132B43", high = "#56B1F7")(
  seq(0, 1, length.out = length(zvals)))


ggplot(data = data.frame(N = 0), aes(x = N)) +
  map(rev(zvals), function(z)
    geom_function(fun = f, args = list(s = 0.5, z = z), 
                  colour = zcols[match(z, zvals)], linewidth = 1.2)) +

  map(zvals, function(z) geom_text(aes(x = 9, y = f(9.5, 0.5, z) + 0.5, 
                                       label = paste("z = ", z)), 
                                   colour = zcols[match(z, zvals)])) +
  lims(x = c(0, 10)) + labs(x = "N", y = "Predation rate (per enemy)") +
  theme_classic(base_size = 18) 

