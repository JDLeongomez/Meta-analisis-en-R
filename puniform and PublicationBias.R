# https://cran.r-project.org/web/packages/puniform/index.html

library(puniform)

meta_plot(ri = dat$ri, ni = dat$ni)

meta_plot(ri = dat$ri, ni = dat$ni, nr_lines = "summary")

puniform(ri = dat$ri, ni = dat$ni, side = "right", method = "P",
         plot = TRUE)




# https://cran.r-project.org/web/packages/PublicationBias/index.html

library(PublicationBias)

pval_plot(dat$yi, dat$vi)

significance_funnel(yi = dat$yi, vi = dat$vi, favor_positive = TRUE, plot_pooled = TRUE)


# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pub-bias.html

library(dmetar)
library(meta)

res.meta <- metacor(cor = ri, n = ni, studlab = paste(dat$authors, dat$year), data = dat)
pcurve(res.meta)
