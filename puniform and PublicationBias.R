# https://cran.r-project.org/web/packages/puniform/index.html

library(puniform)

meta_plot(ri = dat$ri, ni = dat$ni)

meta_plot(ri = dat$ri, ni = dat$ni, nr_lines = "summary")

puniform(ri = dat$ri, ni = dat$ni, side = "right", method = "LNP", plot = TRUE)



# https://cran.r-project.org/web/packages/PublicationBias/index.html

library(PublicationBias)

pval_plot(dat$yi, dat$vi, alpha.select = 0.05)

significance_funnel( yi = dat$yi, vi = dat$vi, favor.positive = TRUE, alpha.select = 0.50, plot.pooled = TRUE )