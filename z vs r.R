library(tidyverse)
library(ggpubr)

rVsz <- tibble(r = seq(-1, 1, by = 0.01)) %>%
  mutate(z = atanh(r))
  
ggplot(rVsz, aes(x = r, y = z)) +
  geom_smooth(se = FALSE, color = "#F68212") +
  geom_abline(slope=1, intercept = 0, lty = "dotted") +
  xlim(-1, 1) +
  scale_x_continuous(breaks = round(seq(min(rVsz$r), max(rVsz$r), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(-3, 3, by = 0.5),1)) +
  labs(x =  expression(paste("Coeficiente de correlación (", italic("r"), ") de Pearson")),
       y = expression(paste("Transformación a ", italic(" z"), " de Fisher"))) +
  theme_pubclean()
  


