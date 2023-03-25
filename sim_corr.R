library(faux)
library(tidyverse)
library(plyr)
library(metafor)
library(ggpubr)
library(scales)

set.seed(666)
data <- rnorm_multi(n = 1000, 
                   mu = c(0, 20),
                   sd = c(1, 5),
                   r = c(0.25), 
                   varnames = c("V1", "V2"),
                   empirical = TRUE)

ggplot(data, aes(x = V1, y = V2)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# Correlación por muestra
corfun <- function(x, y) {
  corr = (cor.test(x, y,
                   alternative = "two.sided", method = "pearson"))
}

# Base de datos 4 8 10
set.seed(10)
dat <- map_dfr(seq_len(25), ~data %>%
                 sample_n(sample(5:500, 1)) %>% 
                 mutate(muestra = as.factor(.x)))

count <- dat %>%
  group_by(muestra) %>%
  tally()

# ggplot(count, aes(x = n)) +
#   geom_histogram()

cors <- ddply(dat, .(muestra), summarise,
              r = round(corfun(V1, V2)$estimate, 2),
              n = corfun(V1, V2)$parameter,
              p = round(corfun(V1, V2)$p.value, 3))

ggplot(cors, aes(x = r)) +
 geom_histogram()

ggplot(cors, aes(x = r, y = n)) +
  geom_point() + 
  geom_vline(xintercept = 0.25, linetype="dotted") +
  labs(x = "Tamaño de efecto",
       y = "Tamaño de muestra") +
  theme_classic2()

# Agregar valores r (coeficientes de correlación)
dat <- merge(dat, cors, by = "muestra", all = T)

# Figuras (todas quedarán guardadas en el directorio de trabajo)
cornull <- cor.test(dat$V1, dat$V2,
                   alternative = "two.sided", method = "pearson")

# Figura hipótesis nula
ggplot(dat, aes(x = V1, y = V2)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  #geom_rug() + 
  annotate(geom = "text", label = paste0("n = ", number(cornull$parameter)),
           x = min(dat$V1), y = max(dat$V2), vjust = 1, hjust = -0.1, colour = "black",
           size = 8) +
  annotate(geom = "text", label = paste0("r = ", round(cornull$estimate, 2)),
           x = min(dat$V1), y = min(dat$V2), vjust = -1, hjust = -0.1, colour = "black",
           size = 8) +
  theme_void()

#Figuras muestras seleccionadas
nu <- unique(dat$muestra)

fig.cor <- list()
for (i in 1:length(nu)) {
  p1 =  ggplot(dat, aes(x = V1, y = V2)) + 
    geom_point(alpha = 0.05, size = 0.1) + 
    geom_smooth(method = "lm", se = FALSE, color = "black") + 
    #geom_rug() +
    geom_point(data = subset(dat, dat$muestra == nu[i]), color = "#f68212", size = 0.1, alpha = 0.5) + 
    geom_smooth(data = subset(dat, dat$muestra == nu[i]), method = "lm", se = FALSE, color = "#f68212") +
    annotate(geom = "text", label = paste0("n = ", number(cors$n[nu[i]])),
             x = min(dat$V1), y = max(dat$V2), vjust = 1, hjust = -0.1, colour = "black",
             size = 4) +
    annotate(geom = "text", label = paste0("r = ", round(cors$r[nu[i]], 2)),
             min(dat$V1), y = min(dat$V2), vjust = -1, hjust = -0.1, colour = "black",
             size = 4) +
    theme_void()
  print(i)
  print(p1)
  fig.cor[[i]] <- p1
}

fig.cor <- lapply(fig.cor,
                  function(p) p + theme(plot.background = element_rect(color = "black")))

ggarrange(plotlist = fig.cor,
          ncol = 5,
          nrow = 5)
