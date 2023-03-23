library(faux)
library(tidyverse)
library(plyr)

dat <- rnorm_multi(n = 10000, 
                   mu = c(0, 20),
                   sd = c(1, 5),
                   r = c(-0.12), 
                   varnames = c("V1", "V2"),
                   empirical = TRUE)

ggplot(dat, aes(x = V1, y = V2)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# Correlación por muestra
corfun <- function(x, y) {
  corr = (cor.test(x, y,
                   alternative = "two.sided", method = "pearson"))
}

# Base de datos
dat <- map_dfr(seq_len(50), ~dat %>%
                 sample_n(sample(10:1000, 1)) %>% 
                 mutate(muestra = as.factor(.x)))

count <- dat %>%
  group_by(muestra) %>%
  tally() %>%
  ggplot(., aes(x = n)) +
  geom_histogram()

cors <- ddply(dat, .(muestra), summarise,
              r = round(corfun(V1, V2)$estimate, 2),
              n = corfun(V1, V2)$parameter,
              p = round(corfun(V1, V2)$p.value, 3))

# Agregar * a resultados significativos
cors$Sig <- ifelse(cors$p < 0.05, paste0("p = ", round(cors$p, 3), "*"), 
                   ifelse(cors$p < 0.01, paste0("p = ", round(cors$p, 3), "**"), 
                          ifelse(cors$p < 0.001, paste0("p = ", round(cors$p, 3), "***"), paste0("p = ", round(cors$p, 3)))))

# Agregar valores r (coeficientes de correlación)
dat <- merge(dat, cors, by = "muestra", all = T)

# Figuras (todas quedarán guardadas en el directorio de trabajo)
cornull = cor.test(dat$V1, dat$V2,
                   alternative = "two.sided", method = "pearson")

# Figura hipótesis nula
p1 <- ggplot(dat, aes(x = V1, y = V2)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm", color = "black") + 
  #geom_rug() + 
  annotate(geom = "text", label = paste0("r = ", round(cornull$estimate, 2), 
                                         ", 95% IC ", paste0("[", round(cornull$conf.int[1], 2), ", ", round(cornull$conf.int[2], 2), "]"),
                                         ", p = ", round(cornull$p.value, 3)),
           x = 0.5*(min(dat$V1) + max(dat$V1)), y = max(dat$V2), vjust = 1, colour = "black") +
  ggsave("p1.png", width=5.1, height=5.1, dpi=400)

#Figuras muestras seleccionadas
set.seed(123)
nu <- sample(1:100, 5)
nu <- unique(dat$muestra)

fig.cor <- list()
for (i in 1:length(nu)) {
  p1 =  ggplot(dat, aes(x = V1, y = V2)) + 
    geom_point(alpha = 0.1, size = 0.2) + 
    geom_smooth(method = "lm", color = "black") + 
    #geom_rug() +
    geom_point(data = subset(dat, dat$muestra == nu[i]), color = "#f68212", size = 0.2) + 
    geom_smooth(data = subset(dat, dat$muestra == nu[i]), method = "lm", color = "#f68212", fill = "#f68212") +
    annotate(geom = "text", label = paste0("r = ", round(cors$r[nu[i]], 2), ", n = ", cors$n[nu[i]]),
             x = 0.5*(min(dat$V1) + max(dat$V1)), y = max(dat$V2), vjust = 1, colour = "black") +
    theme_void()
  print(i)
  print(p1)
  fig.cor[[i]] <- p1
}

ggarrange(plotlist = fig.cor,
          ncol = 5,
          nrow = 10)

# Distribución de valores r
N <- nrow(dat)
R <- 2500

data <- dat[,2:3]
cor.orig <- cor(data)
cor.boot <- NULL

for (i in 1:R) {
  idx <- sample.int(N, 100, replace = TRUE) 
  cor.boot[i] <- cor(data[idx, ])[1,2] 
}

cor.boot <- as.data.frame(cor.boot)
colnames(cor.boot) <- "r"

rect1 <- data.frame(xmin=-Inf, xmax=-0.2, ymin=0, ymax=Inf)
rect2 <- data.frame(xmin=0.2, xmax=Inf, ymin=0, ymax=Inf)

rdist <- ggplot(cor.boot, aes(x = r, y = ..density..)) +
  geom_histogram(bins = 80, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  geom_vline(xintercept = mean(cor.boot$r), color = "red", linetype = "dashed", size = 1) +
  ylim(c(0, 7)) +
  xlim(c(-0.4,0.4)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  labs(x = expression(italic(r)~" (Coeficiente de correlación de Pearson)"), 
       y = "Densidad") +
  annotate(geom = "text", label = paste0("Media ~ ", round(mean(cor.boot$r), 3)),
           x = -0.05, y = 6, vjust = 1, colour = "red", size = 5, angle = 90) +
  annotate(geom = "text", label = "Significativo (p < 0.05)",
           x = -0.3, y = 3, vjust = 1, colour = "blue", size = 4, angle = 90) +
  annotate(geom = "text", label = "Significativo (p < 0.05)",
           x = 0.3, y = 3, vjust = 1, colour = "blue", size = 4, angle = 90) +
  ggsave("rdist.png", width=5.1, height=5.1, dpi=400)

# Resultado hipotético (falso positivo)
rdistH1 <- ggplot(cor.boot, aes(x = r, y = ..density..)) +
  geom_histogram(bins = 80, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  geom_vline(xintercept = mean(cor.boot$r), color = "red", linetype = "dashed", size = 1) +
  ylim(c(0, 7)) +
  xlim(c(-0.4,0.4)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_vline(xintercept = -0.25, color = "blue", linetype = "dashed", size = 1) +
  labs(x = expression(italic(r)~" (Coeficiente de correlación de Pearson)"), 
       y = "Densidad") +
  ggsave("rdistH1.png", width=5.1, height=5.1, dpi=400)

# Distribución esperada del resultado hipotético
rdistH2 <- ggplot(cor.boot, aes(x = r, y = ..density..)) +
  geom_histogram(bins = 80, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  geom_vline(xintercept = mean(cor.boot$r), color = "red", linetype = "dashed", size = 1) +
  ylim(c(0, 7)) +
  xlim(c(-0.4,0.4)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_vline(xintercept = -0.25, color = "blue", linetype = "dashed", size = 1) +
  labs(x = expression(italic(r)~" (Coeficiente de correlación de Pearson)"), 
       y = "Densidad") +
  stat_function(fun = dnorm, n = 1000, args = list(mean = -0.25, sd = 0.1), inherit.aes = FALSE) +
  ggsave("rdistH2.png", width=5.1, height=5.1, dpi=400)