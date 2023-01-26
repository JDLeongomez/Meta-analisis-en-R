## ----setup, include = FALSE--------------------------------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(comment = NA)
def_hook <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options){
  out <- def_hook(x, options)
  return(paste("\\begin{ROut}{Consola de R: Output~\\thetcbcounter}
                \\begin{footnotesize}
                \\begin{verbatim}", 
               x,
               "\\end{verbatim}
                \\end{footnotesize}
                \\end{ROut}"))
})
library(robumeta)
library(metafor)
library(tidyverse)
library(ggpubr)
library(kableExtra)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## library(metafor)
## library(metaviz)
## library(dplyr)
## library(forcats)


## ----------------------------------------------------------------------------------------------------------------------------------
dat <- get(data(dat.molloy2014))


## ----------------------------------------------------------------------------------------------------------------------------------
dat


## ----molloy2014--------------------------------------------------------------------------------------------------------------------
dat <- get(data(dat.molloy2014)) %>%
  mutate(study_id = 1:16)  %>% #crear columna study_id y agregar número del 1 al 16
  select(study_id, authors:quality) %>% #mover study_id como primera columna
  mutate_at(c("controls", # Transformar variables en factores
              "design",
              "a_measure",
              "c_measure",
              "quality"), 
            as.factor) %>% 
  mutate(controls = fct_relevel(controls, "none", "multiple"))


## ----estructuramod, echo = FALSE, message = FALSE----------------------------------------------------------------------------------
kable(dat, 
      linesep = "",
      booktabs = TRUE,
      caption = "Estructura de la base de datos con estudios numerados") %>% 
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  footnote(general = "Datos tomados de Molloy et al. (2013).",
           general_title = "Nota:",
           footnote_as_chunk = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
dat <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = dat)


## ----estructuramod2, echo = FALSE, message = FALSE---------------------------------------------------------------------------------
kable(dat, 
      linesep = "",
      booktabs = TRUE,
      caption = "Estructura de la base de datos, con transformación de los r de Pearon a z de Fisher") %>% 
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>% 
  footnote(general = "Las nuevas columnas creadas usando la función \\\\texttt{escalc} 
           (\\\\texttt{yi} como tamaño de efecto y \\\\texttt{vi} como varianza) están 
           resaltadas en naranja",
           general_title = "Nota:",
           footnote_as_chunk = TRUE,
           escape = FALSE) %>% 
  column_spec(12:13, background = "#f68212")


## ----------------------------------------------------------------------------------------------------------------------------------
res <- rma(yi = yi, vi = vi, data = dat)


## ----------------------------------------------------------------------------------------------------------------------------------
res


## ----message = FALSE---------------------------------------------------------------------------------------------------------------
tanh(0.1499)
library(psych)
fisherz2r(0.1499)


## ----rvsz, echo = FALSE, message = FALSE, warning = FALSE, fig.height = 4, fig.cap = "Asociación entre coeficientes de correlación *r* de Pearson (eje *X*), y su transformación a *z* de Fisher (eje *Y*). La línea naranja representa la asociación entre valores *r* y *z*; como referencia, la línea negra punteada representa igualdad entre ejes (*y* = *x*). Como se puede ver, cuando *r* está aproximadamente entre -0.4 y 0.4 (rectángulo gris), los valores *r* y *z* son casi idénticos. Para valores más extremos, el valor de *z* se aleja progresivamente del valor de *r*."----

library(tidyquant)

rVsz <- tibble(r = c(seq(-0.999, -0.901, by = 0.001), 
                     seq(-0.9, 0.9, by = 0.01), 
                     seq(0.901, 0.999, by = 0.001))) %>%
  mutate(z = atanh(r))
  
ggplot(rVsz, aes(x = r, y = z)) +
  annotate("rect", xmin = -0.4, xmax = 0.4, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("text", x = 0, y = 0.2, 
                label = expression(paste("Mínima diferencia entre ", italic(r)," y ", italic(z))), 
           parse = TRUE,
           angle = 16,
           size = 2) +
  geom_smooth(se = FALSE, color = "#F68212") +
  geom_segment(aes(x = -1, xend = 1, y = -1, yend = 1), lty = "dotted", alpha = 0.1) +
  xlim(-1, 1) +
  scale_x_continuous(breaks = round(seq(-1, 1, by = 0.1),1)) +
  scale_y_continuous(breaks = round(seq(-3, 3, by = 0.2),1)) +
  labs(x =  expression(paste("Coeficiente de correlación (", italic("r"), ") de Pearson")),
       y = expression(paste("Transformación a ", italic(" z"), " de Fisher"))) +
  theme_tq()


## ----------------------------------------------------------------------------------------------------------------------------------
confint(res)


## ----------------------------------------------------------------------------------------------------------------------------------
inf.res <- influence(res)


## ----------------------------------------------------------------------------------------------------------------------------------
inf.res


## ----infplot, fig.height = 7, fig.cap = "Diagnóstico de influencia. Estudios particularmente influyentes serían representados con un punto rojo. Los números 1 a 16 en el eje *X* representan cada estudio, como lo definimos en columna \\texttt{study\\_id} de la Tabla \\@ref(tab:estructuramod). En este caso, no hay ningún estudio que se considere demasiado influyente, por lo éste análisis sugiere que podemos estar tranquilos con nuestro meta-análisis."----
plot(inf.res)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## forest(res)


## ----for-plot1, echo = FALSE, fig.height = 5.5, fig.cap = "Forest plot básico de [metafor](https://www.metafor-project.org/doku.php). Para cada estudio meta-analizado, tenemos el efecto (correlación, en este caso en valores *z* de Fisher), así como sus intervalos de confianza entre paréntesis cuadrados. Esta misma información está representada gráficamente, con los cuadrados representando el efecto de cada estudio así como sus intervalos de confianza como barras de error, y el tamaño de muestra representado por el tamaño del cuadrado. Bajo estos resultados, tenemos nuestro meta-análisis, con el mismo formato en texto, pero representando el efecto y sus intervalos de confianza con un diamante."----
par(mar = c(4,0,0,0))
forest(res)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## # forest plot con anotaciones adicionales
## forest(res, cex = 0.75, xlim = c(-1.6, 1.6),
##        slab = paste(dat$authors, dat$year, sep = ", "),
##        showweights = TRUE,
##        xlab = "Coeficiente de correlación transformado en z de Fisher",
##        digits = c(2,3L),
##        mlab = bquote(paste("Modelo EA: Q(", .(res$k - res$p), ") = ",
##      .(formatC(res$QE, digits = 2, format = "f")),
##      ", p ", .(scales::pvalue(res$pval)), "; ", I^2, " = ",
##      .(formatC(res$I2, digits = 1, format = "f")), "%")))
## # agregar encabezados a las columnas (valores de X y Y deben ser ajustados)
## op <- par(cex = 0.8, font = 2)
## text(x = -1.6, y = 18, labels = "Autor(es), Año", pos = 4)
## text(x = 0, y = 18, labels = "Efecto e IC", pos = 4)
## text(x = 1, y = 18, labels = "Peso", pos = 2)
## text(x = 1.6, y = 18, labels = "Corr. [95% IC]", pos = 2)


## ----for-plot2, echo = FALSE, fig.height = 5, fig.cap = "*Forest plot* anotado, creado con [metafor](https://www.metafor-project.org/doku.php). En esta versión agregué algunos encabezados en español, así como estadísticos generales del modelo de meta-análisis. Modelo EA se refiere al modelo meta-analizado, de efectos aleatorios."----
# forest plot con anotaciones adicionales
par(mar = c(4,0,0,0))
forest(res, cex = 0.75, xlim = c(-1.6, 1.6),
       slab = paste(dat$authors, dat$year, sep = ", "),
       showweights = TRUE,
       xlab = "Coeficiente de correlación transformado en z de Fisher",
       digits = c(2,3L),
       mlab = bquote(paste("Modelo EA: Q(", .(res$k - res$p), ") = ",
     .(formatC(res$QE, digits = 2, format = "f")),
     ", p ", .(scales::pvalue(res$pval)), "; ", I^2, " = ",
     .(formatC(res$I2, digits = 1, format = "f")), "%")))
# agregar encabezados a las columnas (valores de X y Y deben ser ajustados)
op <- par(cex = 0.8, font = 2) 
text(x = -1.6, y = 18, labels = "Autor(es), Año", pos = 4)
text(x = 0, y = 18, labels = "Efecto e IC", pos = 4)
text(x = 1, y = 18, labels = "Peso", pos = 2)
text(x = 1.6, y = 18, labels = "Corr. [95% IC]", pos = 2)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## # A. Variante "classic" (no tiene que ser definida, pues es la opción por defecto)
## viz_forest(res,
##            study_labels = paste(dat$authors, dat$year, sep = ", "),
##            xlab = "Correlación",
##            annotate_CI = TRUE,
##            summary_label = "Resumen",
##            text_size = 2.6,
##            x_trans_function = tanh)
## 
## # B. Variante "thick"
## viz_forest(res,
##            study_labels = paste(dat$authors, dat$year, sep = ", "),
##            xlab = "Correlación",
##            variant = "thick",
##            col = "Greens",
##            annotate_CI = TRUE,
##            summary_label = "Resumen",
##            text_size = 2.6,
##            x_trans_function = tanh)
## 
## # C. Variante "rain"
## viz_forest(res,
##            study_labels = paste(dat$authors, dat$year, sep = ", "),
##            xlab = "Correlación",
##            variant = "rain",
##            col = "Oranges",
##            annotate_CI = TRUE,
##            summary_label = "Resumen",
##            text_size = 2.6,
##            x_trans_function = tanh)


## ----for-plot3, echo = FALSE, fig.height = 3---------------------------------------------------------------------------------------
library(metaviz)
p.classic <- viz_forest(res, 
                        study_labels = paste(dat$authors, dat$year, sep = ", "),
                        xlab = "Correlación", 
                        annotate_CI = TRUE,
                        summary_label = "Resumen",
                        text_size = 2.6,
                        x_trans_function = tanh)

ggarrange(p.classic,
          labels = c("A"))


## ----for-plot3b, echo = FALSE, fig.height = 6, warning = FALSE, fig.cap = 'Variantes de *forest plots* creados con  [metaviz](https://cran.r-project.org/web/packages/metaviz/vignettes/metaviz.html). **A.** Variante clásica (opción por defecto). **B.** Variante "thick" y escala de colores "Greens". **C.** Variante "rain" y escala de colores "Oranges".'----
p.thick <- viz_forest(res, 
                      study_labels = paste(dat$authors, dat$year, sep = ", "),
                      xlab = "Correlación", 
                      variant = "thick",
                      col = "Greens",
                      annotate_CI = TRUE,
                      summary_label = "Resumen",
                      text_size = 2.6,
                      x_trans_function = tanh)

p.rain <- viz_forest(res,
                     study_labels = paste(dat$authors, dat$year, sep = ", "),
                     xlab = "Correlación", 
                     variant = "rain",
                     col = "Oranges",
                     annotate_CI = TRUE,
                     summary_label = "Resumen",
                     text_size = 2.6,
                     x_trans_function = tanh)

ggarrange(p.thick, p.rain,
          labels = c("B", "C"),
          nrow = 2)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## funnel(res)


## ----funnel-plot1, echo = FALSE, fig.height = 5, fig.cap = "*Funnel plot* básico de [metafor](https://www.metafor-project.org/doku.php). Para cada estudio meta-analizado, tenemos el efecto (correlación, en este caso en valores *z* de Fisher) en el eje *X*, así como su error estándar en el eje *Y*. La línea punteada vertical representa el efecto meta-analizado que hemos encontrado, así que podemos ver los estudios que encontraron un efecto mayor (derecha) o menor (izquierda) que éste. A primera vista no parece haber mucha asimetría, pero es importante tener en cuenta que es un análisis muy subjetivo."----
par(mar = c(4,4,0,1))
funnel(res)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## funnel(res,
##        xlab = "Coeficiente de correlación transformado en z de Fisher",
##        ylab = "Error estándar")


## ----funnel-plot1a, echo = FALSE, fig.height = 5, fig.cap = "Funnel plot básico de [metafor](https://www.metafor-project.org/doku.php), con títulos de ejes en español. Para cada estudio meta-analizado, tenemos el efecto (correlación, en este caso en valores *z* de Fisher) en el eje *X*, así como su error estándar en el eje *Y*. La línea punteada vertical representa el efecto meta-analizado que hemos encontrado, así que podemos ver los estudios que encontraron un efecto mayor (derecha) o menor (izquierda) que éste."----
par(mar = c(4,4,0,1))
funnel(res, 
       xlab = "Coeficiente de correlación transformado en z de Fisher",
       ylab = "Error estándar")


## ----funnel-plot2, fig.height = 4.5, warning = FALSE, fig.cap = "*Funnel plot* creado con [metaviz](https://cran.r-project.org/web/packages/metaviz/vignettes/metaviz.html). En azul, se representa el área donde estudios, según su error (y su tamaño de muestra), tendrían un efecto significativo al 5% (i.e. $p$ > 0.05), y fuera de ésta, donde tendrían un efecto significativo al 1% (i.e. $p$ > 0.01). La línea negra vertical representa el efecto meta-analizado, y el triángulo a partir de su inicio, el área donde se ubican los estudios que no se diferencian significativamente del resultado del meta-análisis. La línea roja punteada, representa la regresión de Egger."----
viz_funnel(res, 
           egger = TRUE,
           x_trans_function = tanh,
           ylab = "Error estándar",
           xlab = "Coeficiente de correlación")


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## # A. Escala de poder discreta
## viz_sunset(res,
##            contours = TRUE,
##            x_trans_function = tanh)
## 
## # B. Escala de poder contínua
## viz_sunset(res,
##            contours = TRUE,
##            x_trans_function = tanh,
##            power_contours = "continuous")


## ----funnel-plot3, message = TRUE, echo = FALSE, fig.height = 8.5, fig.width = 6, warning = FALSE, fig.cap = "Dos versiones de funnel plot creados con [metaviz](https://cran.r-project.org/web/packages/metaviz/vignettes/metaviz.html), usando la función viz-sunset, que estima el poder de cada estudio para detectar un efecto de interés. **A.** Poder representado por bandas dicretas de color. **B.** Poder representado de manera contínua en una escala de color. En ambos casos, y tal como en la Fig. \\@ref(fig:funnel-plot2), el efecto real está representado como una línea vertical, y el triángulo a partir de su inicio representa el área donde se ubican los estudios que no se diferencian significativamente del resultado del meta-análisis."----
p.suna <- viz_sunset(res, 
                     contours = TRUE,
                     x_trans_function = tanh)
p.sunb <- viz_sunset(res, 
                     contours = TRUE,
                     x_trans_function = tanh,
                     power_contours = "continuous")
  
ggarrange(p.suna, p.sunb,
          labels = "AUTO",
          nrow = 2)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## regtest(res)


## ----echo = FALSE------------------------------------------------------------------------------------------------------------------
regtest(res)


## ----------------------------------------------------------------------------------------------------------------------------------
tf <- trimfill(res)
tf


## ----tf-plot1, fig.cap = "*Funnel plot* básico de [metafor](https://www.metafor-project.org/doku.php) usando el método de recorte y relleno (*trim and fill*). En negro los estudios meta-analizados; en blanco, los estudios *rellenados*."----
funnel(tf, 
       xlab = "Coeficiente de correlación transformado en z de Fisher",
       ylab = "Error estándar")


## ----tf-plot2, fig.cap = "*Funnel plot* creado con [metaviz](https://cran.r-project.org/web/packages/metaviz/vignettes/metaviz.html) usando el método de recorte y relleno (*trim and fill*). En negro los estudios meta-analizados; en blanco, los estudios *rellenados*."----
viz_funnel(res, 
           contours_col = "Oranges",
           trim_and_fill = TRUE, 
           trim_and_fill_side = "left", #IMPORTANTE
           egger = TRUE,
           x_trans_function = tanh,
           ylab = "Error estándar",
           xlab = "Coeficiente de correlación") +
  geom_vline(xintercept = 0, linetype = "dotted")


## ----message = FALSE---------------------------------------------------------------------------------------------------------------
library(weightr)


## ----------------------------------------------------------------------------------------------------------------------------------
wf <- weightfunct(effect = dat$yi, v = dat$vi, table = TRUE)
wf


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## #se debe tener instalado el paquete devtools
## library(devtools)
## install_github("dsquintana/metameta")


## ----------------------------------------------------------------------------------------------------------------------------------
library(metameta)


## ----------------------------------------------------------------------------------------------------------------------------------
dat.power <- summary(dat) %>%
  select(yi, ci.lb, ci.ub) %>%
  rename(lower = ci.lb, upper = ci.ub)

power <- mapower_ul(dat = dat.power, observed_es = 0.15, name = "Molloy et al. 2014")

power_list <- list(power$power_median_dat)
power_dat <- power$dat

power_list

power_dat


## ----------------------------------------------------------------------------------------------------------------------------------
power.plot <- firepower(power_list)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## power.plot


## ----fire-plot1, fig.height = 2, echo = FALSE, fig.cap = "Fireplot básico de [metameta](https://www.dsquintana.blog/metameta-r-package-meta-analysis/), para un análisis de poder de nuestro meta-análisis. *Observed* hace referencia al tamaño de efecto observado en nuestro meta-análisis original; en este caso, 0.15."----
power.plot$fp_plot


## ----fire-plot2, fig.height = 2, message = FALSE, fig.cap = "Fireplot básico de [metameta](https://www.dsquintana.blog/metameta-r-package-meta-analysis/), para un análisis de poder de nuestro meta-análisis, con el texto traducido a español y con la leyenda en una escala discreta para facilitar su lectura. *Observado* hace referencia al tamaño de efecto observado en nuestro meta-análisis original (en este caso, 0.15)."----
power.plot$fp_plot +
  xlab("Tamaño de efecto") +
  guides(fill = guide_legend(title = "Poder", 
                             reverse = TRUE)) +
  scale_x_discrete(labels = c("es_observed" = "Observado",
                              "es01" = 0.1,    
                              "es02" = 0.2,
                              "es03" = 0.3,
                              "es04" = 0.4,    
                              "es05" = 0.5,    
                              "es06" = 0.6,    
                              "es07" = 0.7,
                              "es08" = 0.8,
                              "es09" = 0.9,
                              "es1"  = 1))


## ----------------------------------------------------------------------------------------------------------------------------------
l_one <- leave1out(res)
l_one


## ----cache = TRUE------------------------------------------------------------------------------------------------------------------
gp <- gosh(res)


## ----------------------------------------------------------------------------------------------------------------------------------
plot(gp, breaks = 100,
     labels = c("Coeficiente de correlación transformado en z de Fisher", 
                expression(paste("I"^"2"))))


## ----------------------------------------------------------------------------------------------------------------------------------
mgosh <- gp$res
mean_est <- mean(mgosh$estimate, na.rm = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
dat_mo <- get(data(dat.molloy2014)) 
dat_mo <- escalc(measure = "ZCOR", 
                ri = ri, ni = ni, data = dat_mo)

dat_mo[12,11] = 0.7 
dat_mo[15,11] = 0.6 

res_mo <- rma.uni(yi, vi, data=dat_mo) 

forest(res_mo)


## ----cache = TRUE------------------------------------------------------------------------------------------------------------------
gp_mo <- gosh(res_mo)


## ----------------------------------------------------------------------------------------------------------------------------------
plot(gp_mo, breaks = 100)


## ----fig.cap = "Se puede ver en las distribuciones del eje X (distribuciones arriba) cómo los meta análisis que incluyen ese estudio tienden a arrojar estimados más fuertes para la asociación, sesgando el resultado hacia valores más extremos."----
plot(gp_mo, out = 12, breaks = 100,
     labels = c("Coeficiente de correlación transformado en z de Fisher", 
                expression(paste("I"^"2"))))


## ----------------------------------------------------------------------------------------------------------------------------------
res.modage <- rma(yi = yi, vi = vi, mods = ~meanage, data = dat)


## ----------------------------------------------------------------------------------------------------------------------------------
res.modage


## ----pred-mod1---------------------------------------------------------------------------------------------------------------------
# Calcular efecto ajustado para diferentes edades
pred.res.modage <- predict(res.modage, newmods = seq(20, 80, by = 10)) %>% 
  as.data.frame() %>% 
  mutate_all(~round(., 3)) %>% 
  mutate(meanage = seq(20, 80, by = 10)) %>% 
  select(7, 1:6)
# Ver la tabla
pred.res.modage


## ----reg-plot1, fig.height = 6, fig.cap = "Gráfico de dispersión meta-analítico (*Meta-Analytic Scatter Plot*). El tamaño de los puntos es proporcional al peso que recibieron los estudios en el meta-análisis (puntos más grandes para los estudios con más peso, pues tienen un tamaño de muestra mayor y con un menor error estimado). La línea negra representa el efecto previsto en función del predictor (en este caso \\texttt{meanage}, edad promedio), que por supuesto coincide  con las predicciones del objeto `pred.res.modage` (Output 11, en la sección \\@ref(pred-mods)); la banda gris demilimata por líneas punteadas representa el intervalo de confianza del 95%."----
regplot(res.modage,
        ylab = "Coeficiente de correlación transformado en z de Fisher",
        xlab = "Edad promedio del estudio")


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## # forest plot con anotaciones adicionales
## forest(res.modage,  cex = 0.75, xlim = c(-1.6, 1.6),
##        slab = paste(dat$authors, dat$year, sep = ", "),
##        showweights = TRUE,
##        xlab = "Coeficiente de correlación transformado en z de Fisher",
##        digits = c(2,3L))
## # agregar encabezados a las columnas (valores de X y Y deben ser ajustados)
## par(cex = 0.8, font = 2)
## text(x = -1.6, y = 18, labels = "Autor(es), Año", pos = 4)
## text(x = 0, y = 18, labels = "Efecto e IC", pos = 4)
## text(x = 1, y = 18, labels = "Peso", pos = 2)
## text(x = 1.6, y = 18, labels = "Corr. [95% IC]", pos = 2)


## ----for-plot-mod1, echo = FALSE, warning = FALSE, fig.height = 3.5, fig.cap = "Forest plot básico de [metafor](https://www.metafor-project.org/doku.php), para un meta-análisis incluyendo la edad promedio de los participantes como moderador. En la ilustración gráfica, además de los efectos originales, se puede ver el efecto de cada estudio estimado cuando se incluye el moderador como polígonos (diamantes) de color gris. Sin embargo, ya no obtenemos una fila al final representando el efecto promediado del meta-análisis, ya que no tenemos un solo efecto."----
# forest plot con anotaciones adicionales
par(mar = c(4,0,0,0))
forest(res.modage,  cex = 0.75, xlim = c(-1.6, 1.6),
       slab = paste(dat$authors, dat$year, sep = ", "),
       showweights = TRUE,
       xlab = "Coeficiente de correlación transformado en z de Fisher",
       digits = c(2,3L))
# agregar encabezados a las columnas (valores de X y Y deben ser ajustados)
par(cex = 0.8, font = 2)
text(x = -1.6, y = 18, labels = "Autor(es), Año", pos = 4)
text(x = 0, y = 18, labels = "Efecto e IC", pos = 4)
text(x = 1, y = 18, labels = "Peso", pos = 2)
text(x = 1.6, y = 18, labels = "Corr. [95% IC]", pos = 2)


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## metafor::funnel(res.modage,
##                 xlab = "Valor residual",
##                 ylab = "Error estándar")


## ----funnel-plot-mod1, echo = FALSE, fig.height = 5, fig.cap = "Funnel plot básico de [metafor](https://www.metafor-project.org/doku.php), para un meta-análisis incluyendo la edad promedio de los participantes como moderador, y con títulos de los ejes en español. La línea punteada vertical representa el efecto meta-analizado que hemos encontrado, así que podemos ver los estudios que encontraron un efecto mayor (derecha de la línea punteada) o menor (izquierda) de éste. **Nota**: Para evitar confunción entre las funciones `funnel` de los paquetes `weightr` y `metafor`, en este caso he usado el comando \\texttt{metafor::funnel} para pedirle a R explícitamente que use la función `funnel` del paquete `metafor`."----
par(mar = c(4,4,0,1))
metafor::funnel(res.modage,
                xlab = "Valor residual",
                ylab = "Error estándar")


## ----------------------------------------------------------------------------------------------------------------------------------
res.contr <- rma(yi = yi, vi = vi, mods = ~controls, data = dat)
res.contr


## ----------------------------------------------------------------------------------------------------------------------------------
pred.res.contr <- predict(res.contr, newmods = c(0, 1)) %>% 
  as.data.frame() %>% 
  mutate_all(~round(., 3)) %>%
  mutate(controls = levels(dat$controls)) %>% 
  rename(yi = pred) %>% 
  select(7, 1:6)
pred.res.contr


## ----reg-plot2, fig.height = 6, fig.cap = "Gráfico de dispersión meta-analítico (*Meta-Analytic Scatter Plot*) básico de [metafor](https://www.metafor-project.org/doku.php) creado con la función \\texttt{regplot}. El tamaño de los puntos es proporcional al peso que recibieron los estudios en el meta-análisis (puntos más grandes para los estudios con más peso, pues tienen un tamaño de muestra mayor y con un menor error estimado). La línea negra representa el efecto previsto en función del predictor (en este caso \\texttt{controls}, controles). La banda gris demilimata por líneas punteadas representa el intervalo de confianza del 95%. Dado que la variable moderadora es categórica, el modelo genera variables *dummy* asignando valores de 0 y 1 a los niveles de esta variable (en este caso, 0 = ningúun control; 1 = múltiples controles), tal y como se describe en el eje *X*. Para una versión más apropiada, ver Fig. \\@ref(fig:reg-plot3)."----
regplot(res.contr,
        ylab = "Coeficiente de correlación transformado en z de Fisher",
        xlab = "Controles (0 = ninguno; 1 = múltiples)")


## ----eval = FALSE------------------------------------------------------------------------------------------------------------------
## # Definir base de datos, así como ejes X (controls) y Y (yi)
## ggplot(dat, aes(x = controls, y = yi)) +
##   # Agregar puntos para cada estudio, con tamaño y color según tamaño de muestra (ni)
##   geom_point(aes(size = ni, color = ni),
##              alpha = 0.5) +
##   # Definir escala de colores
##   scale_colour_gradient(low = "red",
##                         high = "blue") +
##   # Definir rango de tamaño de los puntos
##   scale_size_continuous(range = c(1, 10)) +
##   # Combinar leyendas de colores y tamaños de puntos en una sola leyenda
##   guides(color = guide_legend(),
##          size = guide_legend()) +
##   # Traducir etiquetas del eje X en español
##   scale_x_discrete(labels=c("none" = "Ninguno",
##                             "multiple" = "Múltiples")) +
##   # Cambiar títulos de ejes a español
##   labs(x = "Controles",
##        y = "Coeficiente de correlación \ntransformado en z de Fisher ") +
##   # Agregar barras de error para cada categoría, con base en predicción hecha en sección 4.2.1
##   geom_errorbar(data = pred.res.contr,
##                 mapping = aes(ymin = ci.lb, ymax = ci.ub),
##                 width = 0.1,
##                 color = "black") +
##   # Agregar puntos blancos representando predicción para cada categoría hecha en sección 4.2.1
##   geom_point(data = pred.res.contr,
##              shape = 21, size = 3,
##              color = "black", fill = "white") +
##   # Cambiar tptitulo de leyenda
##   labs(color = "Tamaño de \nmuestra",
##        size = "Tamaño de \nmuestra")


## ----reg-plot3, echo = FALSE, fig.cap = "Gráfico de dispersión meta-analítico (*Meta-Analytic Scatter Plot*) creado manualmente con [ggplot2](https://ggplot2.tidyverse.org/) para hacer una mejor representación de un moderador categírico. Los puntos de colores representan el coeficiente de correlación en función de la presencia o ausencia de controles. El tamaño de los puntos es proporcional al tamaño de muestra de los estudios inluidos en el meta-análisis (puntos más grandes y azules para los estudios con mayor tamaño de muestra). Los puntos blancos superpuetos representan el efecto estimado para cada categoría, y las barras de error representan los intervalos de confianza del 95%."----
ggplot(dat, aes(x = controls, y = yi)) +
  geom_point(aes(size = ni, color = ni),
             alpha = 0.5) +
  scale_colour_gradient(low = "red",
                        high = "blue") +
  scale_size_continuous(range = c(1, 10)) +
  guides(color = guide_legend(), 
         size = guide_legend()) +
  scale_x_discrete(labels=c("none" = "Ninguno", 
                            "multiple" = "Múltiples")) +
  labs(x = "Controles", 
       y = "Coeficiente de correlación \ntransformado en z de Fisher ") +
  geom_errorbar(data = pred.res.contr,
                mapping = aes(ymin = ci.lb, ymax = ci.ub),
                width = 0.1,
                color = "black") +
  geom_point(data = pred.res.contr,
             shape = 21, size = 3,
             color = "black", fill = "white") +
  labs(color = "Tamaño de \nmuestra",
       size = "Tamaño de \nmuestra") +
  theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------------------
citation("dplyr")


## ----echo = FALSE------------------------------------------------------------------------------------------------------------------
library(pander)
pander(sessionInfo(), locale = FALSE)

