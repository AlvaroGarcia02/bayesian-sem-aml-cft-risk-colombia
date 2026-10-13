#==============================================================================
#===                 CÓDIGO R PARA EL CASO DE ESTUDIO                       ===
#==============================================================================
#===   Estadística Bayesiana - M. Paula Arévalo F. y Álvaro A. García G.    ===
#===                              2026-1                                    ===
#==============================================================================

rm(list = ls())
# AVISO: El archivo LAFT.xlsx fue editado en Excel por los autores con las
# transformaciones de las variables discutidas en el Anexo C del informe y NO se
# puede trabajar en este script con el archivo original LAFT.xlsx que aparece en
# el Dropbox del Caso de estudio 2026-1 en el site del profesor Juan Sosa. Se
# recomienda crear una carpeta en su ordenador para guardar el archivo
# transformado (que aparece en la carpeta 'Data' en GitHub), y guardar este
# script, pues las ejecuciones de los muestreadores de Gibbs están configuradas
# para almacenar las muestras en la ruta en esa misma carpeta, y no dentro de la
# memoria de RStudio. Una vez creada la carpeta de trabajo, puede fijar a
# continuación la ruta:
setwd("C:/Users/Álvaro García/OneDrive/Documentos/R/a58 Código Caso de estudio - EB - 2026-1")

#==============================================================================
# PARTE I: Finalización de la construcción de los puntajes temáticos y los in-
#          dicadores observados
#==============================================================================

library(readxl) 
library(Hmisc)

LAFT_Cont_Est   <- read_excel("LAFT.xlsx", sheet = 1) #Datos de contexto estructural
LAFT_Cont_Pol   <- read_excel("LAFT.xlsx", sheet = 2) #Datos de contexto político
LAFT_Contrab    <- read_excel("LAFT.xlsx", sheet = 3) #Datos de contrabando
LAFT_Sec_Susc   <- read_excel("LAFT.xlsx", sheet = 4) #Datos de sectores susceptibles al lavado de activos 
LAFT_Terrorismo <- read_excel("LAFT.xlsx", sheet = 5) #Datos de terrorismo
LAFT_Conflicto  <- read_excel("LAFT.xlsx", sheet = 6) #Datos de conflicto
LAFT_Criminal   <- read_excel("LAFT.xlsx", sheet = 7) #Datos de criminalidad
LAFT_Narcotraf  <- read_excel("LAFT.xlsx", sheet = 8) #Datos de narcotráfico

y1 <- unname(unlist(LAFT_Terrorismo[ , "y_i,1"]))
y2 <- unname(unlist(LAFT_Conflicto[ , "y_i,2"]))
y3 <- unname(unlist(LAFT_Criminal[ , "y_i,3"]))
y4 <- unname(unlist(LAFT_Narcotraf[ , "y_i,4"]))

# Número de municipios:
n <- length(y1); n

Y <- cbind(y1, y2, y3, y4) 
dim(Y)

par(mfrow = c(2, 2))
hist(y1); hist(y2); hist(y3); hist(y4)
par(mfrow = c(1, 1))

summary(y1)
summary(y2)
summary(y3)
summary(y4)

x1 <- unname(unlist(LAFT_Cont_Est[ , "x_i,1"]))
x2 <- unname(unlist(LAFT_Cont_Pol[ , "x_i,2"]))
x3 <- unname(unlist(LAFT_Contrab[ , "x_i,3"]))
x4 <- unname(unlist(LAFT_Sec_Susc[ , "x_i,4"]))

X <- cbind(x1, x2, x3, x4) 
dim(X)

y1_plus = y1[y1!=0]
n1_plus = length(y1_plus); n1_plus
y2_plus = y2[y2!=0]
n2_plus = length(y2_plus); n2_plus
y3_plus = y3[y3!=0]
n3_plus = length(y3_plus); n3_plus
y4_plus = y4[y4!=0]
n4_plus = length(y4_plus); n4_plus
nj_plus = c(n1_plus, n2_plus, n3_plus, n4_plus)

y1_0 = y1[y1==0]
n1_0 = length(y1_0); n1_0
y2_0 = y2[y2==0]
n2_0 = length(y2_0); n2_0
y3_0 = y3[y3==0]
n3_0 = length(y3_0); n3_0
y4_0 = y4[y4==0]
n4_0 = length(y4_0); n4_0
nj_0 = c(n1_0, n2_0, n3_0, n4_0)

# Identificadores de pertenencia a los departamentos:
index <- unname(unlist(LAFT_Narcotraf[ , "index"]))
mun_dep <- split(seq_len(n), index)

# Base de datos depurada:
mun <- unname(unlist(LAFT_Terrorismo[ , "COD_MUN"]))
dep <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27",
         "41", "44", "47", "50", "52", "54", "63", "66", "68", "70", "73", "76",
         "81", "85", "86", "88", "91", "94", "95", "97", "99")
deptos <- c("Antioquia", "Atlántico", "Bogotá, D.C.", "Bolívar", "Boyacá",
            "Caldas", "Caquetá", "Cauca", "Cesar", "Córdoba", "Cundinamarca",
            "Chocó", "Huila", "La Guajira", "Magdalena", "Meta", "Nariño",
            "Norte de Santander", "Quindío", "Risaralda", "Santander", "Sucre",
            "Tolima", "Valle del Cauca", "Arauca", "Casanare", "Putumayo",
            "San Andrés", "Amazonas", "Guainía", "Guaviare", "Vaupés", "Vichada")
data <- data.frame(MPIO_CDPMP = mun, DPTO_CCDGO = dep[index],
                   x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                   y1 = y1, y2 = y2, y3 = y3, y4 = y4)
data$MPIO_CDPMP <- sprintf("%05d", data$MPIO_CDPMP) #modificar formato de códigos de municipios

#==============================================================================
# PARTE II: Análisis exploratorio descriptivo de los datos
#==============================================================================

library(ggplot2)
library(RColorBrewer)
library(sf)
library(grid) 
library(dplyr)
library(viridis)
col <- brewer.pal(9, "Set1")[1:9]

Terrorismo<-datosy1$`y_i,1`
Conflicto<-datosy2$`y_i,2`
Criminalidad<-datosy3$`y_i,3`
Narcotráfico<-datosy4$`y_i,4`
Y <- data.frame(
  Terrorismo = Terrorismo,
  Conflicto = Conflicto,
  Criminalidad = Criminalidad,
  Narcotráfico = Narcotráfico
)
R <- cor(Y)


library(corrplot)

corrplot(R,
         method = "color",
         type = "upper",
         addCoef.col = "white",   
         tl.col = "black",        
         tl.srt = 45)



#####Mantarraya 
#x1
ggplot(datosx1,
       aes(x = "", y = `x_i,1`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Puntaje temático de contexto estructural"
  ) +
  theme_bw()
#x2
ggplot(datosx2,
       aes(x = "", y = `x_i,2`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Puntaje temático de contexto político"
  ) +
  theme_bw()
#x3
ggplot(datosx3,
       aes(x = "", y = `x_i,3`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Puntaje temático de contrabando"
  ) +
  theme_bw()
#x4
ggplot(datosx4,
       aes(x = "", y = `x_i,4`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Puntaje temático de sector primario y minero"
  ) +
  theme_bw()
#y1
ggplot(datosy1,
       aes(x = "", y = `y_i,1`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Indicador observado de terrorismo"
  ) +
  theme_bw()
#y2
ggplot(datosy2,
       aes(x = "", y = `y_i,2`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Indicador observado de conflicto"
  ) +
  theme_bw()
#y3
ggplot(datosy3,
       aes(x = "", y = `y_i,3`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Indicador observado de criminalidad"
  ) +
  theme_bw()
#y4
ggplot(datosy4,
       aes(x = "", y = `y_i,4`)) +
  geom_violin(fill = col[3], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Indicador observado de narcotráfico"
  ) +
  theme_bw()
######qqplot

ggplot(Y, aes(sample = Terrorismo )) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos de la normal estándar",
       y = "Cuantiles muestrales",
       title = "Indicador observado de terrorismo") +
  coord_fixed(ratio = 1) +
  theme_bw()

ggplot(Y, aes(sample = Conflicto )) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos de la normal estándar",
       y = "Cuantiles muestrales",
       title = "Indicador observado de conflicto") +
  coord_fixed(ratio = 1) +
  theme_bw()

ggplot(Y, aes(sample = Criminalidad )) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos de la normal estándar",
       y = "Cuantiles muestrales",
       title = "Indicador observado de criminalidad") +
  coord_fixed(ratio = 1) +
  theme_bw()

ggplot(Y, aes(sample = Narcotráfico )) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos de la normal estándar",
       y = "Cuantiles muestrales",
       title = "Indicador observado de narcotráfico") +
  coord_fixed(ratio = 1) +
  theme_bw()

######medidas descriptivas:
summary(datosy1)
summary(datosy2)
summary(datosy3)
summary(datosy4)
summary(datosx1)
summary(datosx2)
summary(datosx3)
summary(datosx4)

# Cargar los shapes para los mapas, indicando la ruta de las carpetas donde los
# tenga almacenados:
depto_shape <- st_read(
  "C:/Users/Álvaro García/OneDrive/Documentos/R/a58 Código Caso de estudio - EB - 2026-1/SHP_MGN2018_INTGRD_DEPTO/MGN_ANM_DPTOS.shp"
)
mcpio_shape <- st_read(
  "C:/Users/Álvaro García/OneDrive/Documentos/R/a58 Código Caso de estudio - EB - 2026-1/SHP_MGN2018_INTGRD_MPIO/MGN_ANM_MPIOS.shp"
)

# Revisión de los shapes:
names(depto_shape)
names(mcpio_shape)
dim(depto_shape)
head(depto_shape)
head(mcpio_shape)
st_geometry(depto_shape)
ggplot(depto_shape) +
  geom_sf()
ggplot(mcpio_shape) +
  geom_sf()

# Creación de los mapas:
map_mun <- left_join(mcpio_shape, data, by = "MPIO_CDPMP")
dep_data <- data |>
  group_by(DPTO_CCDGO) |>  
  summarise(x1 = mean(x1), x2 = mean(x2), x3 = mean(x3), x4 = mean(x4),
            y1 = median(y1), y2 = median(y2), y3 = median(y3), y4 = median(y4))
map_dep <- left_join(depto_shape, dep_data, by = "DPTO_CCDGO")

# Revisión de municipios faltantes:
map_mun[is.na(map_mun$y1), c(1:6)]

## Mapas de los puntajes estructurales a nivel municipal:
ggplot(map_mun) +
  geom_sf(aes(fill = x1), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(x[1]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                         barwidth = unit(1, "cm"))) +
  labs(title = "Contexto estructural") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_mun) +
  geom_sf(aes(fill = x2), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(x[2]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Contexto político") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_mun) +
  geom_sf(aes(fill = x3), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(x[3]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Contrabando") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_mun) +
  geom_sf(aes(fill = x4), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(x[4]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Sectores susceptibles") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))

## Mapas de los indicadores observados a nivel municipal:
ggplot(map_mun) +
  geom_sf(aes(fill = y1), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(y[1]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Terrorismo") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_mun) +
  geom_sf(aes(fill = y2), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(y[2]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Conflicto") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_mun) +
  geom_sf(aes(fill = y3), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(y[3]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Criminalidad") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_mun) +
  geom_sf(aes(fill = y4), color = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", name = expression(y[4]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Narcotráfico") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))

## Mapas de los puntajes estructurales a nivel departamental:
ggplot(map_dep) +
  geom_sf(aes(fill = x1), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(x[1]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Media de contexto estructural") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_dep) +
  geom_sf(aes(fill = x2), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(x[2]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Media de contexto político") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_dep) +
  geom_sf(aes(fill = x3), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(x[3]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Media de contrabando") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_dep) +
  geom_sf(aes(fill = x4), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(x[4]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Media de sectores susceptibles") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))

## Mapas de los indicadores observados a nivel departamental:
ggplot(map_dep) +
  geom_sf(aes(fill = y1), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(y[1]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Mediana de terrorismo") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_dep) +
  geom_sf(aes(fill = y2), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(y[2]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Mediana de conflicto") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_dep) +
  geom_sf(aes(fill = y3), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(y[3]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Mediana de criminalidad") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))
ggplot(map_dep) +
  geom_sf(aes(fill = y4), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(y[4]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Mediana de narcotráfico") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))

# Análisis general de los indicadores observados:
ggplot(data.frame(data), aes(sample = y1)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos de la normal estándar",
    y = "Cuantiles muestrales",
    title = "Indicador observado de terrorismo") +
  coord_fixed(ratio = 1) +
  theme_bw()
ggplot(data,
       aes(x = "", y = y1)) +
  geom_violin(fill = col[2], alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Indicador observado de terrorismo"
  ) +
  theme_bw()

#==============================================================================
# PARTE III: Implementación de los modelos Bayesianos mediante tres muestrea-
#            dores de Gibbs# Gráficos:
#==============================================================================

# AVISO: La ejecución de esta parte puede ser omitida si desea continuar con el
# diagnóstico de las muestras generadas por los tres muestreadores de Gibbs,
# pues, a continuación, se diligencia el código para la creación de los
# muestreadores y se ejecutan los algoritmos con la exportación automática de
# los resultados en la carpeta que fijó al comienzo de este script. Como el
# tiempo de ejecución de cada uno es considerable, puede pasar a la siguiente
# Parte y trabajar con los resultados que ya obtuvimos los autores (que aparecen
# en la carpeta 'Data' en GitHub), fechados en 10, 12 y 13 de julio para los
# muestreadores 1, 2 y 3 respectivamente. Dado que fijamos semillas en cada
# ejecución, obtendrá los mismos archivos .txt que nosotros. 

library(tictoc)
library(data.table)
library(MASS)
library(Matrix)
library(truncnorm)

# Configuración del algoritmo de M1:
B       = 110000 
param   = 1+4+1+4+3+4+4+4+1122     
n_burn  = 10000 
n_thin  = 10 
row    <- 1 #contador para las filas aceptadas (inicia en 1)

# Adaptación de Metropolis:
ac_tot      = 0 #contador para la función Adaptar (inicia en 0)
objetivo    = c(0.3, 0.5) #tasas de aceptación de entre .3 y .5
ventana     = 500 #iteraciones antes de adaptar el parámetro de ajuste
gamma_adapt = 0.05
delta2_mu     = c(.01, .003, .001, .01); adaptado_mu <- rep(FALSE, 4); ac_tot_mu <- rep(0, 4); ar_mu <- rep(0, 4)  #(inicial 0.01 para pruebas)
delta2_lambda = c(0, .003, .003, .01); adaptado_lambda <- rep(FALSE, 4); ac_tot_lambda <- rep(0, 4); ar_lambda <- rep(0,4)  #(inicial 0.01 para pruebas)
delta2_phi    = rep(.01, 4); adaptado_phi <- rep(FALSE, 4); ac_tot_phi <- rep(0, 4); ar_phi <- rep(0, 4)  #(inicial 0.01 para pruebas)
delta2_delta0 = c(1, .5, .5, .2); adaptado_delta0 <- rep(FALSE, 4); ac_tot_delta0 <- rep(0, 4); ar_delta0 <- rep(0, 4)
delta2_delta1 = c(1.2, .4, .5, .01); adaptado_delta1 <- rep(FALSE, 4); ac_tot_delta1 <- rep(0, 4); ar_delta1 <- rep(0, 4)
delta2_R      = rep(0.8, n); adaptado_R <- rep(FALSE, n); ac_tot_R <- rep(0, n); ar_R <- rep(0, n)

# Hiperparámetros de M1:
a_alpha  = 0
b_alpha  = 1
beta_0   = rep(0, 4)
B_0      = diag(1, 4)
a_sigma  = 2
b_sigma  = 1
a_mu     = 0
b_mu     = 1
a_lambda = 0
b_lambda = 1
a_phi    = 2
b_phi    = 1
a_delta0 = 0
b_delta0 = 4
b_delta1 = 1

# Funciones involucradas:
F1 <- function(y, R, mu, lambda){
  ind <- y>0
  sum(y[ind]*exp(-(mu+lambda*R[ind])))
}
F2 <- function(y, R){
  ind <- y>0
  sum(R[ind])
}
F2_0 <- function(y, R){
  ind <- y==0
  sum(R[ind])
}
F3 <- function(y){
  ind <- y>0
  sum(log(y[ind]))
}
F4 <- function(y, R, phi, mu, lambda){
  ind <- y>0
  sum((phi[ind]*y[ind])/exp(mu[ind]+lambda[ind]*R))
}
F5 <- function(y, R, phi, lambda){
  ind <- y>0
  R*sum(phi[ind]*lambda[ind])
}
F6 <- function(y, delta_1){
  ind <- y==0
  sum(delta_1[ind])
}
logpost_mu <- function(theta, a_mu, b_mu, y, R, n_plus, phi, lambda){
  -phi*n_plus*theta-(theta-a_mu)^2/(2*b_mu)-phi*F1(y, R, theta, lambda)
}
logpost_lambda <- function(theta, a_lambda, b_lambda, y, R, mu, phi){
  -((theta-a_lambda)^2)/(2*b_lambda)-phi*theta*F2(y, R)-phi*F1(y, R, mu, theta)
}
logpost_phi <- function(theta, a_phi, b_phi, y, R, n_plus, mu, lambda, F3){
  (a_phi+n_plus*theta-1)*log(theta)-n_plus*lgamma(theta)+theta*F3-
    (b_phi+n_plus*mu+lambda*F2(y, R)+F1(y, R, mu, lambda))*theta
}
logpost_delta0 <- function(theta, a_delta0, b_delta0, y, R, n_0, delta_1){
  n_0*theta-((theta-a_delta0)^2)/(2*b_delta0)-delta_1*F2_0(y, R)-
    sum(log1p(exp(theta-delta_1*R)))
}
logpost_delta1 <- function(theta, b_delta1, y, R, delta_0){
  -theta^2/(2*b_delta1)-theta*F2_0(y, R)-sum(log1p(exp(delta_0-theta*R)))
}
logpost_R <- function(theta, eta, y, sigma2, mu, phi, lambda, delta_0, delta_1){
  (2*theta*eta-theta^2)/(2*sigma2)-F4(y, theta, phi, mu, lambda)-
    F5(y, theta, phi, lambda)-theta*F6(y, delta_1)-sum(log1p(exp(delta_0-delta_1*theta)))
}
ll_ZIGamma <- function(Y, R, mu, lambda, phi, delta0, delta1){
  ll <- 0
  for(j in 1:4){
    eta <- delta0[j]-delta1[j]*R
    muij <- exp(mu[j]+lambda[j]*R)
    ind0 <- Y[ ,j]==0
    ind1 <- !ind0
    ## Parte Bernoulli:
    ll <- ll+sum(eta[ind0]-log1p(exp(eta[ind0])))
    ll <- ll-sum(log1p(exp(eta[ind1])))
    ## Parte Gamma:
    ll <- ll+sum(dgamma(Y[ind1,j], phi[j], rate = phi[j]/muij[ind1], log = TRUE))
  }
  ll
}
Adaptar <- function(delta2, ac_tot, iter, objetivo, gamma_adapt){
  ## Tasa de aceptación acumulada:
  tasa <- ac_tot/iter
  ## Adaptar únicamente si aún no está dentro del intervalo objetivo:
  if(tasa < objetivo[1])
    delta2 <- delta2*exp(-gamma_adapt)
  if(tasa > objetivo[2])
    delta2 <- delta2*exp(gamma_adapt)
  ## Devolver resultados:
  return(list(delta2 = delta2, ac_rate = tasa))
}
Metropolis <- function(theta, delta2, log_kernel, ...){
  ## Paso 1: Propuesta con una normal:
  theta_star <- rnorm(1, theta, sqrt(delta2))
  ## Paso 2: Evaluar el log-kernel:
  log_post_star <- log_kernel(theta_star, ...)
  log_post_curr <- log_kernel(theta, ...)
  ## Paso 3: Probabilidad de aceptación (en escala log):
  log_r <- log_post_star-log_post_curr
  ## Paso 4: Aceptar o rechazar la propuesta:
  ac <- 0
  if(log(runif(1))<log_r){
    theta <- theta_star
    ac <- 1
  }
  ## Paso 5: Devolver resultados:
  return(list(theta = theta, ac = ac))
}
Metropolis_Hastings <- function(theta, delta2, log_kernel, ...){
  ## Paso 1: Propuesta con una lognormal:
  theta_star <- rlnorm(1, meanlog = log(theta), sdlog = sqrt(delta2))
  ## Paso 2: Evaluar el log-kernel:
  log_post_star <- log_kernel(theta_star, ...)
  log_post_curr <- log_kernel(theta, ...)
  ## Paso 3: Probabilidad de aceptación (en escala log):
  log_r <- (log_post_star-log_post_curr)+log(theta_star/theta)
  ## Paso 4: Aceptar o rechazar la propuesta:
  ac <- 0
  if(log(runif(1))<log_r){
    theta <- theta_star
    ac <- 1
  }
  ## Paso 5: Devolver resultados:
  return(list(theta = theta, ac = ac))
}

# Cálculos invariantes respecto a las iteraciones:
B_0_inv       = solve(B_0) #inversa de la matriz de varianzas y covarianzas de beta
B_0_inv.beta0 = B_0_inv%*%beta_0
Gram          = t(X)%*%X #matriz de Gram de las columnas de la matriz de covariables estructurales
F3_j          = apply(Y, 2, F3) #vector con la suma de los ln(y_i,j) para y_i,j>0

# Muestreador de Gibbs para M1:
samples <- matrix(NA, nrow = (B-n_burn)/n_thin, ncol = param+1) #matriz de almacenamiento
MCMC_1 <- function(B, Y, n, X, a_alpha, b_alpha, beta_0, B_0, a_sigma, b_sigma,
                   a_mu, b_mu, a_lambda, b_lambda, a_phi, b_phi, a_delta0,
                   b_delta0, b_delta1) {
  
  # Valores iniciales (simulados de las distribuciones previas):
  alpha         <- rnorm(1, a_alpha, sqrt(b_alpha))
  beta          <- mvrnorm(1, beta_0, B_0)
  sigma2        <- 1/rgamma(1, a_sigma, rate = b_sigma)
  R_i           <- rnorm(n, alpha+X%*%beta, sqrt(sigma2))
  mu_j          <- rnorm(4, a_mu, sqrt(b_mu))
  lambda_j[1]    = 1
  lambda_j[2:4] <- rnorm(3, a_lambda, sqrt(b_lambda))
  phi_j         <- rgamma(4, a_phi, rate = b_phi)
  delta_0j      <- rnorm(4, a_delta0, sqrt(b_delta0))
  delta_1j      <- rtruncnorm(4, 0, Inf, mean = 0, sd = sqrt(b_delta1))
  
  # Cadena MCMC:
  for (b in 1:B) {
    ## Actualizar alpha:
    v_alpha <- 1/(1/b_alpha+n/sigma2)
    m_alpha <- v_alpha*(sum(R_i-X%*%beta)/sigma2+a_alpha/b_alpha)
    alpha   <- rnorm(1, m_alpha, sqrt(v_alpha))
    
    ## Actualizar beta:
    sigma_beta <- solve(B_0_inv+(1/sigma2)*Gram)
    mu_beta    <- sigma_beta%*%(B_0_inv.beta0+(1/sigma2)*t(X)%*%(R_i-alpha))
    beta       <- mvrnorm(1, mu_beta, sigma_beta)
    
    ## Actualizar sigma^2:
    shape_sigma2 <- a_sigma+n/2
    scale_sigma2 <- b_sigma+0.5*sum((R_i-(alpha+X%*%beta))^2)
    sigma2       <- 1/rgamma(1, shape_sigma2, rate = scale_sigma2)
    
    ## Actualizar los mu_j (Algoritmo de Metropolis adaptativo):
    for (j in 1:4) {
      aux <- Metropolis(mu_j[j], delta2_mu[j], logpost_mu, a_mu, b_mu, Y[ ,j],
                        R_i, nj_plus[j], phi_j[j], lambda_j[j])
      mu_j[j] <- aux$theta
      ac_tot_mu[j] <- ac_tot_mu[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_mu[j], ac_tot_mu[j], b, objetivo, gamma_adapt)
        delta2_mu[j] <- aux2$delta2
      }
    }; ar_mu <- ac_tot_mu/B
    
    ## Actualizar los lambda_j (Algoritmo de Metropolis adaptativo):
    for (j in 2:4) {
      aux <- Metropolis(lambda_j[j], delta2_lambda[j], logpost_lambda,
                        a_lambda, b_lambda, Y[ ,j], R_i, mu_j[j], phi_j[j])
      lambda_j[j] <- aux$theta
      ac_tot_lambda[j] <- ac_tot_lambda[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_lambda[j], ac_tot_lambda[j], b, objetivo, gamma_adapt)
        delta2_lambda[j] <- aux2$delta2
      }
    }; ar_lambda <- ac_tot_lambda/B
    
    ## Actualizar los phi_j (Algoritmo de Metropolis-Hastings adaptativo):
    for (j in 1:4) {
      aux <- Metropolis_Hastings(phi_j[j], delta2_phi[j], logpost_phi, a_phi,
                                 b_phi, Y[ ,j], R_i, nj_plus[j], mu_j[j],
                                 lambda_j[j], F3_j[j])
      phi_j[j] <- aux$theta
      ac_tot_phi[j] <- ac_tot_phi[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_phi[j], ac_tot_phi[j], b, objetivo, gamma_adapt)
        delta2_phi[j] <- aux2$delta2
      }
    }; ar_phi <- ac_tot_phi/B
    
    ## Actualizar los delta_0j (Algoritmo de Metropolis adaptativo):
    for (j in 1:4) {
      aux <- Metropolis(delta_0j[j], delta2_delta0[j], logpost_delta0, a_delta0,
                        b_delta0, Y[ ,j], R_i, nj_0[j], delta_1j[j])
      delta_0j[j] <- aux$theta
      ac_tot_delta0[j] <- ac_tot_delta0[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_delta0[j], ac_tot_delta0[j], b, objetivo, gamma_adapt)
        delta2_delta0[j] <- aux2$delta2
      }
    }; ar_delta0 <- ac_tot_delta0/B
    
    ## Actualizar los delta_1j (Algoritmo de Metropolis-Hastings adaptativo):
    for (j in 1:4) {
      aux <- Metropolis_Hastings(delta_1j[j], delta2_delta1[j], logpost_delta1,
                                 b_delta1, Y[ ,j], R_i, delta_0j[j])
      delta_1j[j] <- aux$theta
      ac_tot_delta1[j] <- ac_tot_delta1[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_delta1[j], ac_tot_delta1[j], b, objetivo, gamma_adapt)
        delta2_delta1[j] <- aux2$delta2
      }
    }; ar_delta1 <- ac_tot_delta1/B
    
    ## Actualizar los R_i (Algoritmo de Metropolis adaptativo):
    for (i in 1:n) {
      eta_i <- alpha+drop(crossprod(X[i, ], beta))
      aux <- Metropolis(R_i[i], delta2_R[i], logpost_R, eta_i, Y[i, ], sigma2,
                        mu_j, phi_j, lambda_j, delta_0j, delta_1j)
      R_i[i] <- aux$theta
      ac_tot_R[i] <- ac_tot_R[i]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_R[i], ac_tot_R[i], b, objetivo, gamma_adapt)
        delta2_R[i] <- aux2$delta2
      }
    }; ar_R <- ac_tot_R/B
    
    # Recentrado del factor latente:
    c <- mean(R_i)
    R_i <- R_i-c
    alpha <- alpha-c
    mu_j <- mu_j+lambda_j*c
    delta_0j <- delta_0j-delta_1j*c
    
    # Log-verosimilitud:
    ll <- ll_ZIGamma(Y, R_i, mu_j, lambda_j, phi_j, delta_0j, delta_1j)
    
    # Almacenar resultados después de burn-in y thinning:
    if (b>n_burn && b%%n_thin==1) {
      samples[row, ] <- c(alpha, beta, sigma2, mu_j, lambda_j[2:4], phi_j,
                          delta_0j, delta_1j, R_i, ll)
      row <- row+1
    }
    
    # Progreso:
    ncat <- floor(B/10)
    if (b%%ncat==0) {
      cat(100*round(b/B, 1), "% completado... \n", sep = "")
    }
  }
  
  # Salida final:
  colnames(samples) <- c("alpha", paste0("beta[", 1:4, "]"), "sigma2",
                         paste0("mu[", 1:4, "]"), paste0("lambda[", 2:4, "]"),
                         paste0("phi[", 1:4, "]"), paste0("delta0[", 1:4, "]"),
                         paste0("delta1[", 1:4, "]"), paste0("R[", 1:n, "]"), "ll")
  samples <- as.data.frame(samples)
  nombre_archivo <- paste0("Chain 1 (Arévalo-García) ", format(Sys.time(), "%d.%m.%Y %H.%M"), ".txt")
  write.table(samples, file = nombre_archivo, sep = ";", dec = ".",
              row.names = FALSE, quote = TRUE)
  message("Archivo exportado como: ", normalizePath(nombre_archivo))
  message("Tasa de aceptación de las simulaciones de mu: ",
          paste(round(ar_mu, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de lambda: ",
          paste(round(ar_lambda[2:4], 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de phi: ",
          paste(round(ar_phi, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de delta0: ",
          paste(round(ar_delta0, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de delta1: ",
          paste(round(ar_delta1, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de R: mínima = ",
          round(min(ar_R), 3), ", media = ", round(mean(ar_R), 3), ", máxima = ",
          round(max(ar_R), 3))
}

# Ajuste del Modelo 1:
set.seed(2706); tic(); MCMC_1(B, Y, n, X, a_alpha, b_alpha, beta_0, B_0, a_sigma,
                              b_sigma, a_mu, b_mu, a_lambda, b_lambda, a_phi,
                              b_phi, a_delta0, b_delta0, b_delta1); toc(); rm(samples)

# Configuración del algoritmo de M2:
B       = 110000 
param   = 1+4+1+4+3+4+4+4+1122+33+1     
n_burn  = 10000 
n_thin  = 10 
row    <- 1 #contador para las filas aceptadas (inicia en 1)

# Adaptación de Metropolis:
ac_tot      = 0 #contador para la función Adaptar (inicia en 0)
objetivo    = c(0.3, 0.5) #tasas de aceptación de entre .3 y .5
ventana     = 500 #iteraciones antes de adaptar el parámetro de ajuste
gamma_adapt = 0.05
delta2_mu     = c(.01, .003, .001, .01); adaptado_mu <- rep(FALSE, 4); ac_tot_mu <- rep(0, 4); ar_mu <- rep(0, 4)  #(inicial 0.01 para pruebas)
delta2_lambda = c(0, .003, .003, .01); adaptado_lambda <- rep(FALSE, 4); ac_tot_lambda <- rep(0, 4); ar_lambda <- rep(0,4)  #(inicial 0.01 para pruebas)
delta2_phi    = rep(.01, 4); adaptado_phi <- rep(FALSE, 4); ac_tot_phi <- rep(0, 4); ar_phi <- rep(0, 4)  #(inicial 0.01 para pruebas)
delta2_delta0 = c(1, .5, .5, .2); adaptado_delta0 <- rep(FALSE, 4); ac_tot_delta0 <- rep(0, 4); ar_delta0 <- rep(0, 4)
delta2_delta1 = c(1.2, .4, .5, .01); adaptado_delta1 <- rep(FALSE, 4); ac_tot_delta1 <- rep(0, 4); ar_delta1 <- rep(0, 4)
delta2_R      = rep(0.8, n); adaptado_R <- rep(FALSE, n); ac_tot_R <- rep(0, n); ar_R <- rep(0, n)

# Hiperparámetros de M2:
a_alpha  = 0
b_alpha  = 1
beta_0   = rep(0, 4)
B_0      = diag(1, 4)
a_sigma  = 2
b_sigma  = 1
a_omega  = 2
b_omega  = 1
a_mu     = 0
b_mu     = 1
a_lambda = 0
b_lambda = 1
a_phi    = 2
b_phi    = 1
a_delta0 = 0
b_delta0 = 4
b_delta1 = 1

# Funciones involucradas:
ll_ZIGamma <- function(Y, R, mu, lambda, phi, delta0, delta1){
  ll_B <- 0
  ll_G <- 0
  for(j in 1:4){
    eta  <- delta0[j]-delta1[j]*R
    muij <- exp(mu[j]+lambda[j]*R)
    ind0 <- Y[,j]==0
    ind1 <- !ind0
    ## Bernoulli:
    ll_B <- ll_B+sum(eta[ind0]-log1p(exp(eta[ind0])))
    ll_B <- ll_B-sum(log1p(exp(eta[ind1])))
    ## Gamma:
    ll_G <- ll_G+sum(dgamma(Y[ind1,j], phi[j], rate = phi[j]/muij[ind1], log = TRUE))
  }
  list(total = ll_B+ll_G, bern  = ll_B, gamma = ll_G)
}

# Muestreador de Gibbs para M2:
samples <- matrix(NA, nrow = (B-n_burn)/n_thin, ncol = param+3) #matriz de almacenamiento
MCMC_2 <- function(B, Y, n, X, a_alpha, b_alpha, beta_0, B_0, a_sigma, b_sigma,
                   a_omega, b_omega, a_mu, b_mu, a_lambda, b_lambda, a_phi,
                   b_phi, a_delta0, b_delta0, b_delta1) {
  
  # Valores iniciales (simulados de las distribuciones previas):
  alpha         <- rnorm(1, a_alpha, sqrt(b_alpha))
  beta          <- mvrnorm(1, beta_0, B_0)
  sigma2        <- 1/rgamma(1, a_sigma, rate = b_sigma)
  omega2        <- 1/rgamma(1, a_omega, rate = b_omega)
  u_d           <- rnorm(33, 0, sqrt(omega2))
  u_xi_i        <- u_d[index]
  R_i           <- rnorm(n, alpha+X%*%beta+u_d[index], sqrt(sigma2))
  mu_j          <- rnorm(4, a_mu, sqrt(b_mu))
  lambda_j[1]    = 1
  lambda_j[2:4] <- rnorm(3, a_lambda, sqrt(b_lambda))
  phi_j         <- rgamma(4, a_phi, rate = b_phi)
  delta_0j      <- rnorm(4, a_delta0, sqrt(b_delta0))
  delta_1j      <- rtruncnorm(4, 0, Inf, mean = 0, sd = sqrt(b_delta1))
  
  # Cadena MCMC:
  for (b in 1:B) {
    ## Actualizar alpha:
    xb      <- drop(X%*%beta)
    v_alpha <- 1/(1/b_alpha+n/sigma2)
    m_alpha <- v_alpha*(sum(R_i-xb-u_xi_i)/sigma2+a_alpha/b_alpha)
    alpha   <- rnorm(1, m_alpha, sqrt(v_alpha))
    
    ## Actualizar beta:
    sigma_beta <- solve(B_0_inv+(1/sigma2)*Gram)
    mu_beta    <- sigma_beta%*%(B_0_inv.beta0+(1/sigma2)*t(X)%*%(R_i-alpha-u_xi_i))
    beta       <- mvrnorm(1, mu_beta, sigma_beta)
    xb         <- drop(X%*%beta)
    e_i        <- R_i-alpha-xb
    
    ## Actualizar omega^2:
    shape_omega2 <- a_omega+33/2
    scale_omega2 <- b_omega+0.5*sum(u_d^2)
    omega2       <- 1/rgamma(1, shape_omega2, rate = scale_omega2)
    
    ## Actualizar los u_d:
    for(d in 1:33){
      idx <- mun_dep[[d]]
      n_d <- length(idx)
      suma_e <- sum(e_i[idx])
      v_u    <- 1/(n_d/sigma2+1/omega2)
      m_u    <- v_u*suma_e/sigma2
      u_d[d] <- rnorm(1, m_u, sqrt(v_u))
    }
    ### Vector de efectos departamentales:
    u_xi_i <- u_d[index]
    
    ## Actualizar sigma^2:
    shape_sigma2 <- a_sigma+n/2
    scale_sigma2 <- b_sigma+0.5*sum((e_i-u_xi_i)^2)
    sigma2       <- 1/rgamma(1, shape_sigma2, rate = scale_sigma2)
    
    ## Actualizar los mu_j (Algoritmo de Metropolis adaptativo):
    for (j in 1:4) {
      aux <- Metropolis(mu_j[j], delta2_mu[j], logpost_mu, a_mu, b_mu, Y[ ,j],
                        R_i, nj_plus[j], phi_j[j], lambda_j[j])
      mu_j[j] <- aux$theta
      ac_tot_mu[j] <- ac_tot_mu[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_mu[j], ac_tot_mu[j], b, objetivo, gamma_adapt)
        delta2_mu[j] <- aux2$delta2
      }
    }; ar_mu <- ac_tot_mu/B
    
    ## Actualizar los lambda_j (Algoritmo de Metropolis adaptativo):
    for (j in 2:4) {
      aux <- Metropolis(lambda_j[j], delta2_lambda[j], logpost_lambda,
                        a_lambda, b_lambda, Y[ ,j], R_i, mu_j[j], phi_j[j])
      lambda_j[j] <- aux$theta
      ac_tot_lambda[j] <- ac_tot_lambda[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_lambda[j], ac_tot_lambda[j], b, objetivo, gamma_adapt)
        delta2_lambda[j] <- aux2$delta2
      }
    }; ar_lambda <- ac_tot_lambda/B
    
    ## Actualizar los phi_j (Algoritmo de Metropolis-Hastings adaptativo):
    for (j in 1:4) {
      aux <- Metropolis_Hastings(phi_j[j], delta2_phi[j], logpost_phi, a_phi,
                                 b_phi, Y[ ,j], R_i, nj_plus[j], mu_j[j],
                                 lambda_j[j], F3_j[j])
      phi_j[j] <- aux$theta
      ac_tot_phi[j] <- ac_tot_phi[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_phi[j], ac_tot_phi[j], b, objetivo, gamma_adapt)
        delta2_phi[j] <- aux2$delta2
      }
    }; ar_phi <- ac_tot_phi/B
    
    ## Actualizar los delta_0j (Algoritmo de Metropolis adaptativo):
    for (j in 1:4) {
      aux <- Metropolis(delta_0j[j], delta2_delta0[j], logpost_delta0, a_delta0,
                        b_delta0, Y[ ,j], R_i, nj_0[j], delta_1j[j])
      delta_0j[j] <- aux$theta
      ac_tot_delta0[j] <- ac_tot_delta0[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_delta0[j], ac_tot_delta0[j], b, objetivo, gamma_adapt)
        delta2_delta0[j] <- aux2$delta2
      }
    }; ar_delta0 <- ac_tot_delta0/B
    
    ## Actualizar los delta_1j (Algoritmo de Metropolis-Hastings adaptativo):
    for (j in 1:4) {
      aux <- Metropolis_Hastings(delta_1j[j], delta2_delta1[j], logpost_delta1,
                                 b_delta1, Y[ ,j], R_i, delta_0j[j])
      delta_1j[j] <- aux$theta
      ac_tot_delta1[j] <- ac_tot_delta1[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_delta1[j], ac_tot_delta1[j], b, objetivo, gamma_adapt)
        delta2_delta1[j] <- aux2$delta2
      }
    }; ar_delta1 <- ac_tot_delta1/B
    
    ## Actualizar los R_i (Algoritmo de Metropolis adaptativo):
    for (i in 1:n) {
      eta_i <- alpha+drop(crossprod(X[i, ], beta))+u_xi_i[i]
      aux <- Metropolis(R_i[i], delta2_R[i], logpost_R, eta_i, Y[i, ], sigma2,
                        mu_j, phi_j, lambda_j, delta_0j, delta_1j)
      R_i[i] <- aux$theta
      ac_tot_R[i] <- ac_tot_R[i]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_R[i], ac_tot_R[i], b, objetivo, gamma_adapt)
        delta2_R[i] <- aux2$delta2
      }
    }; ar_R <- ac_tot_R/B
    
    # Recentrado de los efectos departamentales:
    c_u   <- mean(u_d)
    u_d   <- u_d-c_u
    alpha <- alpha+c_u
    
    # Recentrado de los factores latentes:
    c_R      <- mean(R_i)
    R_i      <- R_i-c_R
    alpha    <- alpha-c_R
    mu_j     <- mu_j+lambda_j*c_R
    delta_0j <- delta_0j-delta_1j*c_R
    
    # Log-verosimilitud:
    aux_ll <- ll_ZIGamma(Y, R_i, mu_j, lambda_j,
                         phi_j, delta_0j, delta_1j)
    ll_total[b] <- aux_ll$total
    ll_bern[b]  <- aux_ll$bern
    ll_gamma[b] <- aux_ll$gamma
    
    # Almacenar resultados después de burn-in y thinning:
    if (b>n_burn && b%%n_thin==1) {
      samples[row, ] <- c(alpha, beta, omega2, u_d, sigma2, mu_j, lambda_j[2:4],
                          phi_j, delta_0j, delta_1j, R_i, ll_bern[b], ll_gamma[b],
                          ll_total[b])
      row <- row+1
    }
    
    # Progreso:
    ncat <- floor(B/10)
    if (b%%ncat==0) {
      cat(100*round(b/B, 1), "% completado... \n", sep = "")
    }
  }
  
  # Salida final:
  colnames(samples) <- c("alpha", paste0("beta[", 1:4, "]"), "omega2",
                         paste0("u[", 1:33, "]"), "sigma2", paste0("mu[", 1:4, "]"),
                         paste0("lambda[", 2:4, "]"), paste0("phi[", 1:4, "]"),
                         paste0("delta0[", 1:4, "]"), paste0("delta1[", 1:4, "]"),
                         paste0("R[", 1:n, "]"), "ll_bern", "ll_gamma", "ll")
  samples <- as.data.frame(samples)
  nombre_archivo <- paste0("Chain 2 (Arévalo-García) ", format(Sys.time(), "%d.%m.%Y %H.%M"), ".txt")
  write.table(samples, file = nombre_archivo, sep = ";", dec = ".",
              row.names = FALSE, quote = TRUE)
  message("Archivo exportado como: ", normalizePath(nombre_archivo))
  message("Tasa de aceptación de las simulaciones de mu: ",
          paste(round(ar_mu, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de lambda: ",
          paste(round(ar_lambda[2:4], 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de phi: ",
          paste(round(ar_phi, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de delta0: ",
          paste(round(ar_delta0, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de delta1: ",
          paste(round(ar_delta1, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de R: mínima = ",
          round(min(ar_R), 3), ", media = ", round(mean(ar_R), 3), ", máxima = ",
          round(max(ar_R), 3))
}

# Ajuste del Modelo 2:
set.seed(1007); tic(); MCMC_2(B, Y, n, X, a_alpha, b_alpha, beta_0, B_0, a_sigma, b_sigma,
                              a_omega, b_omega, a_mu, b_mu, a_lambda, b_lambda, a_phi,
                              b_phi, a_delta0, b_delta0, b_delta1); toc(); rm(samples)

# Configuración del algoritmo de M3:
B       = 110000 
param   = 1+4+1+4+3+4+4+4+1122+33+1     
n_burn  = 10000 
n_thin  = 10 
row    <- 1 #contador para las filas aceptadas (inicia en 1)

# Adaptación de Metropolis:
ac_tot      = 0 #contador para la función Adaptar (inicia en 0)
objetivo    = c(0.3, 0.5) #tasas de aceptación de entre .3 y .5
ventana     = 500 #iteraciones antes de adaptar el parámetro de ajuste
gamma_adapt = 0.05
delta2_mu     = c(.01, .003, .001, .01); adaptado_mu <- rep(FALSE, 4); ac_tot_mu <- rep(0, 4); ar_mu <- rep(0, 4)  #(inicial 0.01 para pruebas)
delta2_lambda = c(0, .003, .003, .01); adaptado_lambda <- rep(FALSE, 4); ac_tot_lambda <- rep(0, 4); ar_lambda <- rep(0,4)  #(inicial 0.01 para pruebas)
delta2_phi    = rep(.01, 4); adaptado_phi <- rep(FALSE, 4); ac_tot_phi <- rep(0, 4); ar_phi <- rep(0, 4)  #(inicial 0.01 para pruebas)
delta2_delta0 = c(1, .5, .5, .2); adaptado_delta0 <- rep(FALSE, 4); ac_tot_delta0 <- rep(0, 4); ar_delta0 <- rep(0, 4)
delta2_delta1 = c(1.2, .4, .5, .01); adaptado_delta1 <- rep(FALSE, 4); ac_tot_delta1 <- rep(0, 4); ar_delta1 <- rep(0, 4)
delta2_R      = rep(0.8, n); adaptado_R <- rep(FALSE, n); ac_tot_R <- rep(0, n); ar_R <- rep(0, n)

# Hiperparámetros de M3:
a_alpha  = 0
b_alpha  = 1
beta_0   = rep(0, 4)
B_0      = diag(1, 4)
a_sigma  = 2
b_sigma  = 1
a_omega  = 2
b_omega  = 1
a_mu     = 0
b_mu     = 1
a_lambda = 0
b_lambda = 1
a_phi    = 2
b_phi    = 1
a_delta0 = 0
b_delta0 = 4
b_delta1 = 1

# Matriz de vecindad:
W <- matrix(c(0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0,
              0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1,
              0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,
              0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 
              0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
              0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1,
              0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0),
            nrow = 33, byrow = TRUE)
isSymmetric(W)

# Matriz de precisión:
D <- diag(rowSums(W))
Q <- D-W

# Muestreador de Gibbs para M3:
samples <- matrix(NA, nrow = (B-n_burn)/n_thin, ncol = param+3) #matriz de almacenamiento
MCMC_3 <- function(B, Y, n, X, a_alpha, b_alpha, beta_0, B_0, a_sigma, b_sigma,
                   a_omega, b_omega, a_mu, b_mu, a_lambda, b_lambda, a_phi,
                   b_phi, a_delta0, b_delta0, b_delta1) {
  
  # Valores iniciales (simulados de las distribuciones previas):
  alpha         <- rnorm(1, a_alpha, sqrt(b_alpha))
  beta          <- mvrnorm(1, beta_0, B_0)
  sigma2        <- 1/rgamma(1, a_sigma, rate = b_sigma)
  omega2        <- 1/rgamma(1, a_omega, rate = b_omega)
  psi_d         <- rep(0, 33) #los efectos departamentales espaciales inician en 0
  psi_xi_i      <- psi_d[index]
  R_i           <- rnorm(n, alpha+X%*%beta+psi_xi_i, sqrt(sigma2))
  mu_j          <- rnorm(4, a_mu, sqrt(b_mu))
  lambda_j[1]    = 1
  lambda_j[2:4] <- rnorm(3, a_lambda, sqrt(b_lambda))
  phi_j         <- rgamma(4, a_phi, rate = b_phi)
  delta_0j      <- rnorm(4, a_delta0, sqrt(b_delta0))
  delta_1j      <- rtruncnorm(4, 0, Inf, mean = 0, sd = sqrt(b_delta1))
  
  # Cadena MCMC:
  for (b in 1:B) {
    ## Actualizar alpha:
    xb      <- drop(X%*%beta)
    v_alpha <- 1/(1/b_alpha+n/sigma2)
    m_alpha <- v_alpha*(sum(R_i-xb-psi_xi_i)/sigma2+a_alpha/b_alpha)
    alpha   <- rnorm(1, m_alpha, sqrt(v_alpha))
    
    ## Actualizar beta:
    sigma_beta <- solve(B_0_inv+(1/sigma2)*Gram)
    mu_beta    <- sigma_beta%*%(B_0_inv.beta0+(1/sigma2)*t(X)%*%(R_i-alpha-psi_xi_i))
    beta       <- mvrnorm(1, mu_beta, sigma_beta)
    xb         <- drop(X%*%beta)
    e_i        <- R_i-alpha-xb
    
    ## Actualizar omega^2:
    shape_omega2 <- a_omega+16
    scale_omega2 <- b_omega+0.5*sum(psi_d*(Q%*%psi_d))
    omega2       <- 1/rgamma(1, shape_omega2, rate = scale_omega2)
    
    ## Actualizar los psi_d:
    for(d in 1:33){
      idx <- mun_dep[[d]]
      n_d <- length(idx)
      suma_e <- sum(e_i[idx])
      v_psi  <- 1/(n_d/sigma2+D[d,d]/omega2)
      m_psi  <- v_psi*((1/sigma2)*suma_e+(1/omega2)*sum(W[d, ]*psi_d))
      psi_d[d] <- rnorm(1, m_psi, sqrt(v_psi))
    }
    ### Vector de efectos departamentales espaciales:
    psi_xi_i <- psi_d[index]
    
    ## Actualizar sigma^2:
    shape_sigma2 <- a_sigma+n/2
    scale_sigma2 <- b_sigma+0.5*sum((e_i-psi_xi_i)^2)
    sigma2       <- 1/rgamma(1, shape_sigma2, rate = scale_sigma2)
    
    ## Actualizar los mu_j (Algoritmo de Metropolis adaptativo):
    for (j in 1:4) {
      aux <- Metropolis(mu_j[j], delta2_mu[j], logpost_mu, a_mu, b_mu, Y[ ,j],
                        R_i, nj_plus[j], phi_j[j], lambda_j[j])
      mu_j[j] <- aux$theta
      ac_tot_mu[j] <- ac_tot_mu[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_mu[j], ac_tot_mu[j], b, objetivo, gamma_adapt)
        delta2_mu[j] <- aux2$delta2
      }
    }; ar_mu <- ac_tot_mu/B
    
    ## Actualizar los lambda_j (Algoritmo de Metropolis adaptativo):
    for (j in 2:4) {
      aux <- Metropolis(lambda_j[j], delta2_lambda[j], logpost_lambda,
                        a_lambda, b_lambda, Y[ ,j], R_i, mu_j[j], phi_j[j])
      lambda_j[j] <- aux$theta
      ac_tot_lambda[j] <- ac_tot_lambda[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_lambda[j], ac_tot_lambda[j], b, objetivo, gamma_adapt)
        delta2_lambda[j] <- aux2$delta2
      }
    }; ar_lambda <- ac_tot_lambda/B
    
    ## Actualizar los phi_j (Algoritmo de Metropolis-Hastings adaptativo):
    for (j in 1:4) {
      aux <- Metropolis_Hastings(phi_j[j], delta2_phi[j], logpost_phi, a_phi,
                                 b_phi, Y[ ,j], R_i, nj_plus[j], mu_j[j],
                                 lambda_j[j], F3_j[j])
      phi_j[j] <- aux$theta
      ac_tot_phi[j] <- ac_tot_phi[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_phi[j], ac_tot_phi[j], b, objetivo, gamma_adapt)
        delta2_phi[j] <- aux2$delta2
      }
    }; ar_phi <- ac_tot_phi/B
    
    ## Actualizar los delta_0j (Algoritmo de Metropolis adaptativo):
    for (j in 1:4) {
      aux <- Metropolis(delta_0j[j], delta2_delta0[j], logpost_delta0, a_delta0,
                        b_delta0, Y[ ,j], R_i, nj_0[j], delta_1j[j])
      delta_0j[j] <- aux$theta
      ac_tot_delta0[j] <- ac_tot_delta0[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_delta0[j], ac_tot_delta0[j], b, objetivo, gamma_adapt)
        delta2_delta0[j] <- aux2$delta2
      }
    }; ar_delta0 <- ac_tot_delta0/B
    
    ## Actualizar los delta_1j (Algoritmo de Metropolis-Hastings adaptativo):
    for (j in 1:4) {
      aux <- Metropolis_Hastings(delta_1j[j], delta2_delta1[j], logpost_delta1,
                                 b_delta1, Y[ ,j], R_i, delta_0j[j])
      delta_1j[j] <- aux$theta
      ac_tot_delta1[j] <- ac_tot_delta1[j]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_delta1[j], ac_tot_delta1[j], b, objetivo, gamma_adapt)
        delta2_delta1[j] <- aux2$delta2
      }
    }; ar_delta1 <- ac_tot_delta1/B
    
    ## Actualizar los R_i (Algoritmo de Metropolis adaptativo):
    for (i in 1:n) {
      eta_i <- alpha+drop(crossprod(X[i, ], beta))+psi_xi_i[i]
      aux <- Metropolis(R_i[i], delta2_R[i], logpost_R, eta_i, Y[i, ], sigma2,
                        mu_j, phi_j, lambda_j, delta_0j, delta_1j)
      R_i[i] <- aux$theta
      ac_tot_R[i] <- ac_tot_R[i]+aux$ac
      ### Adaptar la propuesta de delta^2:
      if(b<=n_burn && b%%ventana==0){
        aux2 <- Adaptar(delta2_R[i], ac_tot_R[i], b, objetivo, gamma_adapt)
        delta2_R[i] <- aux2$delta2
      }
    }; ar_R <- ac_tot_R/B
    
    # Recentrado de los efectos departamentales espaciales:
    c_psi <- mean(psi_d)
    psi_d <- psi_d-c_psi
    alpha <- alpha+c_psi
    
    # Recentrado de los factores latentes:
    c_R      <- mean(R_i)
    R_i      <- R_i-c_R
    alpha    <- alpha-c_R
    mu_j     <- mu_j+lambda_j*c_R
    delta_0j <- delta_0j-delta_1j*c_R
    
    # Log-verosimilitud:
    aux_ll <- ll_ZIGamma(Y, R_i, mu_j, lambda_j,
                         phi_j, delta_0j, delta_1j)
    ll_total[b] <- aux_ll$total
    ll_bern[b]  <- aux_ll$bern
    ll_gamma[b] <- aux_ll$gamma
    
    # Almacenar resultados después de burn-in y thinning:
    if (b>n_burn && b%%n_thin==1) {
      samples[row, ] <- c(alpha, beta, omega2, psi_d, sigma2, mu_j, lambda_j[2:4],
                          phi_j, delta_0j, delta_1j, R_i, ll_bern[b], ll_gamma[b],
                          ll_total[b])
      row <- row+1
    }
    
    # Progreso:
    ncat <- floor(B/10)
    if (b%%ncat==0) {
      cat(100*round(b/B, 1), "% completado... \n", sep = "")
    }
  }
  
  # Salida final:
  colnames(samples) <- c("alpha", paste0("beta[", 1:4, "]"), "omega2",
                         paste0("psi[", 1:33, "]"), "sigma2", paste0("mu[", 1:4, "]"),
                         paste0("lambda[", 2:4, "]"), paste0("phi[", 1:4, "]"),
                         paste0("delta0[", 1:4, "]"), paste0("delta1[", 1:4, "]"),
                         paste0("R[", 1:n, "]"), "ll_bern", "ll_gamma", "ll")
  samples <- as.data.frame(samples)
  nombre_archivo <- paste0("Chain 3 (Arévalo-García) ", format(Sys.time(), "%d.%m.%Y %H.%M"), ".txt")
  write.table(samples, file = nombre_archivo, sep = ";", dec = ".",
              row.names = FALSE, quote = TRUE)
  message("Archivo exportado como: ", normalizePath(nombre_archivo))
  message("Tasa de aceptación de las simulaciones de mu: ",
          paste(round(ar_mu, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de lambda: ",
          paste(round(ar_lambda[2:4], 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de phi: ",
          paste(round(ar_phi, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de delta0: ",
          paste(round(ar_delta0, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de delta1: ",
          paste(round(ar_delta1, 3), collapse = ", "))
  message("Tasa de aceptación de las simulaciones de R: mínima = ",
          round(min(ar_R), 3), ", media = ", round(mean(ar_R), 3), ", máxima = ",
          round(max(ar_R), 3))
}

# Ajuste del Modelo 3:
set.seed(1207); tic(); MCMC_3(B, Y, n, X, a_alpha, b_alpha, beta_0, B_0, a_sigma, b_sigma,
                              a_omega, b_omega, a_mu, b_mu, a_lambda, b_lambda, a_phi,
                              b_phi, a_delta0, b_delta0, b_delta1); toc(); rm(samples)

#==============================================================================
# PARTE IV: Diagnóstico de los resultados de los algoritmos
#==============================================================================

# AVISO: Para esta parte, puede cargar los archivos generados por los
# muestreadores de Gibbs en su ordenador, o trabajar por defecto con los que ya
# obtuvimos los autores (que aparece en la carpeta 'Data' en GitHub), fechados
# en 10, 12 y 13 de julio para los muestreadores 1, 2 y 3 respectivamente. 

library(coda)

# Gráficos de la log-verosimilitud:
Chain1 <- fread("Chain 1 (Arévalo-García) 10.07.2026 07.03.txt", sep = ";", dec = ".")
plot(Chain1$ll, type = "p", pch = 1, cex = .5, col = col[2],
     xlab = "Iteración", ylab = "Log-verosimilitud",
     main = "MLR-LAFT", ylim = c(-150, 125))
abline(h = mean(Chain1$ll), lwd = 3, col = col[2])

Chain2 <- fread("Chain 2 (Arévalo-García) 12.07.2026 20.21.txt", sep = ";", dec = ".")
plot(Chain2$ll, type = "p", pch = 1, cex = .5, col = col[2],
     xlab = "Iteración", ylab = "Log-verosimilitud",
     main = "MLR-LAFT-ED", ylim = c(-150, 125))
abline(h = mean(Chain2$ll), lwd = 3, col = col[2])

Chain3 <- fread("Chain 3 (Arévalo-García) 13.07.2026 01.13.txt", sep = ";", dec = ".")
plot(Chain3$ll, type = "p", pch = 1, cex = .5, col = col[2],
     xlab = "Iteración", ylab = "Log-verosimilitud",
     main = "MLR-LAFT-EED", ylim = c(-150, 125))
abline(h = mean(Chain3$ll), lwd = 3, col = col[2])

# Tamaños efectivos de muestra de parámetros únicos:
round(effectiveSize(Chain1[ , c(1:25)]), 0) #Modelo 1
round(effectiveSize(Chain2[ , c(1:6, 40:59)]), 0) #Modelo 2
round(effectiveSize(Chain3[ , c(1:6, 40:59)]), 0) #Modelo 3

# Tamaños efectivos de muestra de parámetros por familias:
ESS_Ri_M1 <- effectiveSize(Chain1[ , c(26:1147)]) #R_i del Modelo 1
summary(round(ESS_Ri_M1, 0)) 
ESS_Ri_M2 <- effectiveSize(Chain2[ , c(60:1181)]) #R_i del Modelo 2
summary(round(ESS_Ri_M2, 0)) 
summary(round(effectiveSize(Chain2[ , c(7:39)]), 0)) #u_d del Modelo 2
ESS_Ri_M3 <- effectiveSize(Chain3[ , c(60:1181)]) #R_i del Modelo 3
summary(round(ESS_Ri_M3, 0)) 
summary(round(effectiveSize(Chain3[ , c(7:39)]), 0)) #psi_d del Modelo 3

## Autocorrelaciones de los parámetros con ESS máximos:
Ri_max <- which.max(ESS_Ri_M1); names(ESS_Ri_M1)[Ri_max]
acf(Chain1[, ..Ri_max][[1]], lag.max = 50, main = expression(R[427]))

# Criterio de información de la devianza (DIC):

## Para el Modelo 1:

### Medias posteriores:
alpha_hat_1   <- mean(as.matrix(Chain1[ , "alpha"]))
beta_cols     <- paste0("beta[", 1:4, "]")
beta_hat_1    <- colMeans(as.matrix(Chain1[ , ..beta_cols]))
sigma2_hat_1  <- mean(as.matrix(Chain1[ , "sigma2"]))
mu_cols       <- paste0("mu[", 1:4, "]")
mu_hat_1      <- colMeans(as.matrix(Chain1[ , ..mu_cols]))
lambda_cols   <- paste0("lambda[", 2:4, "]")
lambda_hat_1  <- c(1, colMeans(as.matrix(Chain1[, ..lambda_cols])))
phi_cols      <- paste0("phi[", 1:4, "]")
phi_hat_1     <- colMeans(as.matrix(Chain1[ , ..phi_cols]))
delta0_cols   <- paste0("delta0[", 1:4, "]")
delta0_hat_1  <- colMeans(as.matrix(Chain1[ , ..delta0_cols]))
delta1_cols   <- paste0("delta1[", 1:4, "]")
delta1_hat_1  <- colMeans(as.matrix(Chain1[ , ..delta1_cols]))
R_cols        <- paste0("R[", 1:n, "]")
R_hat_1       <- colMeans(as.matrix(Chain1[ , ..R_cols]))

### Media aritmérica de la log-verosimilitud:
mean_LL_1 <- mean(as.matrix(Chain1[ , "ll"])) #l_barra

### Log-verosimilitud en las medias posteriores:
lpy_1 <- ll_ZIGamma(Y, R_hat_1, mu_hat_1, lambda_hat_1, phi_hat_1, delta0_hat_1,
                  delta1_hat_1)$total; lpy_1

### Cálculo del DIC:
pD  <- 2*(lpy_1-mean_LL_1); pD #número efectivo de parámetros
DIC <- -2*lpy_1+2*pD; DIC

## Para el Modelo 2:

### Medias posteriores:
alpha_hat_2   <- mean(as.matrix(Chain2[ , "alpha"]))
beta_hat_2    <- colMeans(as.matrix(Chain2[ , ..beta_cols]))
omega2_hat_2  <- mean(as.matrix(Chain2[ , "omega2"]))
u_cols        <- paste0("u[", 1:33, "]")
u_hat_2       <- colMeans(as.matrix(Chain2[ , ..u_cols]))
sigma2_hat_2  <- mean(as.matrix(Chain2[ , "sigma2"]))
mu_hat_2      <- colMeans(as.matrix(Chain2[ , ..mu_cols]))
lambda_hat_2  <- c(1, colMeans(as.matrix(Chain2[, ..lambda_cols])))
phi_hat_2     <- colMeans(as.matrix(Chain2[ , ..phi_cols]))
delta0_hat_2  <- colMeans(as.matrix(Chain2[ , ..delta0_cols]))
delta1_hat_2  <- colMeans(as.matrix(Chain2[ , ..delta1_cols]))
R_hat_2       <- colMeans(as.matrix(Chain2[ , ..R_cols]))

### Media aritmérica de la log-verosimilitud:
mean_LL_2 <- mean(as.matrix(Chain2[ , "ll"])) #l_barra

### Log-verosimilitud en las medias posteriores:
lpy_2 <- ll_ZIGamma(Y, R_hat_2, mu_hat_2, lambda_hat_2, phi_hat_2, delta0_hat_2,
                  delta1_hat_2)$total; lpy_2

### Cálculo del DIC:
pD  <- 2*(lpy_2-mean_LL_2); pD #número efectivo de parámetros
DIC <- -2*lpy_2+2*pD; DIC

## Para el Modelo 3:

### Medias posteriores:
alpha_hat_3   <- mean(as.matrix(Chain3[ , "alpha"]))
beta_hat_3    <- colMeans(as.matrix(Chain3[ , ..beta_cols]))
omega2_hat_3  <- mean(as.matrix(Chain3[ , "omega2"]))
psi_cols      <- paste0("psi[", 1:33, "]")
psi_hat_3     <- colMeans(as.matrix(Chain3[ , ..psi_cols]))
sigma2_hat_3  <- mean(as.matrix(Chain3[ , "sigma2"]))
mu_hat_3      <- colMeans(as.matrix(Chain3[ , ..mu_cols]))
lambda_hat_3  <- c(1, colMeans(as.matrix(Chain3[, ..lambda_cols])))
phi_hat_3     <- colMeans(as.matrix(Chain3[ , ..phi_cols]))
delta0_hat_3  <- colMeans(as.matrix(Chain3[ , ..delta0_cols]))
delta1_hat_3  <- colMeans(as.matrix(Chain3[ , ..delta1_cols]))
R_hat_3       <- colMeans(as.matrix(Chain3[ , ..R_cols]))

### Media aritmérica de la log-verosimilitud:
mean_LL_3 <- mean(as.matrix(Chain3[ , "ll"])) #l_barra

### Log-verosimilitud en las medias posteriores:
lpy_3 <- ll_ZIGamma(Y, R_hat_3, mu_hat_3, lambda_hat_3, phi_hat_3, delta0_hat_3,
                  delta1_hat_3)$total; lpy_3

### Cálculo del DIC:
pD  <- 2*(lpy_3-mean_LL_3); pD #número efectivo de parámetros
DIC <- -2*lpy_3+2*pD; DIC

# Criterio de información de Watanabe-Akaike (WAIC):

## Función involucrada:
ll_i_ZIGamma <- function(Y, R, mu, lambda, phi, delta0, delta1){
  ll <- numeric(n)
  for(i in 1:n){
    total_i <- 0
    for(j in 1:4){
      eta  <- delta0[j]-delta1[j]*R[i]
      pi   <- plogis(eta)
      muij <- exp(mu[j]+lambda[j]*R[i])
      if(Y[i,j]==0){
        total_i <- total_i+log(pi)
      }else{
        total_i <- total_i+log(1-pi)+dgamma(Y[i,j], phi[j], rate = phi[j]/muij, log=TRUE)
      }
    }
    ll[i] <- total_i
  }
  ll
}

## Para el Modelo 1:

### Matriz de almacenamiento:
S  <- nrow(Chain1)
LL <- matrix(NA, n, S)
ncat <- floor(S/10) #progreso

### Algoritmo:
tic(); for(s in 1:S){
  # Extraer actualizaciones de la muestra s:
  mu_s      <- as.numeric(Chain1[s, ..mu_cols]) 
  lambda_s  <- c(1, as.numeric(Chain1[s, ..lambda_cols]))
  phi_s     <- as.numeric(Chain1[s, ..phi_cols])
  delta0_s  <- as.numeric(Chain1[s, ..delta0_cols])
  delta1_s  <- as.numeric(Chain1[s, ..delta1_cols])
  R_s       <- as.numeric(Chain1[s, ..R_cols])
  # Log-verosimilitud por municipio:
  LL[, s] <- ll_i_ZIGamma(Y, R_s, mu_s, lambda_s, phi_s, delta0_s, delta1_s)
  ## Progreso:
  if(s %% ncat == 0){
    cat(100*round(s/S, 1), "% completado...\n", sep = "")
  }
}; toc()
lppd <- sum(log(rowMeans(exp(LL)))); lppd #densidad predictiva puntual logarítmica
pWAIC <- 2*sum(log(rowMeans(exp(LL)))-rowMeans(LL)); pWAIC #número efectivo de parámetros
WAIC <- -2*lppd+2*pWAIC; WAIC
rm(LL)

## Para el Modelo 2:

### Matriz de almacenamiento:
S  <- nrow(Chain2)
LL <- matrix(NA, n, S)
ncat <- floor(S/10) #progreso

### Algoritmo:
tic(); for(s in 1:S){
  # Extraer actualizaciones de la muestra s:
  mu_s      <- as.numeric(Chain2[s, ..mu_cols]) 
  lambda_s  <- c(1, as.numeric(Chain2[s, ..lambda_cols]))
  phi_s     <- as.numeric(Chain2[s, ..phi_cols])
  delta0_s  <- as.numeric(Chain2[s, ..delta0_cols])
  delta1_s  <- as.numeric(Chain2[s, ..delta1_cols])
  R_s       <- as.numeric(Chain2[s, ..R_cols])
  # Log-verosimilitud por municipio:
  LL[, s] <- ll_i_ZIGamma(Y, R_s, mu_s, lambda_s, phi_s, delta0_s, delta1_s)
  ## Progreso:
  if(s %% ncat == 0){
    cat(100*round(s/S, 1), "% completado...\n", sep = "")
  }
}; toc()
lppd <- sum(log(rowMeans(exp(LL)))); lppd #densidad predictiva puntual logarítmica
pWAIC <- 2*sum(log(rowMeans(exp(LL)))-rowMeans(LL)); pWAIC #número efectivo de parámetros
WAIC <- -2*lppd+2*pWAIC; WAIC
rm(LL)

## Para el Modelo 3:

### Matriz de almacenamiento:
S  <- nrow(Chain3)
LL <- matrix(NA, n, S)
ncat <- floor(S/10) #progreso

### Algoritmo:
tic(); for(s in 1:S){
  # Extraer actualizaciones de la muestra s:
  mu_s      <- as.numeric(Chain3[s, ..mu_cols]) 
  lambda_s  <- c(1, as.numeric(Chain3[s, ..lambda_cols]))
  phi_s     <- as.numeric(Chain3[s, ..phi_cols])
  delta0_s  <- as.numeric(Chain3[s, ..delta0_cols])
  delta1_s  <- as.numeric(Chain3[s, ..delta1_cols])
  R_s       <- as.numeric(Chain3[s, ..R_cols])
  # Log-verosimilitud por municipio:
  LL[, s] <- ll_i_ZIGamma(Y, R_s, mu_s, lambda_s, phi_s, delta0_s, delta1_s)
  ## Progreso:
  if(s %% ncat == 0){
    cat(100*round(s/S, 1), "% completado...\n", sep = "")
  }
}; toc()
lppd <- sum(log(rowMeans(exp(LL)))); lppd #densidad predictiva puntual logarítmica
pWAIC <- 2*sum(log(rowMeans(exp(LL)))-rowMeans(LL)); pWAIC #número efectivo de parámetros
WAIC <- -2*lppd+2*pWAIC; WAIC
rm(LL)

# Posterior predictive check (con el Modelo 1):

## Gráfico mixto (masa-densidad):
PPC_ZIGamma <- function(Y, R_hat, mu_hat, lambda_hat, phi_hat, delta0_hat,
                        delta1_hat, indicador = 1, breaks = 30, ylim = NULL, main = NULL){
  y <- Y[, indicador]
  y_pos <- y[y > 0]
  n <- length(y)
  mu_i <- exp(mu_hat[indicador]+lambda_hat[indicador]*R_hat)
  pi_i <- plogis(delta0_hat[indicador]-delta1_hat[indicador]*R_hat)
  x <- seq(min(y_pos), max(y), length = 500)
  dens <- numeric(length(x))
  for(i in 1:n){
    dens <- dens+(1-pi_i[i])*dgamma(x, phi_hat[indicador],
                                    rate = phi_hat[indicador]/mu_i[i])
  }
  dens <- dens/n
  p0_obs <- mean(y==0)
  p0_pred <- mean(pi_i)
  h <- hist(y_pos, breaks = breaks, plot = FALSE)
  dx <- min(diff(h$breaks))
  if(is.null(ylim)){
    ylim <- c(0, min(4, 1.05*max(h$density)))
  }
  col.obs <- adjustcolor("steelblue", alpha.f=.45)
  col.pred <- adjustcolor("firebrick3", alpha.f=.90)
  plot(NA, xlim = c(-dx,max(x)), ylim = ylim, xaxt = "n",
       xlab = paste("Indicador", indicador), ylab = "Densidad",
       main = main)
  hist(y_pos, breaks = breaks, freq = FALSE, add = TRUE, col = col.obs, border = "white")
  lines(x, dens, lwd = 3, col = col.pred)
  abline(v = 0, lty = 2, col = "grey60")
  ancho <- 0.25*dx
  rect(-0.65*dx, 0, -0.65*dx+ancho, p0_obs, col = col.obs, border = NA)
  rect(-0.30*dx, 0, -0.30*dx+ancho, p0_pred, col = col.pred, border = NA)
  axis(1, at = c(-0.5*dx, pretty(x)), labels = c("0", pretty(x)))
  legend("topright", legend = c("Datos observados", "Modelo ZIGamma"),
         fill = c(col.obs, col.pred), bty = "n")
  invisible(list(x = x, dens = dens, p0_obs = p0_obs, p0_pred = p0_pred,
    mu_i = mu_i, pi_i = pi_i))
}
PPC_ZIGamma(Y, R_hat_1, mu_hat_1, lambda_hat_1, phi_hat_1, delta0_hat_1,
            delta1_hat_1, indicador = 1, main = "Terrorismo")
PPC_ZIGamma(Y, R_hat_1, mu_hat_1, lambda_hat_1, phi_hat_1, delta0_hat_1,
            delta1_hat_1, indicador = 2, main = "Conflicto")
PPC_ZIGamma(Y, R_hat_1, mu_hat_1, lambda_hat_1, phi_hat_1, delta0_hat_1,
            delta1_hat_1, indicador = 3, main = "Criminalidad")
PPC_ZIGamma(Y, R_hat_1, mu_hat_1, lambda_hat_1, phi_hat_1, delta0_hat_1,
            delta1_hat_1, indicador = 4, main = "Narcotráfico")

# Estimaciones:

## Función para predicción:
Pred_ZIGamma <- function(Chain, n){
  S <- nrow(Chain)
  Yhat <- matrix(0, n, 4)
  ncat <- max(1, floor(S/10)) #progreso
  for(s in 1:S){
    mu <- as.numeric(Chain[s, ..mu_cols])
    lambda <- c(1, as.numeric(Chain[s, ..lambda_cols]))
    phi <- as.numeric(Chain[s, ..phi_cols])
    delta0 <- as.numeric(Chain[s, ..delta0_cols])
    delta1 <- as.numeric(Chain[s, ..delta1_cols])
    R <- as.numeric(Chain[s, ..R_cols])
    for(j in 1:4){
      eta <- delta0[j]-delta1[j]*R #predictor lineal parte Bernoulli
      pi <- plogis(eta)
      muij <- exp(mu[j]+lambda[j]*R) #predictor lineal parte Gamma
      z <- rbinom(n, 1, 1-pi) #función indicadora z_ij
      ypred <- z*rgamma(n, phi[j], rate = phi[j]/muij) #densidad ZIGamma
      Yhat[,j] <- Yhat[,j] + ypred #almacenamiento
    }
    if(s%%ncat == 0){cat(100*round(s/S,1), "% completado...\n", sep="")}
  }
  Yhat <- Yhat/S #media predictiva posterior
  return(Yhat)
}

## Función para las métricas de desempeño:
Metricas <- function(Y, Yhat){
  mse <- apply((Y-Yhat)^2, 2, mean)
  mae <- apply(abs(Y-Yhat), 2, mean)
  r2 <- numeric(4)
  for(j in 1:4){
    SSE <- sum((Y[,j]-Yhat[,j])^2)
    SST <- sum((Y[,j]-mean(Y[,j]))^2)
    r2[j] <- 1-SSE/SST
  }
  out <- rbind(MSE = mse, MAE = mae, R2 = r2)
  colnames(out) <- paste0("y_",1:4)
  return(out)
}

## Para el Modelo 1:
set.seed(2407); Yhat_1 <- Pred_ZIGamma(Chain1, n)
Metricas(Y, Yhat1)

## Para el Modelo 2:
set.seed(2407); Yhat2 <- Pred_ZIGamma(Chain2, n)
Metricas(Y, Yhat2)

## Para el Modelo 3:
set.seed(2407); Yhat3 <- Pred_ZIGamma(Chain3, n)
Metricas(Y, Yhat3)

# Inferencia:

## Función auxiliar:
Inferencia <- function(x, level = 0.95){
  if(is.vector(x))
    x <- matrix(x, ncol = 1)
  alpha <- (1-level)/2
  Media <- colMeans(x)
  LL    <- apply(x, 2, quantile, alpha)
  UL    <- apply(x, 2, quantile, 1-alpha)
  SD    <- apply(x, 2, sd)
  CV    <- SD/abs(Media)
  salida <- data.frame(Media = Media, LL = LL, UL = UL, CV = CV, row.names = colnames(x))
  return(salida)
}

## Sobre alpha:
alpha_post <- as.matrix(Chain3[, "alpha"])
Inferencia(alpha_post)

## Sobre beta_j:
beta_post <- as.matrix(Chain3[, ..beta_cols])
Inferencia(beta_post)

## Sobre sigma2:
sigma2_post <- as.matrix(Chain3[, "sigma2"])
Inferencia(sigma2_post)

## Sobre mu_j:
mu_post <- as.matrix(Chain3[, ..mu_cols])
Inferencia(mu_post)

## Sobre lambda_j:
lambda_post <- as.matrix(Chain3[, ..lambda_cols])
Inferencia(lambda_post)

## Sobre phi_j:
phi_post <- as.matrix(Chain3[, ..phi_cols])
Inferencia(phi_post)

## Sobre psi_d:
psi_hat_3 #medias posteriores de los 33 psi_d
Means_psi <- data.frame(DPTO_CCDGO = dep,
                        means_psi = psi_hat_3)
map_psi <- left_join(depto_shape, Means_psi, by = "DPTO_CCDGO")
ggplot(map_psi) +
  geom_sf(aes(fill = means_psi), color = "grey70") +
  scale_fill_viridis_c(option = "plasma", name = expression(psi[d]),
                       guide = guide_colorbar(barheight = unit(8, "cm"),
                                              barwidth = unit(1, "cm"))) +
  labs(title = "Efectos espaciales departamentales") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13))

# Clasificación:

## Usando los zeta_d como medidas agregadas de R_i:
R_cols <- paste0("R[", 1:n, "]")
R_post <- as.matrix(Chain3[, ..R_cols])
S <- nrow(R_post)
zeta_d <- tapply(R_hat_3, index, mean)
zeta_post <- matrix(NA, S, 33) #medias posteriores de los 1122 R_i
for(d in 1:33){
  ind <- which(index==d)
  zeta_post[ , d] <- rowMeans(R_post[ , ind, drop = FALSE])
}
zeta_hat <- colMeans(zeta_post) 
zeta_L <- apply(zeta_post, 2, quantile, 0.025)
zeta_U <- apply(zeta_post, 2, quantile, 0.975)
ranking <- data.frame(
  Departamento = deptos,
  Media = zeta_hat,
  LI = zeta_L,
  LS = zeta_U
)
ranking <- ranking[order(ranking$Media), ]
ranking$Departamento <- factor(ranking$Departamento, levels = ranking$Departamento)
ggplot(ranking, aes(x = Media, y = Departamento)) +
  geom_errorbarh(aes(xmin = LI, xmax = LS), height = 0.2, linewidth = 0.8,
                 colour = "grey30") +
  geom_point(size = 2.8, colour = "steelblue") +
  labs(title = expression("Ranking departamental según "*hat(zeta)[d]),
       x = expression(hat(zeta)[d]), y = NULL) +
  theme_bw(base_size = 13) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50")

## Usando terciles para zeta_d:
q_dep <- quantile(zeta_hat, probs = c(1/3, 2/3))
nivel_dep <- cut(zeta_hat, breaks = c(-Inf, q_dep[1], q_dep[2], Inf),
  labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE)
clasificacion_departamentos <- data.frame(
  Departamento = deptos,
  DPTO_CCDGO = dep,
  zeta_hat = zeta_hat,
  Nivel = nivel_dep
); clasificacion_departamentos
map_zeta <- left_join(depto_shape, clasificacion_departamentos, by = "DPTO_CCDGO")
ggplot(map_zeta) +
  geom_sf(aes(fill = Nivel), color = "grey70") +
  scale_fill_manual(
    values = c("Bajo"  = viridisLite::plasma(3)[1], "Medio" = viridisLite::plasma(3)[2],
      "Alto"  = viridisLite::plasma(3)[3]),
    name = "Nivel\nLAFT"
  ) +
  labs(title = expression("Clasificación departamental según "*hat(zeta)[d])) +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
        legend.text  = element_text(size = 13))

## Usando terciles para R_i:
q_mun <- quantile(R_hat_3, probs = c(1/3, 2/3))
nivel_mun <- cut(R_hat_3, breaks = c(-Inf, q_mun[1], q_mun[2], Inf),
  labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE
)
clasificacion_municipios <- data.frame(
  DPTO_CCDGO = dep,
  MPIO_CDPMP = mun,
  R_hat = R_hat_3,
  Nivel = nivel_mun
); clasificacion_municipios
clasificacion_municipios$MPIO_CDPMP <- sprintf("%05d", clasificacion_municipios$MPIO_CDPMP) #modificar formato de códigos de municipios
map_riesgo <- left_join(mcpio_shape, clasificacion_municipios, by = "MPIO_CDPMP")
ggplot(map_riesgo) +
  geom_sf(aes(fill = Nivel), color = "grey70", linewidth = 0.05) +
  scale_fill_manual(
    values = c("Bajo"  = viridisLite::plasma(3)[1], "Medio" = viridisLite::plasma(3)[2],
      "Alto"  = viridisLite::plasma(3)[3]),
    name = "Nivel\nLAFT"
  ) +
  labs(title = "Clasificación municipal del riesgo LAFT") +
  theme_bw() +
  theme(legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 13))
