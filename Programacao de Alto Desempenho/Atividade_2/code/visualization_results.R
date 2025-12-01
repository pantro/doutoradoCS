setwd("~/Descargas/codigo/")

# Instalar paquetes si es necesario
# install.packages(c("ggplot2", "dplyr", "readr", "scales"))

library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# Cargar CSV (ajusta la ruta si es necesario)
df <- read_csv("measurements.csv")

# Convertir a factores ordenados para controlar el orden en los gráficos
df$mode <- factor(df$mode, levels = c("blas", "strassen", "block", "ijk", "ikj", "jik", "jki", "kij", "kji"))

# Tiempo de ejecucion por algoritmo y tamaño
ggplot(df, aes(x = as.factor(N), y = real_time_s, color = mode, group = mode)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +  # linewidth en lugar de size
  stat_summary(fun = mean, geom = "point", size = 3) +
  scale_y_log10(
    labels = label_number(scale_cut = cut_si("s"))  # reemplazo moderno de label_number_si()
  ) +
  labs(
    title = "Execution Time by Algorithm and Matrix Size",
    x = "Matrix Size (N)",
    y = "Execution Time (s, log scale)",
    color = "Algorithm"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# Ciclos vs Instrucciones (eficiencia)
ggplot(df, aes(x = instructions, y = cycles, color = mode)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_x_log10(labels = label_number(scale_cut = cut_si("s"))) +
  scale_y_log10(labels = label_number(scale_cut = cut_si("s"))) +
  labs(
    title = "Cycles vs Instructions per Algorithm",
    x = "Instructions",
    y = "CPU Cycles",
    color = "Algorithm"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Cache misses (rendimiento de memoria)
# - L1
ggplot(df, aes(x = as.factor(N), y = L1_dcache_load_misses, fill = mode)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_y_log10(labels = label_number(scale_cut = cut_si("s"))) +
  labs(
    title = "Average L1 Cache Misses by Algorithm",
    x = "Matrix Size (N)",
    y = "L1 Cache Misses (log scale)",
    fill = "Algorithm"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )
# - LLC cache
ggplot(df, aes(x = as.factor(N), y = LLC_load_misses, fill = mode)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_y_log10(labels = label_number(scale_cut = cut_si("s"))) +
  labs(
    title = "Average LLC Cache Misses by Algorithm",
    x = "Matrix Size (N)",
    y = "LLC Cache Misses (log scale)",
    fill = "Algorithm"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )


