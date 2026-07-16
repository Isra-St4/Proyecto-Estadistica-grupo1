
library(readr)
library(dplyr)
library(ggplot2)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Cargamos los archivos 
Riobamba2023 <- read.csv2("Riobamba2023.csv", stringsAsFactors = FALSE)
Riobamba2024 <- read.csv2("Riobamba2024.csv", stringsAsFactors = FALSE)
Riobamba2025 <- read.csv2("Riobamba2025.csv", stringsAsFactors = FALSE)

cat("Columnas 2023:", ncol(Riobamba2023), "\n")
cat("Columnas 2024:", ncol(Riobamba2024), "\n")
cat("Columnas 2025:", ncol(Riobamba2025), "\n")

Riobamba2023$Anio <- 2023
Riobamba2024$Anio <- 2024
Riobamba2025$Anio <- 2025

P_estadistica_ <- bind_rows(Riobamba2023, Riobamba2024, Riobamba2025)
cat("\nDimensiones de la base combinada:", dim(P_estadistica_), "\n")

View(P_estadistica_)

df_limpio <- P_estadistica_ %>%
  # Filtrar solo a los Jefes de Hogar
  filter(p04 == 1) %>%
  # Seleccionar y renombrar las variables de tu matriz metodológica
  select(
    Anio,
    Ingreso_Mensual = ingrl,
    Nivel_Instruccion = p10a,
    Anios_Escolaridad = p10b,
    Sexo = p02,
    Edad = p03,
    Sector_Laboral = p42,
    Area_Residencia = area
  ) %>%
  # Eliminar filas vacías o atípicas en ingresos y educación
  filter(!is.na(Ingreso_Mensual) & !is.na(Nivel_Instruccion)) %>%
  filter(Ingreso_Mensual > 0 & Ingreso_Mensual < 99999) %>%
  # Convertir los códigos a etiquetas de texto
  mutate(
    Sexo = factor(Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    Area_Residencia = factor(Area_Residencia, levels = c(1, 2), labels = c("Urbana", "Rural")),
    Nivel_Instruccion_Cat = factor(Nivel_Instruccion,
                                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                   labels = c("Ninguno", "Alfabetización", "Jardín",
                                              "Primaria", "Educación Básica", "Secundaria",
                                              "Educación Media", "Sup. Técnica",
                                              "Universidad", "Posgrado"))
  )


# 3. BOXPLOT

ggplot(df_limpio, aes(x = reorder(Nivel_Instruccion_Cat, Nivel_Instruccion), y = Ingreso_Mensual, fill = Nivel_Instruccion_Cat)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 3000)) +
  labs(
    title = "Incidencia del Nivel de Instrucción en el Ingreso Mensual",
    subtitle = "Jefes de Hogar - Riobamba 2023-2025",
    x = "Nivel de Instrucción",
    y = "Ingreso Mensual (USD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# -------------------------------------------------------------
# 4. CORRELACIÓN DE SPEARMAN
# -------------------------------------------------------------
correlacion <- cor.test(df_limpio$Anios_Escolaridad, df_limpio$Ingreso_Mensual,
                        method = "spearman", exact = FALSE)
print("Prueba de Correlación de Spearman:")
print(correlacion)


# 5. ECUACION DE MINCER (REGRESION MULTIPLE)

df_modelo <- df_limpio %>%
  mutate(
    Experiencia = Edad - Anios_Escolaridad - 6,
    Experiencia = ifelse(Experiencia < 0, 0, Experiencia),
    Experiencia_al_cuadrado = Experiencia^2,
    Ln_Ingreso = log(Ingreso_Mensual)
  )

modelo_mincer <- lm(Ln_Ingreso ~ Anios_Escolaridad + Experiencia + Experiencia_al_cuadrado + Sexo + Area_Residencia,
                    data = df_modelo)
summary(modelo_mincer)


 # -------------GRAFICOS------------------

# Histograma
ggplot(df_limpio, aes(x = Ingreso_Mensual)) +
  geom_histogram(binwidth = 100, fill = "#2c3e50", color = "white") +
  coord_cartesian(xlim = c(0, 4000)) +
  labs(
    title = "Distribución y Sesgo del Ingreso Mensual",
    subtitle = "Jefes de Hogar - Riobamba 2023-2025",
    x = "Ingreso Mensual (USD)",
    y = "Frecuencia (Número de Personas)"
  ) +
  theme_minimal

# Barras_genero
ggplot(df_limpio, aes(x = Sexo, fill = Sexo)) +
  geom_bar(color = "white", width = 0.6) +
  geom_text(stat = 'count', aes(label = scales::comma(..count..)), vjust = -0.5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución de Jefes de Hogar por Género",
    subtitle = "Muestra Depurada - Riobamba 2023-2025",
    x = "Género",
    y = "Cantidad de Jefes de Hogar"
  ) +
  scale_fill_manual(values = c("Hombre" = "#3498db", "Mujer" = "#e74c3c")) +
  theme_minimal() +
  theme(legend.position = "none")



#barras_area
ggplot(df_limpio, aes(x = Area_Residencia, fill = Area_Residencia)) +
  geom_bar(color = "white", width = 0.6) +
  geom_text(stat = 'count', aes(label = scales::comma(..count..)), vjust = -0.5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución de Jefes de Hogar por Área de Residencia",
    subtitle = "Muestra Depurada - Riobamba 2023-2025",
    x = "Área Geográfica",
    y = "Cantidad de Jefes de Hogar"
  ) +
  scale_fill_manual(values = c("Urbana" = "#2ecc71", "Rural" = "#f1c40f")) +
  theme_minimal() +
  theme(legend.position = "none")

#.  TABLA
resumen_ingresos <- df_limpio %>%
  group_by(Nivel_Instruccion_Cat) %>%
  summarise(
    Cantidad = n(),
    Ingreso_Promedio = round(mean(Ingreso_Mensual), 2),
    Ingreso_Mediano = round(median(Ingreso_Mensual), 2)
  ) %>%
  arrange(desc(Ingreso_Promedio))


# 1. Creamos una tabla temporal para calcular los porcentajes
df_pastel_area <- df_limpio %>%
  count(Area_Residencia) %>%
  mutate(
    Porcentaje = n / sum(n) * 100,
    Etiqueta = paste0(Area_Residencia, "\n", round(Porcentaje, 1), "%")
  )

# 2. Generamos el gráfico de pastel
ggplot(df_pastel_area, aes(x = "", y = Porcentaje, fill = Area_Residencia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Etiqueta), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            fontface = "bold", 
            size = 5) +
  labs(
    title = "Distribución Porcentual de Jefes de Hogar por Área",
    subtitle = "Muestra Depurada - Riobamba 2023-2025",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c("Urbana" = "#2ecc71", "Rural" = "#e67e22")) +
  theme_void() + # Elimina el fondo y los ejes para que quede circular limpio
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none" # Ocultamos la leyenda porque el texto ya está dentro del pastel
  )
print(resumen_ingresos)
