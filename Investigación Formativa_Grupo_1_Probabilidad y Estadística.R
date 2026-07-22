
# SCRIPT: ANÁLISIS DE MERCADO LABORAL EN CHIMBORAZO (2023 - 2025)
# ASIGNATURA: INVESTIGACIÓN FORMATIVA / ESTADÍSTICA


# 1. CARGA DE LIBRERÍAS 
library(readr)
library(dplyr)
library(ggplot2)
# install.packages("sjPlot") # Descomentar si no lo tiene instalado
library(sjPlot)

# 2. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 3. IMPORTACIÓN DE DATOS 
cat("Cargando bases de datos de Chimborazo...\n")
Ch_2023 <- read.csv2("Chimborazo2023.csv", stringsAsFactors = FALSE)
Ch_2024 <- read.csv2("Chimborazo2024.csv", stringsAsFactors = FALSE)
Ch_2025 <- read.csv2("Chimborazo2025.csv", stringsAsFactors = FALSE)

# 4. ASIGNACIÓN DEL PERÍODO CRONOLÓGICO
Ch_2023$Anio <- 2023
Ch_2024$Anio <- 2024
Ch_2025$Anio <- 2025

# 5. UNIFICACIÓN DE LAS MUESTRAS EN UN SOLO PANEL COMPLETO
Base_Unida <- bind_rows(Ch_2023, Ch_2024, Ch_2025)


# 6. FILTRADO DE LA PEA Y DEPURACIÓN DE VARIABLES 
df_laboral <- Base_Unida %>%
  # Paso A: Quedarnos ÚNICAMENTE con la PEA
  filter(condact %in% c(1, 2)) %>%
  
  # Paso B: Seleccionar, renombrar y CONSTRUIR la Escolaridad Total
  select(
    Anio,
    Condicion_Actividad = condact,
    Nivel_Instruccion = p10a,
    Anios_Aprobados = p10b, 
    Ingreso_Laboral = ingrl,
    Sexo = p02,
    Edad = p03,
    Area = area
  ) %>%
  
  # Paso C: Recodificar las condiciones de empleo y Calcular Escolaridad Real
  mutate(
    Condicion_Empleo = case_when(
      Condicion_Actividad == 2 ~ "Desempleo",
      Condicion_Actividad == 1 & (Ingreso_Laboral >= 460) ~ "Empleo Adecuado", 
      Condicion_Actividad == 1 & (Ingreso_Laboral < 460 & Ingreso_Laboral > 0) ~ "Subempleo",
      TRUE ~ "Otro Empleo Inadecuado"
    ),
    # Aproximación estándar ENEMDU
    Anios_Escolaridad = case_when(
      Nivel_Instruccion %in% c(1, 2, 3) ~ 0,
      Nivel_Instruccion == 4 ~ Anios_Aprobados,       # Primaria
      Nivel_Instruccion == 5 ~ Anios_Aprobados,       # Básica
      Nivel_Instruccion == 6 ~ Anios_Aprobados + 6,   # Secundaria
      Nivel_Instruccion == 7 ~ Anios_Aprobados + 10,  # Bachillerato
      Nivel_Instruccion %in% c(8, 9) ~ Anios_Aprobados + 12, # Superior
      Nivel_Instruccion == 10 ~ Anios_Aprobados + 17, # Posgrado
      TRUE ~ 0
    )
  ) %>%
  
  # Paso D: Convertir variables categóricas en Factores
  mutate(
    Sexo = factor(Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    Area = factor(Area, levels = c(1, 2), labels = c("Urbana", "Rural")),
    Nivel_Instruccion_Cat = case_when(
      Nivel_Instruccion %in% c(1, 2, 3, 4, 5) ~ "Educación Básica",
      Nivel_Instruccion %in% c(6, 7)          ~ "Bachillerato",
      Nivel_Instruccion %in% c(8, 9, 10)     ~ "Educación Superior",
      TRUE ~ "Sin Instrucción"
    ),
    Nivel_Instruccion_Cat = factor(Nivel_Instruccion_Cat, 
                                   levels = c("Sin Instrucción", "Educación Básica", "Bachillerato", "Educación Superior"))
  )

#  EJECUCIÓN DEL ANÁLISIS ESTADÍSTICO 


### ANÁLISIS DESCRIPTIVO 
cat("\n--- DISTRIBUCIÓN DE FRECUENCIAS: NIVEL DE INSTRUCCIÓN vs CONDICIÓN DE EMPLEO ---\n")
tabla_contingencia <- table(df_laboral$Nivel_Instruccion_Cat, df_laboral$Condicion_Empleo)
print(tabla_contingencia)

cat("\n--- PORCENTAJES RELATIVOS POR NIVEL EDUCATIVO ---\n")
print(prop.table(tabla_contingencia, 1) * 100)

cat("\n--- DESCRIPTIVOS DE INGRESO LABORAL EN LA PEA ---\n")
descriptivos_ingreso <- df_laboral %>%
  filter(Ingreso_Laboral > 0 & Ingreso_Laboral < 99999) %>%
  group_by(Anio, Nivel_Instruccion_Cat) %>%
  summarise(
    Muestra_N = n(),
    Media_Ingreso = mean(Ingreso_Laboral, na.rm = TRUE),
    Mediana_Ingreso = median(Ingreso_Laboral, na.rm = TRUE),
    Desviacion_Std = sd(Ingreso_Laboral, na.rm = TRUE),
    Coef_Variacion = (Desviacion_Std / Media_Ingreso) * 100,
    .groups = 'drop'
  )
print(descriptivos_ingreso)


###  PRUEBAS DE HIPÓTESIS INFERENCIAL
cat("\n--- PRUEBA CHI-CUADRADO DE INDEPENDENCIA (Nivel vs Empleo) ---\n")
tabla_filtrada <- table(df_laboral$Nivel_Instruccion_Cat, df_laboral$Condicion_Empleo)
tabla_filtrada <- tabla_filtrada[rowSums(tabla_filtrada) > 0, ]

prueba_chi2 <- chisq.test(tabla_filtrada)
print(prueba_chi2)


### Figura1_Estructura_Condicion_Empleo
ggplot(df_laboral,
       aes(x = Nivel_Instruccion_Cat, fill = Condicion_Empleo)) +
  geom_bar(position = "fill", width = 0.75, color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Nivel de instrucción",
    y = "Porcentaje",
    fill = "Condición de empleo"
  ) +
  theme_classic(base_size = 13)


# CAMBIO CRÍTICO: ANÁLISIS CON ESCOLARIDAD REESTRUCTURADA

cat("\n--- ANÁLISIS DE CORRELACIÓN DE SPEARMAN (Escolaridad vs Ingreso) ---\n")
df_correlacion <- df_laboral %>% 
  filter(Ingreso_Laboral > 0 & Ingreso_Laboral < 99999 & !is.na(Anios_Escolaridad))

prueba_spearman <- cor.test(df_correlacion$Anios_Escolaridad, df_correlacion$Ingreso_Laboral, method = "spearman")
print(prueba_spearman)


### FASE 7.3: MODELO ECONOMÉTRICO COMPLEMENTARIO (Mincer)
cat("\n--- REGRESIÓN LINEAL MÚLTIPLE DE MINCER ---\n")
df_mincer <- df_correlacion %>%
  mutate(
    Experiencia = Edad - Anios_Escolaridad - 6,
    Experiencia = ifelse(Experiencia < 0, 0, Experiencia),
    Experiencia_2 = Experiencia^2,
    Ln_Ingreso = log(Ingreso_Laboral)
  )

modelo_final <- lm(Ln_Ingreso ~ Anios_Escolaridad + Experiencia + Experiencia_2 + Sexo + Area, 
                   data = df_mincer)
summary(modelo_final)


# 8. GENERACIÓN DE EVIDENCIA GRÁFICA
# ==============================================================================
ggplot(df_laboral, aes(x = Nivel_Instruccion_Cat, fill = Condicion_Empleo)) +
  geom_bar(position = "fill") +
  facet_wrap(~Anio) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Evolución de la Condición de Empleo según Nivel de Instrucción",
    subtitle = "Provincia de Chimborazo, Período 2023-2025",
    x = "Nivel de Instrucción Alcanzado",
    y = "Proporción Porcentual (%)",
    fill = "Situación Laboral"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3. BOXPLOT:

df_boxplot <- df_laboral %>% 
  filter(Ingreso_Laboral > 0 & Ingreso_Laboral <= 3000)

ggplot(df_boxplot, aes(x = Nivel_Instruccion_Cat, y = Ingreso_Laboral, fill = Nivel_Instruccion_Cat)) +
  geom_boxplot(outlier.alpha = 0.1, alpha = 0.9, color = "black", size = 0.6) +
  
  scale_fill_manual(values = c(
    "Educación Básica"   = "#00cc66",  
    "Bachillerato"       = "#00b0f0",  
    "Educación Superior" = "#ff33cc"   
  )) +
  labs(
    title = "Incidencia del Nivel de Instrucción en el Ingreso Laboral",
    subtitle = "Provincia de Chimborazo, Período 2023-2025",
    x = "Nivel de Instrucción Alcanzado",
    y = "Ingreso Laboral (USD)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )


# 4. DISPERSIÓN: CORRELACIÓN DE SPEARMAN

ggplot(df_correlacion, aes(x = Anios_Escolaridad, y = Ingreso_Laboral)) +
  geom_jitter(alpha = 0.08, color = "#1a5276", width = 0.3) + 
  geom_smooth(method = "lm", color = "#e74c3c", se = FALSE, size = 1.5) +
  coord_cartesian(ylim = c(0, 2500)) +
  labs(
    title = "Dispersión y Tendencia: Escolaridad vs Ingreso",
    subtitle = "Demostración del Efecto de la Informalidad y Sector Agrícola",
    x = "Años de Escolaridad Oficiales",
    y = "Ingreso Laboral Mensual (USD)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )


# Grafico coeficiente - modelo Mincer
sjPlot::plot_model(modelo_final, 
                   type = "est",                  
                   show.values = TRUE,            
                   value.offset = 0.3,            
                   line.size = 1,                 
                   dot.size = 3,                  
                   colors = "#1a5276",            
                   vline.color = "black",         
                   title = "Gráfico de Coeficientes (Forest Plot) - Modelo de Mincer",
                   axis.labels = c(
                     "AreaRural" = "Área Geográfica (Rural)",
                     "SexoMujer" = "Género (Mujer)",
                     "Experiencia_2" = "Experiencia al Cuadrado",
                     "Experiencia" = "Experiencia Laboral",
                     "Anios_Escolaridad" = "Años de Escolaridad Oficial"
                   )) +
  labs(x = "Efecto Estimado sobre el Logaritmo del Ingreso (β)") + 
  theme_classic() +                        
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", size = 10)
  )


# DISTRIBUCION DEL INGRESO POR GENERO

ggplot(data = df_mincer, aes(x = Sexo, y = exp(Ln_Ingreso), fill = Sexo)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1, outlier.color = "gray50", width = 0.6) +
  scale_fill_manual(values = c("Hombre" = "#1a5276", "Mujer" = "#b03a2e")) +
  labs(
    title = "Distribución del Ingreso Laboral Mensual por Género en Chimborazo",
    x = "Género",
    y = "Ingreso Laboral Mensual ($ USD)"
  ) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 1500)) + 
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )


# GRAFICO MENSUAL X ZONA

ggplot(data = df_mincer, aes(x = Area, y = exp(Ln_Ingreso), fill = Area)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1, outlier.color = "gray50", width = 0.6) +
  scale_fill_manual(values = c("Urbana" = "#2e4053", "Rural" = "#d35400")) + 
  labs(
    title = "Distribución del Ingreso Laboral Mensual por Área Geográfica en Chimborazo",
    x = "Área de Residencia",
    y = "Ingreso Laboral Mensual ($ USD)"
  ) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 1500)) + 
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )
