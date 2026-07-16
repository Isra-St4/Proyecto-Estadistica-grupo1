# Investigación Formativa - Grupo 1
## Materia: Probabilidad y Estadística 

Este repositorio contiene la sintaxis oficial en R desarrollada por el **Analista de Datos** para el procesamiento, depuración y modelación econométrica del proyecto de investigación de la Unidad 4.

### Título de la Investigación
> **Evolución del empleo, desempleo y subempleo según el nivel de instrucción de la población económicamente activa de la provincia de Chimborazo, período 2023–2025**

---

###  Integrantes - Grupo 1
* Sandy
* Mishell
* Stalyn
* Kleyton

---

###  Requisitos del Entorno
Para replicar el análisis y procesamiento de los microdatos de las encuestas (2023-2025), asegúrese de contar con:
* **R** (Versión 4.6.1 o superior)
* **RStudio**
* Librerías necesarias: `tidyverse`, `ggplot2`, `dplyr`, `readr`

### Contenido del Repositorio
* `limpieza_riobamba_P_ESTADÍSTICA.R`: Script principal con la carga, depuración, generación de gráficos y estimación de la Ecuación de Mincer.

###  Origen de los Datos
Para ejecutar el script de análisis, es necesario contar con las bases de datos de empleo en formato `.csv` correspondientes a los periodos 2023, 2024 y 2025. 

* **Fuente:** Instituto Nacional de Estadística y Censos (INEC) / Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU).
* **Preparación:** Asegúrese de ubicar los siguientes archivos en el mismo directorio de trabajo donde se ejecutará el script `limpieza_riobamba_P_ESTADÍSTICA.R`:
  * `Riobamba2023.csv`
  * `Riobamba2024.csv`
  * `Riobamba2025.csv`
