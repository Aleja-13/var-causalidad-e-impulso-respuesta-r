# Modelos VAR, causalidad de Granger y funciones impulso-respuesta en R

Modelos de vectores autorregresivos (VAR) para analizar cómo se mueven juntas varias series: selección de rezagos, causalidad de Granger y funciones impulso-respuesta.

## Qué encontrarás aquí

| Archivo | Qué hace |
|---|---|
| `scripts/var_dow_jones_dorman.R` | VAR de dos variables con precios diarios del Dow Jones (DJI) y de DORM. |
| `scripts/var_tesla_visa.R` | VAR de dos variables con precios de TSLA y V (Visa). |
| `scripts/var_tasas.R` | VAR con tasa de referencia, desempleo e inflación. |
| `scripts/var_macro_peru.R` | Varios VAR macroeconómicos (PIB, créditos, inflación, bolsa, inversión, tipo de cambio, comercio exterior) con pruebas de estacionariedad, causalidad e impulso-respuesta. |
| `scripts/var_practica_4.R` | Práctica de VAR con tipo de cambio, inflación, PIB e índice bursátil. |

## Cómo ejecutarlo

1. Descarga o clona este repositorio y abre RStudio.
2. Fija la carpeta del repositorio como directorio de trabajo (`Session > Set Working Directory`).
3. Los scripts leen los datos desde la carpeta `datos/`. Instala los paquetes que falten con `install.packages("nombre")`.

**Paquetes usados:** `AER`, `forecast`, `ggplot2`, `readxl`, `vars`.

## Datos

Se incluyen los precios diarios (DJI, DORM, TSLA, V) que usan los dos primeros scripts. Las bases macroeconómicas de los otros tres scripts no están incluidas todavía.

## Autoría

Código y análisis de Alejandra, con fines educativos y de portafolio. Más sobre mi trabajo en analítica de datos: [alejadata.pro](https://alejadata.pro/).
