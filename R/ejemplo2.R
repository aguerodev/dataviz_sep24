library(tidyverse)
library(sf)
library(scales)

datos <- read_sf("data/cantones_ips_cr.geojson")

glimpse(datos)

df <- datos |> 
  select(
    tasa_de_homicidios,
    matriculacion_en_educacion_secundaria,
    poblacion_total,
    vivienda
  )
df

ggplot(
  data = df,
  mapping = aes(
    x = tasa_de_homicidios,
    y = matriculacion_en_educacion_secundaria,
    size = poblacion_total,
    color = vivienda
  )
) +
  scale_size_continuous(
    breaks = pretty_breaks(n = 5),
    labels = comma_format(),
    range = c(1, 12)
  ) +
  scale_color_binned(
    type = "viridis"
  ) +
  geom_point() +
  theme_minimal()

## mapa
## 
## 

ggplot(
  data = datos,
  mapping = aes(
    fill = poblacion_total
  )
) +
  geom_sf(
    color = "white"
  ) +
  scale_fill_continuous(
    breaks = log_breaks(),
    label = comma_format(),
    type = "viridis",
    trans = "log10"
  ) +
  theme_minimal()

  
  
  
  
  
  
  
