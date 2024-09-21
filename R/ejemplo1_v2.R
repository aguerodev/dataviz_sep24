library(tidyverse)
library(scales)

pelis <- read_csv("data/peliculas_1977_2023.csv")
glimpse(pelis)


df <- count(pelis, genre, sort = TRUE)

# preparación de los datos
# 
df <- pelis |> 
  mutate(
    genre = fct_lump_n(genre, n = 6, other_level = "Otros")
  ) |> 
  count(genre, sort = TRUE) |> 
  mutate(
    genre = fct_reorder(genre, n),
    etiqueta = comma(n),
    grupo = if_else(genre == "Otros", "#3C3D37","#6A9AB0")
  ) |> 
  drop_na()


# generar gráfico
# 
ggplot(
  data = df,
  mapping = aes(
    y = genre,
    x = n,
    label = etiqueta,
    fill = grupo
  )
) +
  geom_col() +
  geom_text(
    hjust = 1.2,
    size = 16/.pt,
    color = "white",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(
    labels = comma_format()
  ) +
  scale_fill_identity() +
  labs(
    title = "Distribución de Películas por Género",
    subtitle = "Comparación de la cantidad de películas en diferentes géneros cinematográficos",
    x = "Cantidad de películas",
    y = "Género"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 18,
      face = "bold",
      color = "#A04747",
      family = "Open Sans"
    ),
    panel.grid = element_blank()
  )
