library(tidyverse)
library(scales)

pelis <- read_csv("data/peliculas_1977_2023.csv")
glimpse(pelis)

unique(pelis$genre)

df <- pelis |> 
  mutate(
    genre = fct_lump_n(genre, n = 8, other_level = "Otros"),
    genre = if_else(is.na(genre), "Otros", genre)
  ) |> 
  count(genre, name = "cantidad_peliculas", sort = TRUE) |> 
  mutate(
    genre = fct_reorder(genre, cantidad_peliculas),
    genre = fct_relevel(genre, "Otros"),
    color = if_else(
      genre == "Otros",
      "Otros",
      "Peliculas"
    )
  )


ggplot(
  data = df,
  mapping = aes(
    x = cantidad_peliculas,
    y = genre,
    label = cantidad_peliculas,
    fill = color
  )
) +
  geom_col() + 
  geom_text(
    size = 5,
    hjust = -0.2
  ) +
  scale_x_continuous(
    breaks = breaks_pretty(n = 10),
    expand = expansion(mult = c(0,0.1)),
    labels = comma_format()
  ) +
  scale_fill_manual(
    values = c("#686D76","#F9D689"),
    guide = "none"
  ) +
  labs(
    title = "Distribución de Películas por Género",
    subtitle = "Comparación de la cantidad de películas en diferentes géneros cinematográficos",
    x = "Cantidad de películas",
    y = "Género"
  ) +
  theme_minimal(base_size = 12) 
  


