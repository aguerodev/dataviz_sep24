library(tidyverse)

datos <- data.frame(
  pais = rep(c("United States", "Japan", "South Korea", "OECD average", "Colombia", "Belgium", "Denmark"), each = 3),
  grupo = rep(c("61-70", "71-80", "80+"), times = 7),
  porcentaje = c(30, 10, 5, 35, 10, 0, 25, 5, 0, 15, 5, 0, 10, 2, 0, 5, 1, 0, 5, 1, 0)
) 


# si tenemos valores para x,y
# usamos geom_col
ggplot(
  data = datos,
  mapping = aes(
    x = porcentaje,
    y = pais,
    fill = grupo
  )
) + 
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.75
    ) +
  scale_fill_manual(
    values = c(
      "80+" = "black",
      "71-80" = "#C7253E",
      "61-70" = "#FFCFB3"
    )
  ) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    sec.axis = dup_axis()
  ) +
  labs(
    title = "American politicians are the\noldest in the rich world",
    subtitle = "Legislators, by age group, % of total Selected\nOECD countries, July 2024 or latest",
    caption = "Sources: IPU Parline;\nThe Economist"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    plot.background = element_rect(
      fill = "#F5F4F0",
      color = "#F5F4F0"
    ),
    plot.title = element_text(
      hjust = 0,
      face = "bold",
      size = 20,
      family = "Arial Black"
    ),
    plot.subtitle = element_text(
      hjust = 0,
      size = 16,
      family = "Arial"
    ),
    plot.caption  = element_text(
      hjust = 0,
      size = 14,
      family = "Arial",
      color = "#705C53"
    )
  )
  
