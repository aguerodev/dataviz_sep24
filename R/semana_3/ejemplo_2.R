library(tidyverse)
library(ggrepel)

datos <- data.frame(
  Country = c("Brazil", "France", "Italy", "Greece", "Japan", "Poland", "Turkey", "Mexico", "South Korea", "United States", "OECD average"),
  Population_65_and_over = c(9, 20, 25, 22, 30, 16, 8, 7, 16, 20, 18),
  Pension_spending_GDP = c(14, 15, 17, 16, 10, 12, 5, 4, 6, 8, 11)
)

ggplot(
  data = datos,
  mapping = aes(
    x = Population_65_and_over,
    y = Pension_spending_GDP,
    label = Country
  )
) + 
  geom_text_repel(
    box.padding = 0.5
  ) +
  geom_point(
    size = 5,
    fill = "#00CCDD",
    color = "black",
    shape = 21) +
  scale_y_continuous(
    sec.axis = dup_axis()
  ) +
  labs(
    x = "%Population 65 and over",
    y = "Goverment spending on\npension benefits\n%GDP",
    title = "Titulo",
    subtitle = "Subtitulo"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y.left = element_blank(),
    axis.title.y.left = element_blank(),
    axis.title.y.right = element_text(
      angle = 0,
      hjust = 1,
      vjust = 1.15,
      margin = margin(l = -140),
      size = 14
    ),
    axis.title.x = element_text(
      size = 14
    ),
    plot.title = element_text(
      margin = margin(b = 5)
    )
  )
